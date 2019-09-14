library(quanteda)
library(tidyverse)
library(randomForest)
library(doParallel)

# This is just a very short script showing a procedure for
# thinking about and measure keyness in a different way.
# Here we'll take advantage of random forest's importance measures.

# We'll load our data as we usually do.
# We'll load our helper functions
source("functions/helper_functions.R")

# Read in the metadata for the entire MICUSP data.
micusp_meta <- read_csv("data/meta_data/micusp_meta.csv")

# Select our categories. In this case we just need the discipline categories.
doc_categories <- micusp_meta %>% select(discipline_cat)

# Read our texts into a data.frame.
doc_df <- readtext_lite(micusp_meta$file_path)

# Create a corpus from that data.frame.
micusp_corpus <- corpus(doc_df)

# Assign the metadata to the corpus
docvars(micusp_corpus) <- doc_categories

# Tokenize the corpus.
micusp_tokens <- tokens(micusp_corpus, include_docvars=TRUE, remove_punct = TRUE,
                        remove_numbers = TRUE, remove_symbols = TRUE, what = "word")

# Load our multi-word expressions.
multiword_expressions <- readLines("dictionaries/mwe_short.txt")

# Compound tokens based on the list.
micusp_tokens <- tokens_compound(micusp_tokens, pattern = phrase(multiword_expressions))

# And create our dfm.
micusp_dfm <- dfm(micusp_tokens)

# For the purposes of this exercise, let's again look at English and Biology.
sub_dfm <- dfm_subset(micusp_dfm, discipline_cat == "ENG" | discipline_cat == "BIO")

# Now we'll normalize the data.
sub_prop <- dfm_weight(sub_dfm, scheme = "prop")

# Convert to a data.frame.
dfm_df <- convert(sub_prop, to = "data.frame")

# Now were going to have to do some reshaping of our data to prep it
# for random forest processing.
# Here we rename the first column to text_id. Right now it's called "document",
# But there is also a token "document". To avoid confusion, we rename it. 
colnames(dfm_df)[1] <- "text_id"

# Then we save it as a new vector for use later.
text_id <- dfm_df$text_id
# And remove that first rown
dfm_df$text_id <- NULL

# The quaneda function dfm_trim() doesn't work very well on weighted dfm's.
# So were just going to filter it using base R.
# First we'll make an index of tokens with a weighted frequency > .01
freq_index <- as.vector(apply(dfm_df, 2, max) > .01)

# Then, we'll use that index to filter out the columns we want.
dfm_df <-dfm_df[, freq_index]

# Now we join our text_id to our filtered data.frame.
dfm_df <- cbind(text_id, dfm_df)
# Finally, we'll get rid of the .txt file extensions.
dfm_df$text_id <- str_replace_all(dfm_df$text_id, "\\.txt", "")

# Select the metadata columns we're going to use for classification.
sub_meta <- micusp_meta %>% select(text_id, discipline_cat) %>%
  filter(discipline_cat == "BIO" | discipline_cat == "ENG")

# Join our cateogry ids keying on text_id.
dfm_df <- left_join(sub_meta, dfm_df, by = "text_id")

# Make text_id rownames and remove that column.
dfm_df <- dfm_df %>% remove_rownames %>% column_to_rownames(var="text_id")

# Convert our categories to a factor for classification.
dfm_df$discipline_cat <- as.factor(dfm_df$discipline_cat)

# Format the matix as a data.frame for processing.
dfm_df <- data.frame(dfm_df)

# Register our cores for parallel processing. You might use 4 instead of 8
# depending on your computer.
doParallel::registerDoParallel(cores = 8)

# Do our random forest classification, making sure we stet importance to TRUE.
rf <- randomForest(discipline_cat~., data=dfm_df, ntree=1000, importance=TRUE)

# Check the result:
rf

# Save importance as a data.frame
df_rf <- as.data.frame(rf$importance)

# And order it by MeanDecreaseGini
#
# Mean Decrease in Gini is the average (mean) of a variable’s total
# decrease in node impurity, weighted by the proportion of samples
# reaching that node in each individual decision tree in the random forest.
# This is effectively a measure of how important a variable is for
# estimating the value of the target variable across all of the trees
# that make up the forest. A higher Mean Decrease in Gini indicates
# higher variable importance. Variables are sorted and displayed in the
# Variable Importance Plot created for the Random Forest by this measure.
# The most important variables to the model will be highest in the plot
# and have the largest Mean Decrease in Gini Values, conversely, the
# least important variable will be lowest in the plot, and have the
# smallest Mean Decrease in Gini values. 
df_rf <- df_rf[order(-df_rf$MeanDecreaseGini),]

# View the result
View(df_rf)

# Plot the importance
varImpPlot(rf,type=2)



