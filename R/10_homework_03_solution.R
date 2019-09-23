
library(quanteda)
library(spacyr)
library(tidyverse)
library(janitor)

spacy_initialize(model = "en")

source("functions/keyness_functions.R")
source("functions/helper_functions.R")
source("functions/dispersion_functions.R")

# You'll have to set this path to get the file to knit.
file_paths <- list.files("data/ya_corpus", full.names = T, recursive = T, pattern = "*.txt")

# We need to subset the paths to files we want.
file_paths <- file_paths %>% str_detect(pattern="lemmatized") %>% 
  discard(file_paths, .) %>%
  str_subset("Meyer|Rowling")

# We can (and should!) check to see if this has returned what we want.
# Using the length() function, we see that there are in fact 10 paths.
length(file_paths)

# So we'll go ahead and read in the texts.
ya_df <- readtext_lite(file_paths)

# Note warning for "incomplete final line". This simply means there's no
# carriage return on the last line of these texts. We can add them, or simply
# ignore the warning.

# Create our corpus.
ya_corpus <- corpus(ya_df)

# From that, we can create a table of totals that can be included in the Report.
totals <- data.frame(tokens= ntoken(ya_corpus)) %>% 
  mutate(Author = gsub("(\\w+)_\\S+", "\\1", rownames(ya_corpus$documents))) %>%
  add_count(Author,  name = "Texts") %>%
  group_by(Author, Texts) %>%
  summarise(Tokens = sum(tokens)) %>%
  adorn_totals("row")

# Run through the tokenizing and filtering.
set.seed(1)

# Tokenize by sentence.
sent_toks <- tokens(ya_corpus, what = "sentence")

# Create and index to iterate through.
idx <- seq(1:length(sent_toks))

# Sample 500 sentences from each novel.
ya_sample <- lapply(idx, function(i) sample(unlist(sent_toks[i]), 500))

# Collapse the camples back into a single string separated by a space.
ya_sample <- lapply(idx, function(i) paste(unlist(ya_sample[i]), collapse = " "))

# Format a data frame from the sampled sentences.
ya_sample <- data.frame(text = do.call(rbind, ya_sample), stringsAsFactors = F)

# Add a column for doc_ids pulled from our corpus object.
ya_sample <- bind_cols(doc_id = rownames(ya_corpus$documents), ya_sample)

# Make a new corpus.
ya_sample <- corpus(ya_sample)

# Note here that you could also make a table of counts from the sampled sentences,
# rather than the full novels. This is sensible. If you were acutally using this
# data for a study, in your data description you would probable want to include
# BOTH sets of counts.

# As always(!) check your progress. Does our data look good?
summary(ya_sample)

# Now we can parse the corpus with part-of-speech tagging.
ya_prsd <- spacy_parse(ya_sample, pos = TRUE, tag = TRUE)

# And we can turn the result into a tokens object with the as.tokens() function.
ya_tokens <- as.tokens(ya_prsd, include_pos = "tag", concatenator = "_")

# When we part-of-speech tag a corpus, we necessarily include punctuation.
# Punctuation provides contextual information for the tagger.
# But we don't neccessarily want to include punctuation in our counts.
# Here, we filter our tokens to rermove numbers and punctuation
# using tokens_select with either "keep" or "remove".
ya_tokens <- tokens_select(ya_tokens, "_[A-Z]", selection = "keep", valuetype = "regex", case_insensitive = T)

ya_tokens <- tokens_select(ya_tokens, "\\W_", selection = "remove", valuetype = "regex")

ya_tokens <- tokens_select(ya_tokens, "\\d_", selection = "remove", valuetype = "regex")

# Now we create a dfm.
ya_dfm <- dfm(ya_tokens)

# Assign author_id from corpus object.
docvars(ya_dfm, "author_id") <- gsub("(\\w+)_\\S+$", "\\1", rownames(ya_corpus$documents))

# Before proceeding, here is a sensible place to calculate the dispersions.
# The function dispersions_all() works on a dfm, so this is simple.
ya_disp <- dispersions_all(ya_dfm)

# Move the rownames to column and name it "feature" to match keyword column name
# for later and select the disperion measure we want to report.
ya_disp <- ya_disp %>% rownames_to_column("feature") %>%
  select(feature, DP)

# Note that an alternative approach would be to use key_keys() function.
# That would also be a way to account for distributions of keywords in
# the various novels.

# Create an index for keyword (log-likelihood) comparision.
rowling_index <- docvars(ya_dfm, "author_id") == "Rowling"

# Generate a keyword list
ya_keywords <- textstat_keyness(ya_dfm, rowling_index, measure = "lr")

# Add our effect size calculations.
ya_keywords <- ya_keywords %>% 
  mutate(effect = log_ratio(n_target, n_reference))

# Add dispersions to table by joining on the "feature" column.
ya_keywords <- left_join(ya_keywords, ya_disp, by = "feature")

# Split out our tags into a new column.
ya_keywords <- ya_keywords %>% 
  separate(col = feature, into = c("feature", "tag"), sep = "_")

# Filter out the proper nouns (both singular and plural)
ya_keywords <- ya_keywords %>%
  filter(!str_detect(tag, 'nnp'))

# Check the result.
View(ya_keywords)

# Since Rowling was the original target corpus, we can just select
# the top 10 rows and round selected columns to 2 digits for reporting.
rowling_keywords <- ya_keywords %>%
  top_n(10, G2) %>% 
  mutate_at(vars(G2, effect, DP), ~round(., 2))


# For Meyer, we can grab the bottom 10 rows of our keyword table.
# But these are negative values, so we want to convert those to
# absolute values and round, again, to 2 digits.
# Finally, we swap the the positions and names of our
# target and reference counts.
meyer_keywords <- ya_keywords %>%
  top_n(-10, G2) %>%
  arrange(G2) %>%
  mutate_if(is.numeric, abs) %>% 
  mutate_at(vars(G2, effect, DP), ~round(., 2)) %>%
  select(feature:p, n_reference, n_target, effect:DP) %>%
  rename(n_target = n_reference, n_reference = n_target)

# Your response will vary for the final question. But a few things jump out to me that 
# might be interesting to discuss. One is the singular vs. plural pronouns
# in the keywords. This suggests something about the importance of group
# identity in the Harry Potter novels vs. the Twlight novels.
# However, this pattern is also clearly driven by the different points
# of view in the two series.

# When the Twilight novels are the target corpus, 
# there are also a fair number of tokens related to looking,
# watching, and being watched. This suggests something about how Bella 
# imagines herself throughout these novels as the subject of her peers'
# gaze, and especially Edward's.

# But there are, of course, many other patterns that may have caught 
# your attention.



