# For this lab, we'll be following many of the same procedures
# that we've done previously:
# - loading multiple texts using readtext()
# - attaching metadata to a corpus using docvars()
# - tokenizing using tokens()
# - handling multiword expressions using tokens_compound()
# - creating a document feature matrix using dfm()
# 
# For today's lab we'll begin some hypothesis testing
# using a new function:
# - textstat_keyness()
#
# Keyness is a generic term for various tests that compare
# observed vs. expected frequencies.
# The most commonly used (though not the only option) is
# called log-likelihood in corpus linguistics, but you will
# see it else where called a G-test goodness-of-fit.
# The calculation is based on a contingency table.
#
# A good explanation of its implemeation in linguistics
# can be found here: http://ucrel.lancs.ac.uk/llwizard.html
#
# Outside of quanteda, the function GTest() is included in
# the DescTools package. See here: https://rcompanion.org/rcompanion/b_04.html
#
# In addition to log-likelihood, the textstat_keyness() function
# in quanteda has other optional measures.
# See here: https://quanteda.io/reference/textstat_keyness.html
#
# We'll begin, just as we did in the distributions lab.
#
# library() loads an R library - sets of functions that we'll want to use during
# this session.

library(quanteda)
library(tidyverse)

# We'll load our helper functions
source("functions/helper_functions.R")

# And our keyness functions
source("functions/keyness_functions.R")

# Read in our metadata.
micusp_meta <- read_csv("data/meta_data/mini_meta.csv")

# Select our categories.
doc_categories <- micusp_meta %>% 
  select(text_id, discipline_cat, level_cat, student_gender, speaker_status, paper_type, paper_features)

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

# Create our dfm.
micusp_dfm <- dfm(micusp_tokens)

# Make sure everything's working.
textstat_frequency(micusp_dfm, n = 10)

# Now collect the word frequencies grouped by discipline
word_freq_discipline <- textstat_frequency(micusp_dfm, groups = "discipline_cat")

# Now we have one row per word/group combination.

# Visualizing with groups

# Often we need to filter our data down a bit in order to plot it. A bar chart
# with 7000 bars isn't very legible. So let's keep just the top 10 words
#
# filter() takes a data frame and then a series of rules based on values in that
# data frame's columns. To get only the rows where the "rank" of a word is
# greater than or equal to 10:
top_word_freqs <- word_freq_discipline %>% 
  filter(rank <= 10)

# filter() statements can be as complex as you need them to be. Keep only the
# top 10 words in "English":
top_english_freqs <- word_freq_discipline %>% 
  filter(rank <= 10 & group == "English") # Note that we use == to mean "is this equal?"

# Now let's do some hypothesis testing.
# There are a variety of ways of subsetting a dfm,
# but here we'll use the dfm_subset() function.
# First, we'll subset out a dfm of English & Biology texts.
sub_dfm <- dfm_subset(micusp_dfm, discipline_cat == "ENG" | discipline_cat == "BIO")

# When we subset out a dfm, note that ALL of the feature columns are
# returned including those that may not occur in either Biology or English.
# So we'll trim our dfm and include only those features that occur at least once.

sub_dfm <- dfm_trim(sub_dfm, min_termfreq = 1)
# Next, let's see how we can specify what categorical variables
# that we want to compare.
# For that, we create an index for our target subcorpus.
bio_index <- docvars(sub_dfm, "discipline_cat") == "BIO"

# This creates a "logical" or "boolean" vector of TRUE/FALSE values. R often
# uses these kinds of indices when you are trying to tell it to look at a subset
# of some data
bio_index

# When we use it with textstat_keyness we are indicating that we want the papers
# with discipline_cat equal to "BIO" to be our target corpus. The "ENG" papers
# will be the reference corpus since we subsetted them out of the larger dfm
# along with the "BIO" papers.
#
# The specific method we're using is log-likelihood, which is designated by
# "lr". Thus keyness will show the tokens that are more frequent in 
# papers written for Biology vs. those written for Engllish.
bio_keywords <- textstat_keyness(sub_dfm, bio_index, measure = "lr")

# Let's look at the top ten keywords.
head(bio_keywords, 10)

# Data frame includes columns for n_target and n_reference.
# And these vectors of numbers are what we pass into the log_ratio() function
# to calculate effect sizes.
log_ratio(bio_keywords$n_target, bio_keywords$n_reference)[1:10]

# A common way to compute new columns for a data frame based on values in
# existing columns is to use the mutate() function:

# (We'll also use the %>% operator for readability)
bio_keywords <- bio_keywords %>% 
  # While we're inside muatate(), we don't have to repeat the data frame name or
  # use $ - mutate() knows that we're trying to refer to other columns. "effect"
  # will be the name of the new column, and its contents will be the output of
  # log_ratio()
  mutate(effect = log_ratio(n_target, n_reference)) 


# Now see that we have a new effect column
bio_keywords[1:10,]

# To reorder the data frame, we use arrange()
bio_keywords %>% 
  arrange(effect)

# To sort in descending order, wrap the variable name in desc()
bio_keywords %>% 
  arrange(desc(effect))

# What if we want to get the largest effect sizes, no matter whether positive or
# negative? We'd need to calculate the absolute value
?abs

# There are a couple of options. You could add a new columne using
# mutate() and abs() in combination.
# Alternatively, abs() can be embedded in the arrange() function.
# The latter won't change the value, it will just be used for ordering.

### YOUR CODE HERE

# We can alsu filter our keywords, to include only those with those
# below a particularly p-value threshold, for example:
bio_keywords %>% 
  filter(p <= 0.05)

# Here we can see the top ten keywords when the paper_type "Report" is the target.
type_report_index <- docvars(micusp_dfm, "paper_type") == "Report"
report_keywords <- textstat_keyness(micusp_dfm, type_report_index, measure = "lr")

report_keywords %>% 
  mutate(effect = log_ratio(n_target, n_reference)) %>% 
  arrange(desc(effect))

# Now try to examine the top 10 keywords when the paper_type is a Proposal. Dont
# forget to add the effect size column as well.

### YOUR CODE HERE

# When we calculate keyness this way, a number of questions arise.
# One relates to distribution: What is driving a particular keyness value?
# High frequency in a few texts? Or many?
# We can grapple with this by reporting out distribution statistics.
# The linguist Mike Scott has also proposed a procedure called key-key words,
# which he includes in the concordancer WordSmith:
# https://lexically.net/downloads/version4/html/index.html?database_info.htm
#
# The procedure involves iterating though each text in the target corpus
# and calcuting keyness against the reference.
# Then, we can see in what percentage of texts does keyness reach
# a particular p-value threshold.
#
# To begin, we'll subset out some data from our dfm
# for our target corpus.
bio_dfm <- dfm_subset(micusp_dfm, discipline_cat == "BIO")
# And we'll trim the dfm.
# The function will do this too to prevent unnecessary processing,
# But we'll do it here anyway as good practice.
bio_dfm <- dfm_trim(bio_dfm, min_termfreq = 1)

# Now we'll select the data for our reference corpus.
eng_dfm <- dfm_subset(micusp_dfm, discipline_cat == "ENG")
# And trim that dfm, too.
eng_dfm <- dfm_trim(eng_dfm, min_termfreq = 1)

# The function we'll use is called key_keys().
# The first argument is our target corpus and the second the reference.
# We can also specify a p-value threshold and whether or not include 
# The Yates' correction for 2x2 contingency tables:
# https://www.ncbi.nlm.nih.gov/pubmed/2362976
#
# The function defaults to p < 0.05 and to include the Yates' correction.
bio_key_keys <- key_keys(bio_dfm, eng_dfm)

# Check the output.
View(bio_key_keys)

# If you change the threshold argument, you can check to see how the range changes.
# The threshold can be set at 0.05, 0.01, 0.001, or 0.0001.

bio_key_keys <- key_keys(bio_dfm, eng_dfm, threshold = 0.001)


# This last bit just walks though some ways of more effienctly
# manipulating quanteda data structures to generate various comparisions.
#
# Here we can quickly compare English papers to the rest of MICUSP
# (or papers that are not ENG) and calculate effect sizes in one line.
english_keywords <- textstat_keyness(micusp_dfm, docvars(micusp_dfm, "discipline_cat") == "ENG", 
                                     measure = "lr") %>% 
  mutate(effect = log_ratio(n_target, n_reference))

# Same with Biology.
biology_keywords <- textstat_keyness(micusp_dfm, docvars(micusp_dfm, "discipline_cat") == "BIO", 
                                     measure = "lr") %>% 
  mutate(effect = log_ratio(n_target, n_reference)) 

# We can also combine these for comaprative purposes.
# To do this we'll need to make one table that has:
# - one row per word
# - columns for the English effect sizes and other stats
# - columns for hte Biology effect sizes and other stats

# left_join() is a function for combining two different data frames based on a
# column with shared values. Both of these data frames will have rows in their
# "feature" columns that are the same, so we will join on that column. The
# "suffix" argument will add extra names to the shared, non-joining columns like
# "effect_size" and "p":

comparison_stats <- english_keywords %>% 
  left_join(biology_keywords, by = "feature", suffix = c("_eng", "_bio"))

# Check the new column names of this table and we see that we have one "feature"
# column and then the other table columns with added suffixes
colnames(comparison_stats)



