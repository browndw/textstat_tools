library(quanteda)
library(spacyr)
library(tidyverse)

# For this script we're going to learn how to use part-of-speech tagging in R.
# This is a little tricky as it required a Python installation,
# and an installation of the Python package spacy.
# The spacyr package is just a wrapper for Python.

# Once you have everything set up, you need to load the packages
# and initialize the spacy model.
# You can substitute other models including en_core_web_sm and
# en_core_web_lg. For these consult:
# https://spacy.io/usage

# You may or may not need to include the python_executable argument.
spacy_initialize(model = "en")

# If it can't find a Python executable, you'll need to add the
# python_executable argument. And you'll need to point to
# the virtual environment in which you've installed spacy.
# On a Mac it will look something like:
# python_executable = "/Users/MyName/.env/bin/python"
spacy_initialize(model = "en", python_executable = ".env/bin/python")

# Load the helper functions
source("functions/helper_functions.R")

# And our keyness functions
source("functions/keyness_functions.R")

# We're going to load in our corpus using a different technique this time.
# First we'll generate a list of file names that are in the folder we want.
# Note that full.names is set to TRUE to retrieve the full paths.
# If we also wanted to retrive paths from subfolders, we'd set recursive to TRUE.
micusp_paths <- list.files("data/text_data/micusp_mini", full.names = TRUE, pattern = "*.txt")

# To save on processing, we're first going to filter out English
# and Biology papers usting str_detect()
paths_sub <- micusp_paths %>% str_detect("ENG|BIO") %>% keep(micusp_paths, .)

# Read in our files from the paths.
sub_df <- readtext_lite(paths_sub)

# And create a corpus object.
sub_corpus <- corpus(sub_df)

# Here's where the process changes from previous ones.
# We're going to use spacy to parse the corpus, rather than quanteda
sub_prsd <- spacy_parse(sub_corpus, pos = TRUE, tag = TRUE)

# Now we'll convert that into a tokens object that quanteda understands.
# Note that we can choose the contatenator. Your choice may affect
# How you split the columns later.
sub_tokens <- as.tokens(sub_prsd, include_pos = "tag", concatenator = "_")

# We can see how many tokens we have.
ntoken(sub_tokens)

# And we can view them.
sub_tokens$BIO.G0.02.1.txt[1:20]

# To get rid of some of tokens we don't want to count, we'll need to do
# Some filtering. The first of these removes anything that doesn't
# have the a letter A-Z after the underscore.
sub_tokens <- tokens_select(sub_tokens, "_[A-Z]", selection = "keep", valuetype = "regex", case_insensitive = T)

# This filters any that don't have a word or digit chacter before the underscore.
sub_tokens <- tokens_select(sub_tokens, "\\W_", selection = "remove", valuetype = "regex")

# And lastly any tokeans with a digit immediate before the underscore.
sub_tokens <- tokens_select(sub_tokens, "\\d_", selection = "remove", valuetype = "regex")

# Look at our new counts.
ntoken(sub_tokens)

# If we wanted to look at all nouns, we could do something like this.
tokens_select(sub_tokens, pattern = c("*_NN*"))

# Now lets make a dfm.
sub_dfm <- dfm(sub_tokens)

# To attach our metadata, we'll again use a differnt technique.
# Note that important metadata like the discipline is encoded into the file names.
# To see them, we can look at rownames() in our corpus object.
rownames(sub_corpus$documents)

# We can use regular expressions in the gsub() to retreive the first three
# characters in the document name. See how the gsub() function works.
?gsub

# So in the pattern below we enclose \\w{3} in paratheses so we can return
# those with \\1.
gsub("(\\w{3})\\..*?$", "\\1", rownames(sub_corpus$documents))

# Then we just pass the results to docvars()
docvars(sub_dfm, "discipline_cat") <- gsub("(\\w{3})\\..*?$", "\\1", rownames(sub_corpus$documents))

# Let's look at frequencies.
textstat_frequency(sub_dfm, n = 10)

# To generate a keyword list, we'llmake an index.
bio_index <- docvars(sub_dfm, "discipline_cat") == "BIO"

# And finally generate a keyword list.
bio_keywords <- textstat_keyness(sub_dfm, bio_index, measure = "lr")

# Let's see what we have.
head(bio_keywords, 10)

# We can add an effect size column.
bio_keywords <- bio_keywords %>% 
  mutate(effect = log_ratio(n_target, n_reference))

# Let's see what we have.
View(bio_keywords)

# We may now want to split our token from our tag at the concatenator.
# This is why the choice of the contatenator can be important.
bio_keywords <- bio_keywords %>% 
  separate(col = feature, into = c("feature", "tag"), sep = "_")

# This is where using tagging can be powerful.
# Now we can filter our keywords by various criteria.
# Say we want all singular nouns with p < 0.01:
bio_select <- bio_keywords %>% 
  filter(tag == "nn" & p < 0.01)

# Or if we want ALL nouns we can use a logical grep argument:
bio_select <- bio_keywords %>% 
  filter(grepl("nn", tag) & p < 0.01)

# Or we may want to find only those with a postive G2:
bio_select <- bio_keywords %>% 
  filter(grepl("nn", tag) & p < 0.01 & G2 > 0)

# Or all parts of speech EXCEPT nouns:
bio_select <- bio_keywords %>% 
  filter(!grepl("nn", tag) & p < 0.01 & G2 > 0)

# To end our spacy session run spacy_finalize.
spacy_finalize()
