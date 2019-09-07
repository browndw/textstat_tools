library(quanteda)
library(tidyverse)

# The following short script introduces code for calculating collocations
# by Mutual Information. This is not a functionality that is 
# built in to quanteda. Thus it requires a custom function
# that is in collocations_functions.R

# We'll load our helper functions
source("functions/helper_functions.R")

# And our keyness functions
source("functions/collocations_functions.R")

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

# From the tokens object, we can find collocations using collocates_by_MI()
# The integers specify the window to the left and right of node token.
# So this looks 5 words to the left and 5 to the right of data.
data_col <- collocates_by_MI(micusp_tokens, "data", 5, 5)

# This 0 to the left and 5 to the right.
data_col <- collocates_by_MI(micusp_tokens, "data", 0, 5)

View(data_col)

# As Brezina observes in the chapter, MI is sensitive to low frequency tokens.
# Thus, it is common to filter to some minium frequency.
#Here we filter to 5 or more.
data_col <- data_col %>% filter(col_freq >= 5)

