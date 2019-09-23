
library(quanteda)
library(tidyverse)
library(corpora)

# The following script uses the prop.cint() function in corpora to calculate
# and plot confidence intervals. The function uses the 
# Clopper-Pearson method (inverted exact binomial test).
# For more information you can see here:
# https://sigmazone.com/binomial-confidence-intervals/
#
# Note that with word counts, confidence intervals shrink as the size of the 
# corpus increases. With large corpora, the ci compresses to near zero.
# Thus, this technique is most useful with smaller, specialized corpora.

# We'll load our helper functions
source("functions/helper_functions.R")

# Read in our metadata.
micusp_meta <- read_csv("data/meta_data/mini_meta.csv")

# Read our texts into a data.frame.
doc_df <- readtext_lite(micusp_meta$file_path)

# Create a corpus from that data.frame.
micusp_corpus <- corpus(doc_df)

# Tokenize the corpus.
micusp_tokens <- tokens(micusp_corpus, remove_punct = TRUE, remove_numbers = TRUE, 
                        remove_symbols = TRUE, what = "word")

# Load our multi-word expressions.
multiword_expressions <- readLines("dictionaries/mwe_short.txt")

# Compound tokens based on the list.
micusp_tokens <- tokens_compound(micusp_tokens, pattern = phrase(multiword_expressions))

# Create our dfm.
micusp_dfm <- dfm(micusp_tokens)

# Create a table of frequencies adding a column for normalized frequencies.
word_freq <- textstat_frequency(micusp_dfm) %>% 
  mutate(freq_norm = frequency/sum(frequency))

# We'll sumn or frequency column and store the value for other calculations.
total <- sum(word_freq$frequency)

# Look at what the function prop.cint() produces for the frequency of "the".
prop.cint(k = 33416, n = total, conf.level = 0.95)

# To apply this function to our counts and append the results we can do the following.
word_freq <- word_freq %>% 
  bind_cols(prop.cint(k = as.vector(word_freq$frequency), n = total, conf.level = 0.95))

# Check the result
View(word_freq)

# Now we can plot adding geom_errorbar() to the bars.
ggplot(word_freq[1:5, ], aes(x=reorder(feature, -freq_norm), y=freq_norm)) +
  geom_bar(stat = "identity", fill="steelblue") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  labs(x="", y = "frequency per ten thousand words") +
  theme_classic() +
  theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=8))

