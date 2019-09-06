# For the second workshop, we'll be following many of the same procedures
# that we practiced in the first workshop. However, today we'll be working with
# many texts rather than a single sentence (or string).
#
# In this lesson you will learn:
#
# New functions for manipulating data.frames
# - select()
# - mutate()
# - gather()
#
#
# Methods for multivariable data visualization
# - grouping by color
# - grouping by facets / small-multiples
# - histograms
# - scatterplots
#
# We'll begin, just as we always do.
#
# library() loads an R library - sets of functions that we'll want to use during
# this session.
#
# quanteda is a large package with text analysis functions (FYI it can be a bit
# slow to load the first time!)
#
# tidyverse contains functions for manipulating data frames, and also includes
# the entire ggplot2 library for visualizing data frames

library(quanteda)
library(tidyverse)

# Reading in large corpora ----

# If you are working locally on your own maching, rather than through the web
# interface, you can also install the readtext package, which assists in loading
# all of our texts

# install.packages("readtext")
# library(readtext)

# Load a couple of handy functions that will calculate various dispersion
# statistics from a document frequency matrix.
source("functions/dispersion_functions.R")

# Like quanteda, readtext is large and contains many functions we won't need to
# use. So we created our own helper function that does what we need it to do a
# little more efficiently. We can load this an another function that we will use
# later by running the line below.
source("functions/helper_functions.R")


# In your Environment on the upper-left, you will see two functions: effect_size
# and readtext_lite.
#
# Now we are going to load file containing all of the metadata for our corpus.
# For this, we use one of R's many read functions. This one is a specific
# implementation for comma-separated-value (or csv) files.
micusp_meta <- read_csv("data/meta_data/mini_meta.csv")

# We can view the file to see what kinds of information it contains. The data
# comes from the Michigan Corpus of Upper-Level Student Papers (or MICUSP). For
# the workshop, we've sampled 10 papers from each of the 17 disciplines
# represented in the corpus.
#
# You can find out more about MICUSP at http://elicorpora.info/
View(micusp_meta)

# If you ever need to quickly peek at the column names of a data frame, use colnames
colnames(micusp_meta)

# Next we are going to select a subset of categorical variables from the
# metadata, which we will want to attach to and use with our corpus. The
# select() function takes a data.frame and then one or more column names to
# keep. We'll name our new data frame "doc_categories".
doc_categories <- micusp_meta %>% 
  select(text_id, discipline_cat, level_cat, student_gender, speaker_status, paper_type, paper_features)

# In our new data frame, you find the following information:
# a three letter code for each discipline (discipline_cat),
# a code for the grade-level of the student writer (level_cat),
# a code for the gender of the student writer (student_gender),
# and a code for whether student writer identefies as a native or non-native
# speaker of English (speaker_status).
View(doc_categories)

# Because we have 170 text files to load into our corpus, we need to first read
# them into a data frame. To do this, you can use the readtext package or the
# lite function that we've already loaded into our environment.
# Note that you're free to install the readtext package and use
# the readtext::readtext() function.
doc_df <- readtext_lite(micusp_meta$file_path)

# Our data frame (doc_df) has just two columns: doc_id and text.
# Using the corpus function, we can now create a corpus from that data frame.
micusp_corpus <- corpus(doc_df)

# Let's take a look at it.
summary(micusp_corpus)

# Notice that none of our metadata has yet been attached to our corpus.
# Remember our doc_categories? To attach those to our new corpus,
# we can assign them as "document variables" using docvars().
?docvars

# The following command might look backwards, with the function on the left hand
# side of the <- operator. That is because it's an accessor function, which lets
# us add or modify data in an object. You can tell when a function is an
# accessor function like this because its help file will show that you can use
# it with <-, for example as we saw in ?docvars:
#
# docvars(x, field = NULL) <- value
docvars(micusp_corpus) <- doc_categories

# Now let's check our summary again.
summary(micusp_corpus)

# So as a next step, we need to customize exactly how we want quanteda to count
# tokens by using the tokens() function.
#
# tokens() takes our corpus, and then takes additional arguments that customize
# what it does. We'll tell it to tokenize by individual word, and to remove
# punctuation, numbers, and symbols.
micusp_tokens <- tokens(micusp_corpus, include_docvars=TRUE, remove_punct = TRUE,
                        remove_numbers = TRUE, remove_symbols = TRUE, what = "word")

# An issue that we run into frequently with corpus analysis is what to do with
# multi-word expressions. For example, consider a common English quantifier: "a
# lot". Typical tokenization rules will split this into two tokens: "a" and
# "lot". But counting "a lot" as a single unit would really be more accurate. We
# have a way of telling quanteda to account for these tokens.
#
# First, we need to load in a list of our expressions. Using readLines(), we
# load in a text file containing a multi-word expression on each line.
multiword_expressions <- readLines("dictionaries/mwe_short.txt")

# This creates a character vector; in other words, multiple string values.
multiword_expressions

# The tokens_compound() functions looks for token sequences that match our list
# and combines them using an underscore.
micusp_tokens <- tokens_compound(micusp_tokens, pattern = phrase(multiword_expressions))

# In the first workshop, we learned about the pipe %>% operator that chains
# together multiple functions. Starting from the micusp_corpus, create the
# micusp_tokens object using both the tokens() and tokens_compound() command

### YOUR CODE HERE 

# (hint:)
# micusp_corpus %>% 
#    tokens(...) %>% ...

# With our tokens object we can now create a document-feature-matrix using the
# "dfm" function. A dfm is grid with one row per document in the corpus, and one
# column per unique "token" in the corpus. Each cell contains a count of how
# many times a token shows up in that document.
#
# A DFM is a commonly-used data structure for statistical analyses that look at
# word/ngram counts.
micusp_dfm <- dfm(micusp_tokens)

# One way to inspect our dfm is to use the topfeatures() function.
topfeatures(micusp_dfm)

# Next we'll create a dfm with proportionally weighted counts
prop_dfm <- dfm_weight(micusp_dfm, scheme = "prop")

# From that new dfm, we select any token that we'd like to plot.
# In this case, we'll select the most frequent token: "the".
word_df <- dfm_select(prop_dfm, "the", valuetype = "fixed")

# Before plotting, we need to convert this to a data.frame.
# And for the sake of consistency, we'll set the column names 
# to "document" and "freq_norm", as well as normalize per ten thousand words.
word_df <- word_df %>% convert(to = "data.frame") %>%
  setNames(c("document", "freq_norm")) %>%
  mutate(freq_norm = freq_norm*10000)

# To set the width of our bins we'll use the Freedman-Diaconis rule. 
# The bin-width is set to h=2xIQRn^1/3
# So the number of bins is (max−min)/h, where n is the number of observations, 
# max is the maximum value and min is the minimum value.
bin_width <- function(x){
  2 * IQR(x) / length(x)^(1/3)
  }

# Apply to function to the appropriate column.
bw <- bin_width(word_df$freq_norm)

# Now we can plot a histogram.
# We're also adding a dashed line showing the mean.
ggplot(word_df,aes(freq_norm)) + 
  geom_histogram(binwidth = bw, colour="black", fill="white", size=.25) +
  geom_vline(aes(xintercept=mean(freq_norm)), color="red", linetype="dashed", size=.5) +
  theme_classic()

# Now make a histogram of the word "data".

### YOUR CODE HERE


# Now let's try plotting histograms of two tokens on the same plot.
# First we're going to use regular expressions to select the columns.
# The carat or hat ^ looks for the start of line.
# Without it, we would also get words like "blather".
# The dallar symbol $ looks for the end of a line.
# The straight line | means OR.
# Think about how useful this flexibility can be.
# You could, for example, extract all words that end in -ion.
word_df <- dfm_select(prop_dfm, "^the$|^of$", valuetype = "regex")

# Now we'll convert our selection and normalize to 10000 words.
word_df <- word_df %>% convert(to = "data.frame") %>%
  mutate(the = the*10000) %>%
  mutate(of = of*10000)

# Finally, we're going to change the shape of our data.frame.
# ggplot2 wants to look DOWN columns for values.
# So whatever our x-axis value is must be in a single column.
# Same thing for the y-axis.
# AND the same thing for categorical variables or factors
# to which we want to assign things like colors or fills.
# To do this, we use a function called gather(),
# which makes wide data narrow.
# Here we're combining 2 columns of frequencies into 1,
# which we're calling "freq_norm" and we're adding a column
# called "token" that just takes the column headings and makes
# them into rows of values.
# We're also telling the function to ignore the document column.
word_df <- word_df %>% gather("token", "freq_norm", -document) %>% 
  mutate(token = factor(token))

# Look at what we've made.
View(word_df)

# Now let's make a new histogram.
# Here we assign the values of color and fill to the "token" column.
# We also make the columns a little transparent using the "alpha" setting.
ggplot(word_df,aes(x = freq_norm, color = token, fill = token)) + 
  geom_histogram(binwidth = bw, alpha=.5, position = "identity") +
  theme_classic() +
  theme(axis.text = element_text(size=5))

# If we don't want overlapping histograms, we can use
# facet_wrap() to split the plots.
ggplot(word_df,aes(x = freq_norm, color = token, fill = token)) + 
  geom_histogram(binwidth = bw, alpha=.5, position = "identity") +
  theme_classic() +
  theme(axis.text = element_text(size=5)) +
  facet_wrap(~ token)

# I've also created a couple of functions that will calcuate the dispersions of
# a selected token or an entire dfm.
# First, let's look at various dispersion measures for "the" and "data".
dispersions_token(micusp_dfm, "the")

dispersions_token(micusp_dfm, "data")

# We can also generate a data.frame of 7 different dispersion measures.
micusp_disp <- dispersions_all(micusp_dfm)

# We can also store full frequencies for all words using textstat_frequency()
# either for the whole corpus -- textstat_frequency(micusp_dfm)
# or with a grouping variable -- textstat_frequency(micusp_dfm, groups = "discipline_cat").
word_freq <- textstat_frequency(micusp_dfm)

# Let's see what we've created.
View(word_freq)

# With this basic information, we can plot some very important information
# about linguistic data.
# Let's make a very simple scatterplot of the 100 most frequent words
# by their frequency and rank.
ggplot(word_freq[1:100,], aes(x = rank, y = frequency)) +
  geom_point(shape = 1, alpha = .5) +
  theme_classic()

# Now try to create the same plot with with the axes being logs
# of rank and frequency.

### YOUR CODE HERE


# Based on these plots, what are the some implications for
# building corpora and working with linguistic data?

# Overview so far ----

# 1. read in files with readtext()
# 2. corpus()
# 3. tokens()
# 4. dfm()
# 5. analysis (with e.g. geom_histogram())


