# library() loads an R library - sets of functions that we'll want to use during
# this session.
#
# quanteda is a large package with text analysis functions (FYI it can be a bit
# slow to load the first time!)
#
# tidyverse is another popular package that includes ggplot2, 
# which has important data visualization functions

library(quanteda)
library(tidyverse)

# Let's begin by creating an object consisting of a character string. In this
# case, the first sentence from _Sense and Sensibility_.
text <- "It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife."

# the <- stores the data into an object called "text" that will live in our
# "environment"

# Using the "class" function we can access the object's attributes (aka what
# KIND of object it is)
class(text)

# By just running the bare name of the object, we can also call up the values
# stored in the object itself (aka what's IN the object)
text

# From our text we can create our corpus object by using the "corpus" function.
small_corpus <- corpus(text)

# Let's look at a summary of our corpus.
summary(small_corpus)

# The first column shows the ids of all the texts in our coprus. Here, we only
# have 1, so our text has been assigned "text1" as an id. Ids are very important
# as they link any meta data we may have about our texts with the textual data
# itself; we will see more of how this works in our next workshop.
#
# The next column shows the number of types we have in our corpus, that is,
# unique tokens (bits of text that aren't spaces) and the next shows the TOTAL
# number of tokens.

# Count the total number of words in text by hand.

# Why might our corpus list 26 "tokens" instead of 23?

# So as a next step, we need to customize exactly how we want quanteda to count
# tokens by using the tokens() function.
#
# tokens() takes our corpus, and then takes additional arguments that customize
# what it does. We'll tell it to tokenize by individual word, and to remove
# punctuation.
small_tokens <- tokens(small_corpus, what = "word", remove_punct = TRUE)

# Now look at the results
small_tokens

summary(small_tokens)

# To learn more about the "tokens" function and its other optional arguments, we
# can put a question mark in front of the function name and run the line.
?tokens

# Note, that we can tokenize for multi-word sequences or "ngrams". Let's
# make an object containing 2-word sequences, or bigrams
bi_grams <- tokens(small_corpus, what = "word", remove_punct = TRUE, ngrams = 2)

# View the object
bi_grams

# Now try to create an object containing 3-word sequences, or trigrams

### YOUR CODE HERE

# With our tokens object we can now create a document-feature-matrix using the
# "dfm" function. A dfm is grid with one row per document in the corpus, and one
# column per unique "token" in the corpus. Each cell contains a count of how
# many times a token shows up in that document.
#
# A DFM is a commonly-used data structure for statistical analyses that look at
# word/ngram counts. We'll do basic operations with it in this workshop, and
# progress to more complex analyses in the next workshop.
small_dfm <- dfm(small_tokens)
small_dfm

# Check that that this dfm has one row
nrow(small_dfm)

# How many columns (aka unique tokens) does it have?
ncol(small_dfm)

# The textstat_frequency() function gives us the frequency of each term in the
# corpus
textstat_frequency(small_dfm)

# Now try to create a dfm of the bigrams on your own and check the most frequent
# bigrams.

### YOUR CODE HERE

# Overview so far ----

# 1. bare text
# 2. corpus()
# 3. tokens()
# 4. dfm()
# 5. analysis (with e.g. textstat_frequency())

# Quick intro to plotting ----

# We'll be doing more plotting in the next workshop, but we'll finish this intro
# with the most basic plot: a bar plot.
#
# ggplot works with data frames, that is, tables with rows and columns. The
# output of textstat_frequency is already a data.frame, so lets save it to a new
# object:
token_counts <- textstat_frequency(small_dfm)

# Finally, let's add a column of normalized frequencies
token_counts <- token_counts %>% mutate(freq_norm = frequency / sum(frequency))

# Before we move on, we should check our work.
sum(token_counts$freq_norm)

# Let's look at what we've created.
View(token_counts)

# ggplot() takes a data frame, and then uses aes() (for "aesthetics") to specify
# which columns should be mapped to which visual variables, such as x, y, color,
# fill, alpha (transparency), etc.
#
# Once you specify this base, you then use the + sign to add at least one geom_
# layer (for "geometry") which specifies which way you want to render the
# aesthethics you specified. For example, geom_col renders x and y as bars of
# different heights, while geom_point would render x and y as points in space
#
# As we add additional layers, scales, or theme modifications onto this plot,
# we'll keep using +

# Our graph will be very basic - we just want to plot the frequency of each
# word, with words on the x axis and the frequency of each word on the y axis
ggplot(token_counts, aes(x = feature, y = freq_norm)) + 
  geom_col()

# This looks a bit ugly to start out with, because our x axis values are whole
# words that don't easily fit on the horizontal axis. ggplot allows a LOT of
# visual customization, but for now we'll just use coord_flip() to flip the axes
# and display the labels more comfortably
ggplot(token_counts, aes(x = feature, y = freq_norm)) +
  geom_col() +
  coord_flip()

# In this caese, we may want to rearrange our data so that the most frequently
# ocurring word comes first. We could do this rearranging on the underlying
# data, but since right now we just want to do this in the context of our plot,
# we'll do the reordering right inside the ggplot(aes()) call.
#
# Reorder changes the ordering of the first variable you give it, ordering it
# based on the second variable you give it
ggplot(token_counts, aes(x = reorder(feature, freq_norm), y = freq_norm)) +
  geom_col() +
  coord_flip()

# Earlier you counted all the bigrams in this text. Create a plot showing them
# in order. Remember first you need to save the bigram counts from
# textstat_frequency to a new object, and then pass that object to ggplot

### YOUR CODE HERE

# So far we've only looked at words or word combinations. Using all we've
# learned today, create a bar plot counting up the individual letters, a-z, used
# in our original sentence.

### YOUR CODE HERE

# Bonus: see if you can make the "fill" color for each bar change based on its
# frequency

# LEARN MORE ABOUT GGPLOT2 (including cheat sheets and links to other resources
# at various levels of depth and difficulty):
#
# https://ggplot2.tidyverse.org/
