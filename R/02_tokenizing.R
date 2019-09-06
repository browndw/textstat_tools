library(quanteda)
library(tidyverse)

# For this exercise, we'll be thinking about the process of tokenizing in more detail.
# Tokenizing is a very important methodolological decision,
# because it affects everything that comes after.
# We need to decide: What are we counting? And why?

# Let's begin by creating an object consisting of a character string.
# Again, it's the first sentence from _Sense and Sensibility_.
text_1 <- "It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife."

# the <- stores the data into an object called "text" that will live in our
# "environment"

# Just as we did before, from our text we can create our corpus object by using the "corpus" function.
corpus_1 <- corpus(text_1)

# And we use the tokens() function to tokenize it.
tokens_1 <- tokens(corpus_1, what = "word", remove_punct = TRUE)

# Check what we've created.
tokens_1

# Now what if we wanted to include punctuation in our counts.
# Easy. We can change the argument "remove_punct" to FALSE

tokens_1 <- tokens(corpus_1, what = "word", remove_punct = FALSE)

# Check again. Note that we have 2 commas and period included.
tokens_1

# Now let's look at a string that is a little more complicated.
text_2 <- "Jane Austen was not credited as the author of 'Pride and Prejudice.' In 1813, the title page simply read \"by the author of Sense and Sensibility.\" It wasn't until after Austen's death that her identity was revealed. #MentalFlossBookClub with @HowLifeUnfolds #15Pages https://pbs.twimg.com/media/EBOUqbfWwAABEoj.jpg"

# Look at the string.
text_2

# This is the text of tweet, and it includes @-mentions, hashtages,
# a link, digits, as well as a possessive and a contraction.
# What do we want to do with these?

# First, let's create our corpus.
corpus_2 <- corpus(text_2)

# Now we can look at some options.
# What happens if we do it the way we did with the first string?
tokens_2 <- tokens(corpus_2, what = "word", remove_punct = TRUE)

# Look what happens to the url at the end of the string.
tokens_2

# With the "what" argument, we can adjust what the tokenizing function
# looks for as a break between words.
# The "fastestword" option just looks for spaces.
tokens_2 <- tokens(corpus_2, what = "fastestword", remove_punct = TRUE)

# Now look what happens to the url at the end of the string.
# But also note what happens to any punctuation immediately
# before or after a word character.
tokens_2

# One option is simply to remove any links, just like we can remove punctuation,
# by adjusting the arguments in tokens() and going back to the more
# sensitive "word" option

tokens_2 <- tokens(corpus_2, what = "word", 
                  remove_punct = TRUE, remove_url = TRUE)

# Check the result
tokens_2

# Now try to create a tokens object that removes the year.
# You can look at the function reference here: https://quanteda.io/reference/tokens.html
# Or simply using ?tokens

### YOUR CODE HERE

# With our tokens object we can now create a document-feature-matrix using the

# Another issue is what to do with phrases like "a lot"
# that we may want to count as single tokens.
# To do that, we can create a vector of strings that we want quanteda
# to combine into single tokens.
multiword_expressions <- c("Jane Austen", "Pride and Prejudice", "title page")

# Then we can use the tokens_compound() function.
tokens_2 <- tokens_compound(tokens_2, pattern = phrase(multiword_expressions))

# Check the output
tokens_2

# The other tokenizing tip that you should know is how to pre-process the text
# BEFORE using the tokens() function in quanteda.

# For example, let's say you were interested in counting forms of
# negation like "not" and contracted forms like "n't".
# To count those, you can do some preparation before tokenizing.

# First, we'll save some text into our environment.
text_3 <- "The more I dove in, though, the less I cared. I watched BTS perform their 2018 anthem \"Idol\" on The Tonight Show and wondered how their lungs didn't explode from exertion. I watched the sumptuous short film for their 2016 hit \"Blood, Sweat, and Tears\" and couldn't tell whether I was more impressed by the choreography or the high-concept storytelling. And I was entranced by the video for \"Spring Day,\" with its dreamlike cinematography and references to Ursula K. Le Guin and Bong Joon-ho's film Snowpiercer. When I learned that the video is often interpreted as a tribute to the school-age victims of 2014's Sewol ferry disaster, I replayed it and cried."

# Check the next and note the contractions.
text_3

# There are a number of ways of manipulating strings, like using the
# gsub() function in base R. But as we've already loaded tidyverse,
# the str_replace_all() function is well-suited to this task.
text_3 <- str_replace_all(text_3, "n't", " n't")

# Now we'll tokenize it.
tokens_3 <- tokens(text_3, what = "word", remove_punct = TRUE)

# And check the output.
tokens_3

# Overview so far ----

# 1. various arguments for the tokens() function that can:
#   a. keep or remove punctuation
#   b. keep or remove numbers
#   c. keep or revove URLs
#   d. keep or remove Twitter symbols
# 2. options for the "what" argument that changes how tokens() senses token boundaries
# 3. using a vector and tokens_compound() to create multi-word units
# 4. using str_replace_all() to pre-process text before tokenizing it

# Okay. Now let's run this line to clear our environment
remove(list = ls())

# Next, we'll read in our BTS text again.
text_3 <- "The more I dove in, though, the less I cared. I watched BTS perform their 2018 anthem \"Idol\" on The Tonight Show and wondered how their lungs didn't explode from exertion. I watched the sumptuous short film for their 2016 hit \"Blood, Sweat, and Tears\" and couldn't tell whether I was more impressed by the choreography or the high-concept storytelling. And I was entranced by the video for \"Spring Day,\" with its dreamlike cinematography and references to Ursula K. Le Guin and Bong Joon-ho's film Snowpiercer. When I learned that the video is often interpreted as a tribute to the school-age victims of 2014's Sewol ferry disaster, I replayed it and cried."

# Now do the following:
# 1. tokenize the text using any combination of techniques we've learned thus far
# 2. create a dfm (described in the day01_basics script)
# 3. use textstat_frequency() to creat a dataframe of counts
# 4. use ggplot2() to make a barplot of those counts ordered from high to low

### YOUR CODE HERE
