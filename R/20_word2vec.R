
library(tidyverse)
library(wordVectors)
library(factoextra)
library(tsne)

# To install wordVectors run:
# devtools::install_github("bmschmidt/wordVectors")
#
# And for a nice introduction to word vectors see here:
# http://mccormickml.com/2016/04/19/word2vec-tutorial-the-skip-gram-model/

# This excercise uses the wordVectors package to build and process vector space models
# and word embeddings. For package detail see here:
# https://github.com/bmschmidt/wordVectors
#
# The exercise also closely follows Schmidt's vingette which is here:
# https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd
#
# The first step in building a model is to tokenize a corpus and combine the texts
# into a single file. Note that not ALL word2vec packages require you to do this.
# The wordVectors packages has a prep_word2vec() that does this:
# prep_word2vec(origin="cookbook_text",destination="cookbooks.txt",lowercase=T,bundle_ngrams=2)
#
# However, you may choose to tokenize and concatenate texts in whatever manner you choose.
# And the structure of that text (whether converted to lowercase or not, for example)
# will determine what is included in the model ("The" and "the", or just "the").
#
# That combined text file is then used to train the model.
# There are 2 basic kinds of models:
# - continuous bag of words (CBOW), which uses context to predict a target word
# - or skip-gram, which uses a word to predict a target context,
# Skip-grams are the default for the train_word2vec() function.
#
# Training a model takes some time, so we have prepared one in advance.
# The basic syntax looks like this:
# model <- train_word2vec("cookbooks.txt","cookbook_vectors.bin", vectors=200, threads=4, window=12, iter=5, negative_samples=0)
#
# A few notes from the package's creator:
# 1. The `vectors` parameter is the dimensionality of the representation. More vectors usually means more precision, but also more random error and slower operations. Likely choices are probably in the range 100-500.
# 2. The `threads` parameter is the number of processors to use on your computer. On a modern laptop, the fastest results will probably be between 2 and 8 threads, depending on the number of cores.
# 3. `iter` is how many times to read through the corpus. With fewer than 100 books, it can greatly help to increase the number of passes; if you're working with billions of words, it probably matters less. One danger of too low a number of iterations is that words that aren't closely related will seem to be closer than they are.
# 4. Training can take a while. On my laptop, it takes a few minutes to train these cookbooks; larger models take proportionally more time. Because of the importance of more iterations to reducing noise, don't be afraid to set things up to require a lot of training time (as much as a day!)
# 5. One of the best things about the word2vec algorithm is that it *does* work on extremely large corpora in linear time.
# 6. If at any point you want to *read in* a previously trained model, you can do so by typing `model =  read.vectors("cookbook_vectors.bin")`.
#
# With those preliminaries out of the way, let's start...
# First, we'll read in our model.
model <- read.binary.vectors("models/cookbook_vectors.bin")

# Now, we can elements closest to any word, like...
model %>% closest_to("fish")

# Let's save the 100 words closest to fish...
fish_neighbors <- model %>% closest_to("fish", 100)

# With that table, we can use the "word" column to subset our vector model.
# We'll call this "fishy"...
fishy <- model[[fish_neighbors$word, average=F]]

# We can acess our vector information from the VectorSpaceModel structure...
fishy@.Data %>% as_tibble()

# This is very useful, as we can use that data to generate plots.
# One common way to flatten high demensional space in 2 is to use PCA.
# So let's try it with "fishy" and plot it using the factoextra package....
prcomp(fishy@.Data) %>% fviz_pca_ind()

##
# Note that our 2 dimensions don't explain a whole lot of the overall variation.
# So we need to be careful not to accept positions on the plot at face value.
# That said, what do you note about Dim1? Dim2?
##

# We can also search for multiple words...
model %>% 
  closest_to(model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]],10) %>%
  as_tibble()

# Again, we can save these...
some_fish <- closest_to(model, model[[c("fish","salmon","trout","shad","flounder","carp","roe","eels")]], 100)

# Subset our model...
fishy <- model[[some_fish$word, average=F]]

# And use PCA to plot the result...
prcomp(fishy@.Data) %>% fviz_pca_ind()

# Another interesting property of these models is that we apply simple arithmetic operations.
# The paradigmatic example is something like:
# model %>% closest_to(~"king" - "man" + "woman")
# to produce words like "queen" and "princess".
# We have a fairly small model that doesn't make it easy to illustrate this;
# however, we'll try a simple example.
# Let's check the words closest to "pie"...
model %>% closest_to("pie", 20)

# Now, we'll add "meat"...
model %>% closest_to(~"pie" + "meat", 20)

# PCA isn't the only way to visualize relationships.
# Let's look at 2 common ingredients...
tastes <- model[[c("sugar","salt"), average=F]]

# We'll calculate cosine similarity of the 3000 most frequent word to our "tastes"...
sweet_and_saltiness <- model[1:3000,] %>% cosineSimilarity(tastes)

# Filter to the top 20 sugar or salt and generate a data.frame.
sweet_and_saltiness <- sweet_and_saltiness[
   rank(-sweet_and_saltiness[,1]) < 20 |
   rank(-sweet_and_saltiness[,2]) < 20,] %>% data.frame()

# And plot the result...
ggplot(sweet_and_saltiness, aes(x = salt, y = sugar)) +
  geom_text(label = rownames(sweet_and_saltiness)) +
  theme_classic()

# Now let's expand our investigation to the 5 tastes: sweet, salty, bitter, sour, and savory.
tastes <- model[[c("sweet","salty","savory","bitter","sour"), average=F]]

# Rather than use a base matrix of the whole set, we can shrink down to just five
# dimensions: how similar every word in our set is to each of these five.
# This uses cosine similarity , so the closer a number is to one, the more similar it is.
# And we're only condisering the 3000 most common words in the set.
common_similarities_tastes <- model[1:3000,] %>% cosineSimilarity(tastes)

# Now we can filter down to the 75  words that are closest to *any* of these
# That's what # the apply-max function below does
# and use a PCA biplot to look at just 75 words in a flavor plane.
high_similarities_to_tastes <- common_similarities_tastes[rank(-apply(common_similarities_tastes,1, max)) < 75,]

high_similarities_to_tastes %>% prcomp %>% fviz_pca_biplot()

##
# What do you make of the result?
##

# wordVectors also has a function for using tSNE to reduce demnsionality.
# Just calling "plot" will display individual tokens grouped relatively close to each other 
# based on their proximity in the higher dimensional space.
# As the author of the package notes:
# "Perplexity" is the optimal number of neighbors for each word. By default it's 50; 
# smaller numbers may cause clusters to appear more dramatically at the cost of overall coherence.

plot(model, perplexity=50)

# Time permitting or on your own what follows is a walkthough for constructing
# a model using GloVe:
# https://nlp.stanford.edu/projects/glove/
# This also creates vectors for word representation, though somewhat differently
# that the process described above.
# This process relies on the text2vec package:
# http://text2vec.org/glove.html

# Load the library
library(text2vec)

# Load the helper functions
source("functions/helper_functions.R")

# Read in our corpus
cook_txt <- list.files("data/text_data/cookbook_corpus", full.names = T) %>%
  readtext_lite()

# Here we use text2vec's tokenizer, which we point to the approriate columns..
cook_tks <- itoken(cook_txt$text, 
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer, 
                  ids = cook_txt$doc_id, 
                  progressbar = TRUE)

# From those, we generate a vocabulary. Terms will be unigrams (simple words).
cook_vocab <- create_vocabulary(cook_tks)

# And we'll prune that vocabulary to include tokens that appear more than once.
cook_vocab <- prune_vocabulary(cook_vocab, term_count_min = 2L)

# Check the result.
cook_vocab

# Now we are ready to construct term-co-occurence matrix (TCM).
# Which takes 2 steps... We vectorize our vocabulary...
vectorizer <- vocab_vectorizer(cook_vocab)
# And use window of 5 for context words.
tcm <- create_tcm(cook_tks, vectorizer, skip_grams_window = 5L)

# Now we have a TCM matrix and can factorize it via the GloVe algorithm.
# text2vec uses a parallel stochastic gradient descent algorithm. 
# By default it will use all cores on your machine, but you can specify
# the number of cores if you wish. For example, to use 4 threads call 
# RcppParallel::setThreadOptions(numThreads = 4)
glove <- GlobalVectors$new(word_vectors_size = 50, vocabulary = cook_vocab, x_max = 10)

# Let’s fit our model. (It can take several minutes...)
cook_main <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)

# On par with any other mlapiDecomposition model second low-rank matrix 
# (context word vectors) is available in components field.
cook_context <- glove$components

# While both of word-vectors matrices can be used as result it usually better 
# (idea from GloVe paper) to average or take a sum of main and context vector.
cook_vectors <-  cook_main + t(cook_context)

# The process for finding cosine similarity is a little more elaborated that
# in the wordVectors package...
fishy <- cook_vectors["fish", , drop = FALSE]
fishy <- sim2(x = cook_vectors, y = fishy, method = "cosine", norm = "l2")
head(sort(fishy[,1], decreasing = TRUE), 5)

# However, we can convert our GloVe model...
cook_vectors <- as.matrix(cook_main + t(cook_context)) %>%
  as.VectorSpaceModel()

# And use the wordVectors syntax, if we wish...
cook_vectors %>% closest_to("fish", 20)


