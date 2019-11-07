
library(tidyverse)
library(quanteda)
library(rsample)
library(randomForest) # basic implementation
library(Boruta) # variable selection

source("functions/helper_functions.R")

# We're going to repeat the Federalist classification problem.
# Rather than a loasso regression model, however, this time we'll be constructing
# a random forest classification model.
#
# Broadly, random forests generate many classification trees. 
# Each tree gives a classification, and we say the tree "votes" for that class. 
# The forest chooses the classification having the most votes (over all the trees in the forest).  
#
# If you're unfamiliar with random forests, you can (and should) read more here:
# https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm

# As with the script on loasso regression, we'll start with Mosteller & Wallace's lists of tokens.

# Their first group contains 70 tokens...
mw_group1 <- c("a", "all", "also", "an", "and", "any", "are", "as", "at", "be", 
               "been", "but", "by", "can", "do", "down", "even", "every", "for", "from", 
               "had", "has", "have", "her", "his", "if", "in", "into", "is", "it", 
               "its", "may", "more", "must", "my", "no", "not", "now", "of", "on", 
               "one", "only", "or", "our", "shall", "should", "so", "some", "such", "than", 
               "that", "the", "their", "then", "there", "things", "this", "to", "up", "upon", 
               "was", "were", "what", "when", "which", "who", "will", "with", "would", "your")

# Their second an addional 47...
mw_group2 <- c("affect", "again", "although", "among", "another", "because", "between",
               "both", "city", "commonly", "consequently", "considerable", "contribute",
               "defensive", "destruction", "did", "direction", "disgracing", "either",
               "enough", "fortune", "function", "himself", "innovation", "join",
               "language", "most", "nor", "offensive", "often", "pass", "perhaps",
               "rapid", "same", "second", "still", "those", "throughout", "under",
               "vigor", "violate", "violence", "voice", "where", "whether", "while", "whilst")

# And their third another 48 (though they identify some by lemmas and
# another "expence" doesn't appear in our data, possibly because of
# later editing done in our particular edition)...
mw_group3 <- c("about", "according", "adversaries", "after", "aid", "always",
               "apt", "asserted", "before", "being", "better", "care", "choice",
               "common", "danger", "decide", "decides", "decided", "deciding",
               "degree", "during", "expense", "expenses", "extent", "follow", 
               "follows", "followed", "following", "i", "imagine", "imagined", 
               "intrust", "intrusted", "intrusting","kind", "large", "likely", 
               "matter", "matters", "moreover", "necessary", "necessity", "necessities", 
               "others", "particularly", "principle", "probability", "proper", 
               "propriety", "provision", "provisions", "requisite", "substance", 
               "they", "though", "truth", "truths", "us", "usage", "usages",
               "we", "work", "works")

# All together, they list 165 candidate variables, though it works out to be
# 180 unlemmatized tokens as potential variables for their model.
#
# We'll concatenate a vector of all their variables into a single vector.
mw_all <- sort(c(mw_group1, mw_group2, mw_group3))

# And we'll start by setting up our data much like we did for lasso.
# First, we'll get the metadata...
fed_meta <- read_csv("data/meta_data/federalist_meta.csv")

# We'll read in the text...
fed_txt <- readtext_lite(fed_meta$file_path)

# Create a corpus...
fed_corpus <- corpus(fed_txt)

# Tokenize it...
fed_tokens <- tokens(fed_corpus, remove_punct = T, remove_symbols = T, what = "word")

# And create a weighted dfm...
# The 3rd line preps the column so it can be merged with our metadata.
# The 4th orders the tokens by their mean frequencies.
# This isn't necessary here, but can be useful when doing quick subsetting of variables.
# And the 5th changes the column name for easy joining.
fed_dfm <- fed_tokens %>% dfm() %>% dfm_weight(scheme = "prop") %>%
  convert(to = "data.frame") %>%
  mutate(document = str_replace(document, ".txt", "")) %>%
  select(document, names(sort(colMeans(.[,-1]), decreasing = TRUE))) %>%
  rename(text_id = document)

# Now let's join the author_id from the metadata...
fed_dfm <- fed_dfm %>% left_join(select(fed_meta, text_id, author_id)) %>% 
  select(text_id, author_id, everything()) %>% 
  as_tibble()

# And see what we have...
head(fed_dfm)

# Now we can subset out our training data, selecting those columns from
# the M & W list, and also converting the author_id to a factor...
train_dfm <- fed_dfm %>% filter(author_id == "Hamilton" | author_id == "Madison") %>%
  select(text_id, author_id, mw_all) %>%
  mutate(author_id = factor(author_id)) %>%
  column_to_rownames("text_id") %>% data.frame()

# And we'll do the same for our test data...
test_dfm <- fed_dfm %>% filter(author_id == "Disputed") %>%
  select(text_id, author_id, mw_all) %>%
  column_to_rownames("text_id") %>% data.frame()

# Now let's generate a rondom forest model.
# Fist, note that when the training set for the current tree is drawn by sampling with replacement, 
# about one-third of the cases are left out of the sample. This oob (out-of-bag) data
# is used to get a running unbiased estimate of the classification error as trees are
# added to the forest. It is also used to get estimates of variable importance.
# Thus, we don't necessarily need to sample out a validation set of our data,
# though you certainly can...
#
# We'll set our seed...
set.seed(123)

# And generate a model...
fed_m1 <- randomForest(formula = author_id ~ .,data = train_dfm)

# Let's look at the result...
# You can see the confusion matrix, as well as our OOB error rate....
fed_m1

# Let's set aside the error rate for a moment, and walk through the prediction...
# So now we use the model on our test data...
pred_randomForest <- predict(fed_m1, test_dfm)

# And view the predicitons...
pred_randomForest %>% data.frame()

# This is exactly what we predicting using lasso regression....

# Now let's look quickly at variable importance.
# Every node in the decision trees is a condition on a single feature, 
# designed to split the dataset into two so that similar response values end up
# in the same set. The measure based on which (locally) optimal condition 
# is chosen is called impurity. For classification, it is typically either 
# Gini impurity or information gain/entropy. Thus when training a tree, 
# it can be computed how much each feature decreases the weighted impurity in a tree. 
# For a forest, the impurity decrease from each feature can be averaged and 
# the features are ranked according to this measure.
#
# These are the features that are important to our particular model and
# from our model, we can plot these...
fed_m1$importance %>% 
  data.frame() %>%
  rownames_to_column("feature") %>%
  dplyr::arrange(desc(MeanDecreaseGini)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(x = reorder(feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col() +
  coord_flip() +
  labs(x = "", y = "MeanDecreaseGini") +
  ggtitle("Top 25 important variables") +
  theme_classic()

# Let's return to our model...
fed_m1

##
# We have a 7.69% OOB error rate.
# There are variety of ways to try to reduce OOB error.
# One is to tune the random forest model.
# And there are only a few parameters that can be adjusted:
# - mtry: the number of variables to randomly sample as candidates at each split.
# - ntree: the number of trees.
# - sampsize: the number of samples to train on.
# - nodesize: minimum number of samples within the terminal nodes.
# - maxnodes: maximum number of terminal nodes.
#
# Initial tuning can be done with the tuneRF() function.
#
# tuneRf will start at a value of mtry that you supply and increase by a certain step
# factor until the OOB error stops improving be a specified amount. 
# For example, the below starts with mtry = 5 and increases by a factor of 2 
# until the OOB error stops improving by 1%.
set.seed(1234)
test <- tuneRF(
  x          = train_dfm[-1],
  y          = train_dfm$author_id,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 2,
  improve    = 0.01,
  trace      = FALSE 
)

# The result is close to the default value of features/3 or 180/3.
#
# There are other methods turning other parameters.
# For instructions on how to do those, you can consult tutorials like the one here:
# https://uc-r.github.io/random_forests

# Beyond the random forest paramters, however, there are other issues 
# to consider in building our model.
#
# For example, consider the (im)balance in our data.
train_dfm %>% group_by(author_id) %>% tally()

# And, again, look at the confusion matrix.
fed_m1

# Random forest is sensitive to unbalanced data.
# There are a variety of ways to try correcting this.
# Let's start by using stratified sampling and changing our sample sizes.
# We'll create 4 models...
rf1 <- randomForest(author_id~., train_dfm,ntree=500, sampsize=65)
rf2 <- randomForest(author_id~., train_dfm, ntree=1000, sampsize=c(38,14), strata=train_dfm$author_id)
rf3 <- randomForest(author_id~., train_dfm, ntree=1000, sampsize=c(25,14), strata=train_dfm$author_id)
rf4 <- randomForest(author_id~., train_dfm, ntree=1000, sampsize=c(14,14), strata=train_dfm$author_id)

# And check the OOB error for each....
data.frame(rf1 = tail(rf1$err.rate[,1], n=1)*100, rf2 = tail(rf2$err.rate[,1], n=1)*100,
           rf3 = tail(rf3$err.rate[,1], n=1)*100, rf4 = tail(rf4$err.rate[,1], n=1)*100) %>%
  as_tibble()

# Now, let's compare models over multiple iteratations.
# We'll start by setting up vector for 100 interations.
intervals <- c(1:100)

# Next, we'll create an empty vector.
oob_error <- NULL

# Now, we'll generate OOB errors for 100 models based on the full training data.
# This will take a few seconds...
for(i in intervals){
  m1 <- randomForest(
    formula = author_id ~ .,
    data    = train_dfm
  )
  oob <-tail(m1$err.rate[,1], n=1)
  oob_error[[i]] <- oob*100
}

# Let's look at the maximum, minimum, and mean values.
data.frame(oob_max = max(oob_error), oob_min = min(oob_error), mean_oob = mean(oob_error)) %>%
  as_tibble()

# Now, we'll reset the vector.
oob_error <- NULL

# And populate it with values based on the model with adjusted sampling.
for(i in intervals){
  m1 <- randomForest(
    formula  = author_id ~ .,
    data     = train_dfm,
    ntree    = 1000, 
    sampsize = c(14,14), 
    strata   = train_dfm$author_id
  )
  oob <-tail(m1$err.rate[,1], n=1)
  oob_error[[i]] <- oob*100
}

# Again, we'll check our OOB error...
data.frame(oob_max = max(oob_error), oob_min = min(oob_error), mean_oob = mean(oob_error)) %>%
  as_tibble()

# So now we use the adjusted model on our test data...
pred_randomForest <- predict(rf4, test_dfm)

# And view the predicitons...
pred_randomForest %>% data.frame()

# Finally, let's check similar model ajustments agaist a validcation set.
# For this, as we did with lasso, we'll use some handy functions from the rsample package.
# First, we make an 80/20 split.
set.seed(123)
valid_split <- initial_split(train_dfm, .8)

# From that we create a new and smaller training set.
train_dfm_v2 <- analysis(valid_split)

# And check the breakdown of our authors.
train_dfm_v2 %>% group_by(author_id) %>% tally()

# And a validation set.
train_valid <- assessment(valid_split)

# Now we can create a new model, using statified sampling.
rf_valid <- randomForest(
  formula  = author_id ~ .,
  data     = train_dfm_v2,
  ntree    = 1000, 
  sampsize = c(12,12), 
  strata   = train_dfm_v2$author_id
)

# Make our predictions on the validation set.
pred_randomForest <- predict(rf_valid, train_valid)

# Look at what they actually are...
table(pred=pred_randomForest, true=train_valid$author_id)

# And calculate our model accuracy...
mean(pred_randomForest == train_valid$author_id)





