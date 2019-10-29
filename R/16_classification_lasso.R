
library(tidyverse)
library(quanteda)
library(rsample)

source("functions/helper_functions.R")

# We going to (mostly) replicate a famous classification task that was
# first done by Frederick Mosteller and David L. Wallace in the early 1960s.
# They undertook this task without the benefit of modern text processing. It was
# truly a pioneering work of computational linguistics and information theory.
# Keep that in mind and we do in minutes what took them months.
# You can read their study here: 
# https://www.jstor.org/stable/2283270?seq=1#page_scan_tab_contents
# And an overview is here:
# https://priceonomics.com/how-statistics-solved-a-175-year-old-mystery-about/
#

# Because of computational limits, they needed to identify potentially productive
# variables ahead of building their regession model. This is not how we would
# go about it now, but it was a constraint at the time. They ended up
# creating 6 bins of likely words, and those are reported in 3 groupings in their study.
#
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

# For their task, Mosteller & Wallace were interested in solving a long-standing
# historical debate about the disputed authorship of 12 of the Federalist Papers.
# The Federalist Papers are made up of 85 articles and essays written by 
# Alexander Hamilton, James Madison, and John Jay under the pseudonym "Publius" 
# to promote the ratification of the United States Constitution.
#
# Authorship of the articles has been disputed since their publication,
# With Hamilton providing a list to his lawer before his death, and Madison
# another disputed some of Hamilton's claims.
#
# We're going to work from the generally accepted authorship designations,
# which assign authorship of 51 articles to Hamilton, 14 to Madison,
# 5 to Jay, and 3 to joint authorship. The other 12 are in doubt as to
# whether they were written by Hamilton or Madison.
#
# So let's begin...
# First, we'll get the metadata...
fed_meta <- read_csv("data/meta_data/federalist_meta.csv")

# And we're going to read in ALL of the data.
# Why do it this way? We could build out seperate data sets for training,
# validating, and predicting. HOWEVER, we need our data to be identitical in
# structure at every step. This can become tedious if you're forever wrangling
# data.frames to get them as the need to be. It's much easier to begin with
# one dfm and subset as necessary for the classification process.
#
# So let's read in the text...
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

# Now we can subset out our training data...
train_dfm <- fed_dfm %>% filter(author_id == "Hamilton" | author_id == "Madison")

# And our testing data...
test_dfm <- fed_dfm %>% filter(author_id == "Disputed")

set.seed(123)

# For the next step we're going to again separate our training data.
# We want a subset of known data against which we can validate our model.
#
# For this, we'll use some handy functions from the rsample package.
# First, we make an 18/20 split.
valid_split <- initial_split(train_dfm, .8)

# From that we create a new and smaller training set.
train_dfm_v2 <- analysis(valid_split)

# And a validation set.
train_valid <- assessment(valid_split)

# Next, we'll select only those 70 tokens from Mosteller & Wallace's first group.
# We also need to convert the author_id into a 2-level factor,
# and to move the text_id to row names.
train_dfm_v2_1 <- train_dfm_v2 %>% select(text_id, author_id, mw_group1) %>%
  mutate(author_id = factor(author_id)) %>%
  column_to_rownames("text_id")

# The same for the validation data, but we don't need to worry about the factor conversion.
train_valid_1 <- train_valid %>% select(text_id, author_id, mw_group1) %>%
  column_to_rownames("text_id")

# For our regression, we're going to take advantage of lasso regression,
# This is a form of penalized logistic regression, which imposes a penalty 
# to the logistic model for having too many variables. 
# This results in shrinking the coefficients of the less contributive 
# variables toward zero. This is also known as regularization.
#
# For this, we'll use the glmnet package.
library(glmnet)

# The fit returned for lasso regression is for an entire sequence of lambdas, 
# from a large lambda that results in the model having 0 coefficients 
# to one where all variables are entered into the model.
# For more detail on lasso regression you can look here:
# https://eight2late.wordpress.com/2017/07/11/a-gentle-introduction-to-logistic-regression-and-lasso-regularisation-using-r/
#
# This is a very useful technique for variable selection and can reduce
# the likelihood of overfitting. This is particularly helpful in linguistic
# analysis where we're often working with many varialbles making 
# the implementation of functions like step() sometimes tedious.
#
# To help you decide which lambda to use, the cv.glmnet() function 
# does cross-validation.
cv_fit <- cv.glmnet(as.matrix(train_dfm_v2_1[, -1]), train_dfm_v2_1[, 1], family = "binomial")

# We can plot the log of the resulting lambdas.
plot(cv_fit)

# The plot displays the cross-validation error according to the log of lambda.
# The left dashed vertical line indicates that the log of the optimal value of 
# lambda is approximately -6, which is the one that minimizes the prediction 
# error. This lambda value will give the most accurate model. 
# The exact value of lambda can aslo be viewed.
lambda_min <- cv_fit$lambda.min

# And we'll save our regression coefficients.
lambda_lse <- cv_fit$lambda.1se

# And we see only the variables have been included in the model.
# Ours has 12.
coef(cv_fit, s = "lambda.min")


# To validate the model, let's create a model matrix from the texts we've
# split off for that purpose.
x_test <- model.matrix(author_id ~., train_valid_1)[,-1]

# From our model, we'll predict the author_id of the validation set.
lasso_prob <- predict(cv_fit, newx = x_test, s = lambda_lse, type = "response")

# From the probabilities, we can return the predicted authors.
lasso_predict <- ifelse(lasso_prob > 0.5, "Madison", "Hamilton")

# Retrieve what they actually are...
table(pred=lasso_predict, true=train_valid_1$author_id)

# And calculate our model accuracy...
mean(lasso_predict == train_valid_1$author_id)

# Ours is about 92% accurate. Not too bad.
# Note that if you wanted to really test the model, we could create a function
# to run through this process starting with the sampling.
# That way, we could generate a range of accuracy over repeated sampling
# of training and validation data.
#
# Let's repeat the process, but this time we'll start with all
# of Mosteller & Wallace's candidate variables.
#
# We'll create a new training set...
train_dfm_v2_2 <- train_dfm_v2 %>% select(text_id, author_id, mw_all) %>%
  mutate(author_id = factor(author_id)) %>%
  column_to_rownames("text_id")

# And validation set...
train_valid_2 <- train_valid %>% select(text_id, author_id, mw_all) %>%
  column_to_rownames("text_id")

# We'll do our cross-validation...
cv_fit <- cv.glmnet(as.matrix(train_dfm_v2_2[, -1]), train_dfm_v2_2[, 1], family = "binomial")

# Look at our coefficients... 17 this time...
coef(cv_fit, s = "lambda.min")

# Save our minimum lambda.
lambda_min <- cv_fit$lambda.min

# And  our regression coefficients.
lambda_lse <- cv_fit$lambda.1se

# Create a matrix from the validation set...
x_test <- model.matrix(author_id ~., train_valid_2)[,-1]

# From our model, we'll predict the author_id of the validation set.
lasso_prob <- predict(cv_fit, newx = x_test, s = lambda_lse, type = "response")

# From the probabilities, we can return the predicted authors.
lasso_predict <- ifelse(lasso_prob > 0.5, "Madison", "Hamilton")

# Retrieve what they actually are...
table(pred=lasso_predict, true=train_valid_1$author_id)

# And calculate our model accuracy...
mean(lasso_predict == train_valid_1$author_id)

# The model looks pretty good... So let's proceed with the data in question.

# This time, we'll use the full training set for our model.
train_dfm <- train_dfm %>% select(text_id, author_id, mw_all) %>%
  mutate(author_id = factor(author_id)) %>%
  column_to_rownames("text_id")

# We'll prep the data we want to test.
test_dfm <- test_dfm %>% select(text_id, author_id, mw_all) %>%
  column_to_rownames("text_id")

# We'll do our cross-validation...
cv_fit <- cv.glmnet(as.matrix(train_dfm[, -1]), train_dfm[, 1], family = "binomial")

# As we would expect, this is close to what we saw previously...
coef(cv_fit, s = "lambda.min")

# Create a matrix from the test set...
x_test <- model.matrix(author_id ~., test_dfm)[,-1]

# From our model, we'll predict the author_id of the validation set.
lasso_prob <- predict(cv_fit, newx = x_test, s = lambda_lse, type = "response")

# From the probabilities, we can return the predicted authors.
lasso_predict <- ifelse(lasso_prob > 0.5, "Madison", "Hamilton")

# So what do we get?
data.frame(lasso_predict, lasso_prob)

# Our model predicts all but 55 were written by Madison.
# Our model is not particularly confident about that result.
# This hews pretty cloesly to Mosteller & Wallace's findings,
# through they come down (sort of) on the side of Madison for 55. However,
# they also acknowledge that the evidence is weak and not very convincing.
#
# It's worth noting, too, that other studies using other techniques
# have suggested that 55 was authored by Hamilton. See, for example, here:
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0054998


