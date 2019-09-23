
library(quanteda)
library(tidyverse)
library(nnet)
library(DescTools)

# This is a script that walks you through basic logistic and multinomial
# regression. Logistic regression can be applied to binary variables.
# In our textbook, Brezina walks though an examples applied to 
# lexico-grammar. What are the conditions that predict whether the definite
# article ("the") or the indefinite article ("a") occur? Or predict which 
# relative pronoun ("that" or "which") is used?
#
# Multinomial regession can be applied to situations where we have
# more than 2 outcome variables AND those categories are UNORDERED.
# Here, we'll look at writing in the Humanities, Sciences, and Social Sciences.
# There is not inherent order to these categores. If we did have
# ORDERED categories, we would use ordinal regression.
# These are very similar procedures, with similar reporting conventions.

# We're going to start by preparing a verions of the that/which
# data that Brezina describes starting on pg. 130

# We have student writing from the US in the MICUSP data
# and from the UK in the BAWE data.
#
# Rather than going through the tagging process, we'll load in
# some data that's already been tagged using the CLAWS7 tagset:
# http://ucrel.lancs.ac.uk/claws7tags.html

us_files_list <- list.files("data/text_data/micusp_tagged", full.names = T)
uk_files_list <- list.files("data/text_data/bawe_tagged", full.names = T)

# We'll also load our metadata for later.
micusp_meta <- read_csv("data/meta_data/micusp_meta.csv")
bawe_meta <- read_csv("data/meta_data/bawe_meta.csv")

# Load our helper functions.
source("functions/helper_functions.R")

# From this data, we'll sample 100 files.
set.seed(5)

us_sample <- sample(us_files_list, 100)
uk_sample <- sample(uk_files_list, 100)

# From those, we'll create data frames with the text.
us_df <- readtext_lite(us_sample)
uk_df <- readtext_lite(uk_sample)

# You should open a file to see how they're structured.
# Each token is followed by an embedded pos tag and
# each sentence is prefixed and suffixed by a non-embedded tag:
# <s>
# Table_NN1 1_MC1 summarises_VVZ the_AT set-up_NN1 of_IO the_AT experiment_NN1 ._. 
# </s>

# We're not going to be using the sentence tags, we're going to get rid of them.
us_df <- us_df %>% mutate(text = str_replace_all(text, "<\\S+>", ""))
uk_df <- uk_df %>% mutate(text = str_replace_all(text, "<\\S+>", ""))

# Now we'll make our corpus objects.
us_corpus <- corpus(us_df)
uk_corpus <- corpus(uk_df)

# And we'll quickly tokenize them.
us_tokens <- tokens(us_corpus, what = "fasterword")
uk_tokens <- tokens(uk_corpus, what = "fasterword")

# Now we're going to do something different.
# We're going to generate what are called skipgrams.
# A skipgram "skips" over a specified number of tokens.
# We'll generate skipgrams 2 to 3 tokens long and skipping over
# 0 or 1 tokens. Thus, we'll generate a series of phrases, 2 to 3 tokens in length.
us_grams <- tokens_skipgrams(us_tokens, n=2:3, skip=0:1, concatenator = " ")
uk_grams <- tokens_skipgrams(uk_tokens, n=2:3, skip=0:1, concatenator = " ")

# You can see what this data looks like:
us_grams["ENG.G0.03.2_tagged.txt"]

# For our purposes, we don't need all of these.
# So we want to begin culling our tokens.
# First we know that we're only interested in those ngrams ending with
# "that" or "which". So we can first identify those.
us_grams <- tokens_select(us_grams, "that_\\S+$|which_\\S+$", selection = "keep", valuetype = "regex", case_insensitive = T)

# We've gone from thousands to just a few hundred.
us_grams["ENG.G0.03.2_tagged.txt"]

# However, we need to sort further. Our tokens of interest can appear
# in a variety of contexts. For example, "that" frequently appears following
# a verb of thinking or speaking, as it does here:
# "has_VHZ discussed_VVN that_CST"
#
# We only want those instances where "that" or "which" is modifying a noun,
# as in this example:
# "belief_NN1 that_CST" 
#
# So next, we'll select only thos ngrams that being with a word that's
# been tagged as a noun (having the _NN* tag).
us_grams <- tokens_select(us_grams, "^[a-z]+_NN\\d?", selection = "keep", valuetype = "regex", case_insensitive = T)

# Now we're down to only about 40 ngrams in this one text.
us_grams["ENG.G0.03.2_tagged.txt"]

# Finally, we want only those 3 token sequences that have a comman
# in the middle like:
# "earth_NN1 ,_, that_CST"
us_grams <- tokens_select(us_grams, "\\s[^,]+_[^,]+\\s", selection = "remove", valuetype = "regex", case_insensitive = T)

# Now our text has less than ten examples and looks pretty good.
us_grams["ENG.G0.03.2_tagged.txt"]

# So let's repreat the sorting process with the UK data.
uk_grams <- tokens_select(uk_grams, "that_\\S+$|which_\\S+$", selection = "keep", valuetype = "regex", case_insensitive = T)
uk_grams <- tokens_select(uk_grams, "^[a-z]+_NN\\d?", selection = "keep", valuetype = "regex", case_insensitive = T)
uk_grams <- tokens_select(uk_grams, "\\s[^,]+_[^,]+\\s", selection = "remove", valuetype = "regex", case_insensitive = T)

# And we can clean up our environment a little.
rm(us_df, uk_df, us_corpus, uk_corpus, us_tokens, uk_tokens, uk_files_list, us_files_list)

# Now let's convert our data structure to a data frame.
us_grams <- data.frame(feature = unlist(us_grams), stringsAsFactors = F)

# We're going to follow Brezina's recommendation on pg. 122 for stucturing
# and idenfiying our variables using some prefixing.
us_grams <- us_grams %>%
  rownames_to_column("text_id") %>%
  mutate(text_id = str_replace(text_id, "_\\S+$", "")) %>%
  mutate(comma_sep = ifelse(str_detect(feature, ",") == T, "B_yes", "A_no")) %>%
  mutate(rel_type = ifelse(str_detect(feature, "that_") == T, "A_that", "B_which"))

# See what we've made.
View(us_grams)

# Now let's join some metadata. We'll select the speaker_status variable.
# Again, we'll clean it using some prefixing. Finally, we'll
# add a column that identifyies the location as being in US.
us_grams <- us_grams %>% 
  left_join(select(micusp_meta, text_id, speaker_status), by = "text_id") %>%
  mutate(speaker_status = str_replace(speaker_status, "NNS", "B_NNS")) %>%
  mutate(speaker_status = str_replace(speaker_status, "^NS$", "A_NS")) %>%
  mutate(nat_id = "A_US")

# See what we've made.
View(us_grams)

# The US data looks good. So let's repeat the process for the UK data.
uk_grams <- data.frame(feature = unlist(uk_grams), stringsAsFactors = F)

uk_grams <- uk_grams %>%
  rownames_to_column("text_id") %>%
  mutate(text_id = str_replace(text_id, "_\\S+$", "")) %>%
  mutate(comma_sep = ifelse(str_detect(feature, ",") == T, "B_yes", "A_no")) %>%
  mutate(rel_type = ifelse(str_detect(feature, "that_") == T, "A_that", "B_which"))

# The UK data doesn't have a speaker_status column but it does have a first_language
# column. We can make a column that matches the US data using ifelse().
uk_grams <- uk_grams %>% 
  left_join(select(bawe_meta, text_id, first_language), by = "text_id") %>%
  mutate(first_language = ifelse(str_detect(first_language, "English") == T, "A_NS", "B_NNS")) %>%
  rename(speaker_status = first_language) %>%
  mutate(nat_id = "B_UK")

# See what we've made.
View(uk_grams)

# Now we can combine the two tables.
rel_data <- bind_rows(us_grams, uk_grams)

# Finally, to run our regression model, we need to convert some character columns
# into factors (or categorical variables).
rel_data <- rel_data %>%
  mutate_at(3:6, factor)

# To understand how to set up the model, it might help to refer to pg. 119.
# Our outcome variable is that/which or the rel_type column.
# For logistic regression, this must be binary.
# For our first model, we'll set up 3 prector variables:
# - comma_sep: whether or not a comma appears between the noun and the relative pronoun
# - nat_id: whether the student is in the US or the UK
# - speaker_status: whether the student is a native speaker of English or not.
glm_fit <- glm(rel_type ~ comma_sep + nat_id + speaker_status, data = rel_data, family = "binomial")

# See the results.
summary(glm_fit)

# We can calculate the odds ratio by exponentiating the coefficients (or log odds).
# For example on pg. 125 of Brezina, he shows an estimate of 6.802 for
# "Context_type B_determined". If we exponentiate that value:

exp(6.802)

# We get odd roughly equal to 900, as Brezina's table shows.
#
# So here we can calculate our odds ratios and our confidence intervals:
exp(cbind(OR = coef(glm_fit), confint(glm_fit)))

##
# Consult Brezina pg. 125 and interpret the summary
##

# While no exact equivalent to the R2 of linear regression exists,
# the Nagelkerke R2 index can be used to assess the model fit.
# Note that pseudo R2 have been critiqued for their lack of accuracy.
# See Brezina pg. 125
PseudoR2(glm_fit, which = "Nagelkerke")

# We can also generate what Brezina calls a C-index.
# The C-index is the area under an ROC.
# The ROC is a curve generated by plotting the true positive rate (TPR)
# against the false positive rate (FPR) at various threshold settings while
#the AUC is the area under the ROC curve. As a rule of thumb, a model with
#good predictive ability should have an AUC closer to 1 (1 is ideal)
# than to 0.5. These can be calculed and plutted using packages like ROCR.
# You can find examples of the plots and the resulting AUC values as in this one:
# https://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/
#
# For our purposes we'll just use the Cstat() function from DescTools.
Cstat(glm_fit)

##
# So what would you do next to our model? How might you improve it?
##

# Let's trying making another model. First, a little tidying.
rm(bawe_meta, glm_fit, rel_data, uk_grams, us_grams, uk_sample, us_sample)

# This time, we'll just use the untaggged MICUSP data.
doc_df <- readtext_lite(micusp_meta$file_path)

# Convert those into a quanteda corpus object.
micusp_corpus <- corpus(doc_df)

# Tokenize that corpus.
# Note that our choices are a little different here.
# We're leaving in punctuation and numbers.
# This is because we'll be doing something a little different
# with this tokens object.
micusp_tokens <- tokens(micusp_corpus, include_docvars=T, remove_punct = F,
                        remove_numbers = F, remove_symbols = T, what = "word")

# Now we load in a dictionary.
# This dictionary has almost 15,000 entries. It is organized
# into only 2 categories: phrases that communicate high confidence
# and those that communicate hedged confidence. Go back and
# look at the Hyland article on stance and engagement for more
# on the importance pof these kinds of features to academic writing.
hb_dict <- dictionary(file = "dictionaries/hedges_boosters.yml")

# Next we create a new tokens object from our original one.
# By using tokens_lookup() with our dicitonary, we will create
# groupings based on our dictionary.
# Note that our dictionary has only 1 level.
# But if we can a more complex taxonomy, we can specify
# which level of the taxonomy we'd like to group our tokens under.
hb_tokens <- tokens_lookup(micusp_tokens, dictionary = hb_dict, levels = 1)

# Now we create a new document features matrix.
# And we're going to convert it to a data frame that we can use later.
hb_dfm <- dfm(hb_tokens)
hb_dataframe <- convert(hb_dfm, to = "data.frame")


# Check the frequencies.
textstat_frequency(hb_dfm)

# To normalize our counts -- 
# by total counts of tokens or sentences in each text.
# This information can be retrieved from our tokens and corpus.
# Note, however, that the nsentence is APPROXIMATE!!!
# That count comes from untokenized data. If we wanted to be 
# more precisice, we would need to tokenize the corpus by sentence
# and retrieve the sentence count from that.
ntoken(micusp_tokens)
nsentence(micusp_corpus)


# You can create multiple new columns at once with mutate()
# by chaining together new column definitions with commas.

# First we'll add columns with total tokens and total sentences.
# Then, we'll normalize using those counts.
# And we'll normalize per 10,000 words...

hb_dataframe <- hb_dataframe %>% 
  mutate(
    tokens_total = ntoken(micusp_tokens),
    sentences_total = nsentence(micusp_corpus),
    hedges_norm = (confidencehedged/tokens_total)*100,
    boosters_norm = (confidencehigh/tokens_total)*100,
  )

# Let's clean up our workspace a little.
rm(doc_df, micusp_corpus,micusp_tokens, hb_dict)

# And check out data frame.
View(hb_dataframe)

# Logistic regression has some prerequisites and assumptions.
# One of which is there is no colinearity between predictors.
# Before proceeding, lets check the correlation between frequencies
# of hedges and boosters.
cor(hb_dataframe$hedges_norm, hb_dataframe$boosters_norm)

##
# How does this look? Check Brezina pg. 121.
##

# Now lets check some distributions.
# First we'll create a data structure for ggplot.
hb_df <- hb_dataframe %>% 
  select(hedges_norm, boosters_norm) %>% 
  gather(confidence, freq_norm)

# Now plot histograms.
ggplot(hb_df,aes(x = freq_norm, color = confidence, fill = confidence)) + 
  geom_histogram(bins = 8, alpha=.5, position = "identity") +
  theme_classic() +
  theme(axis.text = element_text(size=5)) +
  facet_wrap(~ confidence)

# And boxplots.
ggplot(hb_df,aes(x = confidence, y = freq_norm)) + 
  geom_boxplot() +
  theme_classic()

##
# How do these look to you?
##

# Now let's create some data for our regression models.
# For this, we'll combine our frequency counts with some metadata:
# discipline category, speaker status, gender, and paper type.
# We'll also move the text_id to row names to exclude that column from
# further processing.
lr_df <- hb_dataframe %>% 
  mutate(document = str_remove_all(document, ".txt")) %>% 
  select(document, hedges_norm, boosters_norm) %>% 
  rename(text_id = document) %>%
  left_join(select(micusp_meta, text_id, discipline_cat, speaker_status, student_gender, paper_type), by ="text_id") %>% 
  remove_rownames %>% column_to_rownames(var="text_id")

# For the mulinomial regression, we're going to want to collapse all of
# the discipline categories into 3: Science, Humanities, and Social Science.
lr_df$discipline_cat <- str_replace_all(lr_df$discipline_cat, "BIO|CEE|ECO|IOE|MEC|NRE|PHY", "SCI")
lr_df$discipline_cat <- str_replace_all(lr_df$discipline_cat, "CLS|ENG|HIS|PHI", "HUM")
lr_df$discipline_cat <- str_replace_all(lr_df$discipline_cat, "ECO|EDU|LIN|NUR|POL|PSY|SOC", "SOCSCI")

# To carry out our regression, we need to convert our character columns
# to factors. In other words, they need to be treated like categories not strings.
# We can do them all with one simple line of code.
lr_df <- lr_df %>%  mutate_if(is.character, as.factor)

# We'll start with student gender as our outcome variable and
# hedges and boosters as our predictors. And not the "family" arguement
# specifies logistic regression.
glm_fit <- glm(student_gender ~ boosters_norm + hedges_norm, data = lr_df, family = "binomial")

# See the results.
summary(glm_fit)

# Get our odds ratios and confidence intervals.
exp(cbind(OR = coef(glm_fit), confint(glm_fit)))

##
# Consult Brezina pg. 125 and interpret the summary
##

# While no exact equivalent to the R2 of linear regression exists,
# the Nagelkerke R2 index can be used to assess the model fit.
# See Brezina pg. 125
PseudoR2(glm_fit, which = "Nagelkerke")

# And the C-index.
Cstat(glm_fit)


# Let's repeat this processess with a subset of our data.
# We have 3 discipline categories, so let's subset out only 2.
lr_sub <- lr_df %>% filter(discipline_cat == "HUM" | discipline_cat == "SCI")
lr_sub$discipline_cat <- droplevels(lr_sub$discipline_cat)

glm_fit <- glm(discipline_cat ~ boosters_norm + hedges_norm, data = lr_sub, family = "binomial")

summary(glm_fit)

##
# Consult Brezina pg. 125 and interpret the summary
##

# odds ratios and 95% CI
exp(cbind(OR = coef(glm_fit), confint(glm_fit)))

# Get our pseudo R2.
PseudoR2(glm_fit, which = "Nagelkerke")

# And our C-index.
Cstat(glm_fit)

# What if we add speaker_status to our model?
glm_fit <- glm(discipline_cat ~ boosters_norm + hedges_norm + speaker_status, data = lr_sub, family = "binomial")

##
# Is this a better or worse perfoming model?
##

# Now let's try multinomial regression on all 3 of the discipline categories.
# This isn't covered in the textbook, but it's worth looking at even if briefly.
mr_fit <- multinom(discipline_cat ~ boosters_norm + hedges_norm, data = lr_df)

# We first see that some output is generated by running the model,
# even though we are assigning the model to a new R object.
# This model-running output includes some iteration history and includes
# the final  log-likelihood 814.275450. This value multiplied by two 
# is then seen in the model summary as the Residual Deviance and it can be 
# used in comparisons of nested models.

summary(mr_fit)

# Let's look at a couple of boxplots to give us some context for these numbers.
ggplot(lr_df, aes(x = discipline_cat, y = boosters_norm)) +
  geom_boxplot() +
  theme_classic()

ggplot(lr_df, aes(x = discipline_cat, y = hedges_norm)) +
  geom_boxplot() +
  theme_classic()


# Much like logistic regression, th ratio of the probability of choosing one 
# outcome category over the probability of choosing the baseline category is t
# he relative risk or odds. The relative risk is the right-hand side linear equation
# exponentiated, leading to the fact that the exponentiated regression
# coefficients are relative risk ratios for a unit change in the predictor
# variable. We can exponentiate the coefficients from our model to see
# these odds ratios.

exp(coef(mr_fit))

# Sometimes a plot can be helpful in interpreting the results.
# Let's start by making some dummy data.
# For this we'll sequence frequencies of hedges from 0 to 6 percent of a text,
# and frequencies of boosters on an inverse scale: from 6 to 0 percent of a text.
# In essence, we creating hypothetical texts that have at one end
# have low frequencies of hedges and high frequencies of boosters,
# have balanced frequencies in the middle,
# and have high frequencies of hedges and low frequencies of boosters.
hb_new <- data.frame(hedges_norm = seq(0, 6, by = .1), 
                     boosters_norm = seq(6, 0, by = -.1))

# Next, we create a data frome of discipline probablities based on our fit.
prob_disc <- cbind(hb_new, predict(mr_fit, newdata = hb_new, type = "probs", se = TRUE))

# We'll format a data frame for plotting.
plot_prob <- prob_disc %>% 
  gather(key = "feature", value = "confidence", hedges_norm, boosters_norm) %>% 
  gather(key = "variable", value = "probability", HUM, SCI, SOCSCI)

# Finally, we'll create a plot the probabilities and color by hedges & boosters.
ggplot(plot_prob, aes(x = confidence, y = probability, color = feature)) + geom_line() + 
  theme_classic() +
  facet_grid(variable ~ ., scales = "free")

##
# What does the plot suggest about hedging and boosting in the 
# disciplinary categories?
##

