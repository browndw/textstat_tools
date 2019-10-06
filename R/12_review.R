require(xml2)
library(tidyverse)
library(quanteda)


# The Skakespeare files have been formatted to make the dialegue and direction
# easy to extract. If you were to open one of the files, you'd finde
# very simple markup. To give you sense of what you can do, let's read in Hamlet
# using the read_html function in the xml2 package
hamlet <- read_html("data/text_data/shakespeare_corpus/tragedies_hamlet_prince_of_denmark.txt")

# If we wanted to extract all of Hamlet's dialogue, for example,
# We would use a series of xml2 functions, specifically, xml_find_all()
# and xml() text. And we could then convert the result into a data.frame.
text_data <- hamlet %>% xml_find_all("//hamlet//dialogue")  %>% 
  xml_text() %>% str_squish() %>% data.frame(text = ., stringsAsFactors = F)

View (text_data)

# Or we could extract all of Hamlet's dialogue from Act I as follows:
text_data <- hamlet %>% xml_find_all("//act_1//hamlet//dialogue")  %>% 
  xml_text() %>% str_squish() %>% data.frame(text = ., stringsAsFactors = F)

# Or we extract the stage direction for Act I like this:
text_data <- hamlet %>% xml_find_all("//act_1//direction")  %>% 
  xml_text() %>% str_squish() %>% data.frame(text = ., stringsAsFactors = F)

rm(hamlet, text_data)

# Let's load our functions.
source("functions/helper_functions.R")
source("functions/keyness_functions.R")
# We can also use a function in our repository called readplay()
# to extract data from multiple files and put them into a quaneda-friedly data.frame.
#
# Let's start by getting the paths to all of our Shakespeare plays:
files_list <- list.files("data/text_data/shakespeare_corpus/", full.names = T, pattern = "*.txt")

# Note that the files are prefixed by their type:
# - comedies
# - histororical
# - tragedies
# So we can use str_detect() to subset our files to just include comedies.
comedies <- files_list[str_detect(files_list, "comedies_")]

# And we'll do the same for the historical plays.
historical <- files_list[str_detect(files_list, "historical_")]

# Now we'll use the readplay() function to extract the dialogue.
df_com <- readplay(comedies, extract = "dialogue")
df_hist <- readplay(historical, extract = "dialogue")

# Convert these into two corpora...
com_corpus <- corpus(df_com)
hist_corpus <- corpus(df_hist)

# Quickly tokenize our corpora...
com_tokens <- tokens(com_corpus, what = "word", remove_punct = T)
hist_tokens <- tokens(hist_corpus, what = "word", remove_punct = T)

# Create our dfms...
com_dfm <- dfm(com_tokens)
hist_dfm <- dfm(hist_tokens)

# Clean up our environment...
rm(com_tokens, com_corpus, hist_tokens, hist_corpus, comedies, historical, df_com, df_hist)

# Check our token frequencies...
textstat_frequency(com_dfm, n = 25)

# One of the idiosyncrasies of quanteda is that to generate keywords,
# All of the data needs to be contained in a single data structure.
# To make things easier, I wrote a function that will enable you to
# generate keywords from any number of quanteda dfms.
# To see how it works, let's try keyness_pairs() with our two dfms.
key_shakes <- keyness_pairs(com_dfm, hist_dfm)

##
# What distinguishes comedies from histories and visa versa?
##

# Now let's add the tragedies...
tragedies <- files_list[str_detect(files_list, "tragedies_")]

# Proceed with our data preparation...
df_trag <- readplay(tragedies, extract = "dialogue")
trag_corpus <- corpus(df_trag)
trag_tokens <- tokens(trag_corpus, what = "word", remove_punct = T)
trag_dfm <- dfm(trag_tokens)

rm(trag_tokens, trag_corpus, tragedies, df_trag)

# And redo our keyness calculations, this time with three dfms...
key_shakes <- keyness_pairs(com_dfm, hist_dfm, trag_dfm)

View(key_shakes)

# Let's clean up and work with some different data.
rm(key_shakes, com_dfm, hist_dfm, trag_dfm, files_list)

# Now let's read in some metadata for a little more than 900 movies.
meta <- read_csv("data/meta_data/movie_meta.csv")

View(meta)

# From that, we'll select out movies that have "Comedy" and "Romance" in their
# genre discriptions, but not "Action".
romcom_movies <- meta %>% filter(str_detect(genres, "Comedy")) %>%
  filter(str_detect(genres, "Romance")) %>%
  filter(!str_detect(genres, "Action"))

# And we'll subset out movies that have "Action" in their genre descriptions
# but not "Romance" or "Comedy".
act_movies <- meta %>% filter(str_detect(genres, "Action")) %>%
  filter(!str_detect(genres, "Romance")) %>%
  filter(!str_detect(genres, "Comedy"))

# From those subsets, we'll sample 25 screenplays from each category.
set.seed(5)
act_sample <- sample(act_movies$file_path, 25)
romcom_sample <- sample (romcom_movies$file_path, 25)

# And we'll read in the dialogue from those screenplays into two data.frames.
df_act <- readplay(act_sample, extract = "dialogue")
df_romcom <- readplay(romcom_sample, extract = "dialogue")

# Again, we'll create two copora...
act_corpus <- corpus(df_act)
romcom_corpus <- corpus(df_romcom)

# Two sets of tokens...
act_tokens <- tokens(act_corpus, what = "word", remove_punct = T)
romcom_tokens <- tokens(romcom_corpus, what = "word", remove_punct = T)

# And two dfms...
act_dfm <- dfm(act_tokens)
romcom_dfm <- dfm(romcom_tokens)

# Let's check the result
textstat_frequency(act_dfm, n = 25)

# We'll generate our keyness and effect size values.
key_movies <- keyness_pairs(romcom_dfm, act_dfm)

# And see what we have.
View(key_movies)

##
# What do you find?
# From this sample distinguishes the dialogue in romcoms 
# from the dialogue in action movies and visa versa?
##

##
# If you wanted to further explore these differences,
# what other strategies might you try?
##
