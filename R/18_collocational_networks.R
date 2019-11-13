
library(tidyverse)
library(quanteda)
library(tidygraph) # for creating network data objects
library(ggraph) # for plotting ggplot2 networks

# Load the functions that we'll need.
source("functions/helper_functions.R")
source("functions/collocations_functions.R")

# For this exercise, we'll be working with screenplays. So first the metadata...
meta <- read_csv("data/meta_data/movie_meta.csv")

# From that, we'll select out movies that have "Comedy" and "Romance" in their
# genre discriptions, but not "Action". We did this once before...
romcom_movies <- meta %>% filter(str_detect(genres, "Comedy")) %>%
  filter(str_detect(genres, "Romance")) %>%
  filter(!str_detect(genres, "Action"))

# And we'll subset out movies that have "Action" in their genre descriptions
# but not "Romance" or "Comedy".
act_movies <- meta %>% filter(str_detect(genres, "Action")) %>%
  filter(!str_detect(genres, "Romance")) %>%
  filter(!str_detect(genres, "Comedy"))

# From those subsets, we'll sample 25 screenplays from each category.
set.seed(123)
act_sample <- sample(act_movies$file_path, 25)
romcom_sample <- sample (romcom_movies$file_path, 25)

# And we'll read in the direction from those screenplays into two data.frames.
# Direction includes descriptions of scenes and characters.
df_act <- readplay(act_sample, extract = "direction")
df_romcom <- readplay(romcom_sample, extract = "direction")

# We'll create two copora...
act_corpus <- corpus(df_act)
romcom_corpus <- corpus(df_romcom)

# And we'll do some very quick tokenizing...
act_tokens1 <- tokens(act_corpus, what = "word", remove_punct = T)
romcom_tokens1 <- tokens(romcom_corpus, what = "word", remove_punct = T)

# From those, we'll retrieve collocations for the word "girl" from romcoms
# with a window of 5 tokens to the left and right.
girl_romcom <- collocates_by_MI(romcom_tokens1, "girl", 5, 5)

# Then, we'll filter those at thresholds of 3 for both Mutual Information
# and frequency.
girl_romcom <- girl_romcom %>% filter(col_freq >= 3 & MI_1 > 3)

# Now we can create a network object from the collocations data.frame.
# This function operationalizes the idea of collcational networks described by
# Brezina, McEnery & Wattam (2015):
# https://www.jbe-platform.com/content/journals/10.1075/ijcl.20.2.01bre
#
# The function takes data.frames produced by the collocates_by_MI() function
# and generates a tidygraph data object for plotting in ggraph.
net <- col_network(girl_romcom)

# Examine the object and the columns for Node Data and Edge Data.
# If you don't know much about networks in R, there is a wonderful tutorial here:
# https://kateto.net/networks-r-igraph
net

# The plot has the node word ("girl") at its center and links to the collocations.
# The collocates are located by their strength of association (as calculated by MI).
# In this case tokens like "young" have a stronger association with "girl" than "looks".
# The nodes are also shaded by their normalized frequencies assigned by: "alpha = node_weight"
ggraph(net, weight = link_weight, layout = "stress") + 
  geom_edge_link(color = "gray80", alpha = .75) + 
  geom_node_point(aes(alpha = node_weight, size = 3, color = n_intersects)) +
  geom_node_text(aes(label = label), repel = T) +
  scale_alpha(range = c(0.2, 0.8)) +
  theme_graph() +
  theme(legend.position="none")

# Let's repeat this process for the word "boy".
boy_romcom <- collocates_by_MI(romcom_tokens1, "boy", 5, 5)
boy_romcom <- boy_romcom %>% filter(col_freq >= 3 & MI_1 > 3)

# This time we'll feed both sets of collocates into the newwork...
net <- col_network(girl_romcom, boy_romcom)

# And plot the results...
# This time we have tokens that collocate with BOTH "girl" and "boy".
# These appear in blue in the middle of the plot.
ggraph(net, weight = link_weight, layout = "stress") + 
  geom_edge_link(color = "gray80", alpha = .75) + 
  geom_node_point(aes(alpha = node_weight, size = 3, color = n_intersects)) +
  geom_node_text(aes(label = label), repel = T) +
  scale_alpha(range = c(0.2, 0.8)) +
  theme_graph() +
  theme(legend.position="none")

###
# Now, create a similar plot for "boy and "girl"
# but using the tokens from action movies (act_tokens1)
##

# While the results seem potentially interesting, there are tokens
# in our plot that do not seem particularly explanatory on their face (like "the").
#
# For more targeted collocations, we can part-of-speech tag our data.
# Let's initialize spacy.
spacyr::spacy_initialize()

# And parse our corpus... This will take a minute...
romcom_tokens <- spacy_parse(romcom_corpus, pos = FALSE, tag = TRUE)

# Now we'll convert that into a tokens object that quanteda understands
# And filter our tokens...
romcom_tokens <- as.tokens(romcom_tokens, include_pos = "tag", concatenator = "_")
romcom_tokens <- tokens_select(romcom_tokens, "_[A-Z]", selection = "keep", valuetype = "regex", case_insensitive = T)
romcom_tokens <- tokens_select(romcom_tokens, "\\W_", selection = "remove", valuetype = "regex")
romcom_tokens <- tokens_select(romcom_tokens, "\\d_", selection = "remove", valuetype = "regex")

# While we're at it, we'll parse the action movie corpus, as well...
act_tokens <- spacy_parse(act_corpus, pos = FALSE, tag = TRUE)

# And convert and filter those tokens....
act_tokens <- as.tokens(act_tokens, include_pos = "tag", concatenator = "_")
act_tokens <- tokens_select(act_tokens, "_[A-Z]", selection = "keep", valuetype = "regex", case_insensitive = T)
act_tokens <- tokens_select(act_tokens, "\\W_", selection = "remove", valuetype = "regex")
act_tokens <- tokens_select(act_tokens, "\\d_", selection = "remove", valuetype = "regex")

# Now we that we have tagged data we can target our search and filter our resutls
# more precisely. We'll begin by gathering collocates for the subject pronouns
# "she" and "he". We'll also specify that we're looking ONLY 5 to RIGHT of our node words.
she_romcom <- collocates_by_MI(romcom_tokens, "she_PRP", 0, 5)
he_romcom <- collocates_by_MI(romcom_tokens, "he_PRP", 0, 5)

# Again, we'll filter these...
she_romcom <- she_romcom %>% filter(col_freq >= 3 & MI_1 > 3)
he_romcom <- he_romcom %>% filter(col_freq >= 3 & MI_1 > 3)

# But now (!) we're only going to select tokens that have been tagged as adjectives...
she_romcom_j <- she_romcom %>% filter(str_detect(feature, "_j"))
he_romcom_j <- he_romcom %>% filter(str_detect(feature, "_j"))

# View the results
she_romcom_j %>% as_tibble() %>% head(10)

# Note: at this point you could easily regex out the *_jj tags.
# We'll leave them in for now...

# We'll create our network object...
net <- col_network(she_romcom_j, he_romcom_j)

# And plot the results...
ggraph(net, weight = link_weight, layout = "stress") + 
  geom_edge_link(color = "gray80", alpha = .75) + 
  geom_node_point(aes(alpha = node_weight, size = 3, color = n_intersects)) +
  geom_node_text(aes(label = label), repel = T) +
  scale_alpha(range = c(0.2, 0.8)) +
  theme_graph() +
  theme(legend.position="none")

##
# What patterns does the plot suggest?
##

# As one final illustration, we'll calculate collocates for "she" from that action movies.
she_act <- collocates_by_MI(act_tokens, "she_PRP", 0, 5)
# Filter them...
she_act <- she_act %>% filter(col_freq >= 3 & MI_1 > 3)
# And select the adjectives...
she_act_j <- she_act %>% filter(str_detect(feature, "_j"))

# We'll put these into a network object with the "she" romcom collocates..
net <- col_network(she_act_j, she_romcom_j)

# And plot the results...
ggraph(net, weight = link_weight, layout = "stress") + 
  geom_edge_link(color = "gray80", alpha = .75) + 
  geom_node_point(aes(alpha = node_weight, size = 3, color = n_intersects)) +
  geom_node_text(aes(label = label), repel = T) +
  scale_alpha(range = c(0.2, 0.8)) +
  theme_graph() +
  theme(legend.position="none")

##
# Again, what is suggested by the pattern you see?
##

# Finally, we can check our collocates in context by calling
# the kwic() function (Key Words in Context) and filering it for
# adjective tags in the 5 words after our node word.
kwic(romcom_tokens, "she_PRP", 5) %>% as_tibble() %>%
  filter(str_detect(post, "_JJ"))
