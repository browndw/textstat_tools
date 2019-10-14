
library(quanteda)
library(tidyverse)
library(spacyr)
library(cluster)
library(factoextra)

# Our clustering will be based on pos counts, so we need to initialize our spcacy model.
spacy_initialize(model = "en")

source("functions/helper_functions.R")

# We'll get the paths to the mini MICUSP corpus.
micusp_paths <- list.files("data/text_data/micusp_mini", full.names = TRUE, pattern = "*.txt")

# And we'll subset out the Biology and English papers.
paths_sub <- micusp_paths %>% str_detect("ENG|BIO") %>% keep(micusp_paths, .)

# Finally, we'll create our data.frame of texts and doc_ids
sub_df <- readtext_lite(paths_sub)

# We going to use regex to remove all parenthetical references,
# which will help with the accuracy of our parsing.
sub_df$text <- gsub("\\s\\(([^()\\d]*\\d[^()]*)\\)", "", sub_df$text, perl = T)

# And create a corpus object.
sub_corpus <- corpus(sub_df)

# Next, wee're going to use spacy to parse the corpus.
# Note that we can add a dependency column to our parsing.
# We'll do it here so you can see what it looks like.
sub_prsd <- spacy_parse(sub_corpus, pos = T, tag = T, dependency = T, entity = F)

View(sub_prsd)

# Now, we're going to do something new.
# We're going to remove any spaces, and then combine our pos and tag columns.
sub_prsd <- sub_prsd  %>% filter(pos != "SPACE") %>% 
  unite("pos", pos:tag)

# Next we create a named list from the new, concatenated column.
sub_tokens <- split(sub_prsd$pos, sub_prsd$doc_id)

# See what the result looks like.
sub_tokens

# Now, we'll use that as our tokens object.
sub_tokens <- as.tokens(sub_tokens)

# From that, we'll generate a dfm.
sub_dfm <- dfm(sub_tokens)

# We'll weight the raw counts.
sub_dfm <- dfm_weight(sub_dfm, scheme = "prop")

# And convert the result to a data.frame.
sub_dfm <- convert(sub_dfm, to = "data.frame") %>%
  rename(doc_id = document)

# Finally, we're going to convert the first row (doc_id) into row names.
# And, for fun, we'll order our columns alphabetically.
sub_dfm <- sub_dfm %>% column_to_rownames("doc_id") %>% 
  select(order(colnames(.)))

# View the result.
View(sub_dfm)

# As we did with factor analysis, we'll scale our variables.
df <- data.frame(scale(sub_dfm))

# Scaling the variables transforms them such that they have a mean of roughly zero,
# and a standard deviation of 1. See Brezina pg. 152-153.
# We can chck the noun column, for example.
round(mean(df$noun_nn), 5)
sd(df$noun_nn)

# We can use some base R functions to create our dendrogram from the following steps.
# First, we need to create a difference matrix based on distances.
# The two most common distance measures are "euclidean" and "manhattan",
# which are described on pg. 153.
# Note, however, that there are other options.
# We'll start with the most comman, "educidean" distance...
d <- dist(df, method = "euclidean")

# The next step is to determine the linkage method.
# Brezina details these on pg. 154-159.
# Here they are in summary:
# 
# - Maximum or complete linkage clustering: It computes all pairwise dissimilarities between
# the elements in cluster 1 and the elements in cluster 2, and considers the largest value
# (i.e., maximum value) of these dissimilarities as the distance between the two clusters.
# It tends to produce more compact clusters.
#
# - Minimum or single linkage clustering: It computes all pairwise dissimilarities between the
# elements in cluster 1 and the elements in cluster 2, and considers the smallest of these
# dissimilarities as a linkage criterion. It tends to produce long, “loose” clusters.
#
# - Mean or average linkage clustering: It computes all pairwise dissimilarities between the
# elements in cluster 1 and the elements in cluster 2, and considers the average of these
# dissimilarities as the distance between the two clusters.
#
# - Centroid linkage clustering: It computes the dissimilarity between the centroid for
# cluster 1 (a mean vector of length p variables) and the centroid for cluster 2.
#
# - Ward’s minimum variance method: It minimizes the total within-cluster variance. At each
# step the pair of clusters with minimum between-cluster distance are merged.

# We'll carry out the hierarchical clustering using Ward...
hc <- hclust(d, method = "ward.D2" )

# And plot the dendrogram...
plot(hc, cex = 0.6, hang = -1)

# We can follow these same steps using functions from the cluster package, too.
# These provide us with a few additional options, like get_dist()...
?get_dist

# Which we'll use to ceate a dissimilarity matrix.
d <- get_dist(df)

# Now let's visualize that matrix.
fviz_dist(d, gradient = list(low = "tomato", mid = "white", high = "steelblue"))

# We create our plot using the agnes() function, this time.
hc <- agnes(d, method = "ward" )

# And plot the results.
plot(as.hclust(hc), cex = 0.6, hang = -1)

# The agnes() function also computes an agglomerative coefficient, 
# which measures the amount of clustering structure found 
# (values closer to 1 suggest strong clustering structure).
hc$ac

# To see how these change with different linkage methods,
# We can create a vector.
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# And a simple function.
ac <- function(x) {
  agnes(d, method = x)$ac
}

# Then, we map that function onto the vector to see how our
# clustering would be affected...
map_dbl(m, ac)

# We can also "cut" our dendrogram in any number of clusters.
# The question is: How many clusters are optimal.
#
# Here, we can use some plotting functions that are part of the factoextra package.
# The first is the familiar "elbow" method.
fviz_nbclust(df, FUN = hcut, method = "wss")

# With our data, the result isn't particularly helpful.
# We can then try the "silhouette" methods.
# The average silhouette approach measures the quality of a clustering.
# That is, it determines how well each object lies within its cluster.
# A high average silhouette width indicates a good clustering.
# The average silhouette method computes the average silhouette of observations for different values of k.
# The optimal number of clusters k is the one that maximizes the average
# silhouette over a range of possible values for k.
fviz_nbclust(df, FUN = hcut, method = "silhouette")

# We can use the result to choose how we want to "cut" our dendrogram.
# Here we'll cut it into two clusters.
plot(as.hclust(hc), cex = 0.6, hang = -1)
rect.hclust(hc, k = 2)

# Although it's not covered in Brezina, another very common clustering method is called k-means.
# The basic idea behind k-means clustering consists of defining clusters 
# so that the total intra-cluster variation (known as total within-cluster variation) is minimized.
#
# The k-means algorithm can be summarized as follows:
# By the analyst:
# - Specify the number of clusters (k) to be created
# By the algorithm:
# - Select randomly k objects from the data set as the initial cluster centers or means
# - Assign each observation to their closest centroid, based on the Euclidean distance between the object and the centroid
# - For each of the k clusters update the cluster centroid by calculating the new mean values of all the data points in the cluster. The centroid of a kth cluster is a vector of length p containing the means of all variables for the observations in the kth cluster; p is the number of variables.
# - Iteratively minimize the total within sum-of-squares. That is, iterate steps 3 and 4 until the cluster assignments stop changing or the maximum number of iterations is reached. By default, the R software uses 10 as the default value for the maximum number of iterations.

# We've already determined that our data is best divided into 2 clusters.
# So we specify "centers" to be 2.
km <- kmeans(df, centers = 2, nstart = 25)

# Note that we can access important information about our clusters.
km

# For example, we can return the wint sum-of-squares...
km$withinss

# Or the between sum-of-squares...
km$betweenss

# Plotting the result is easy with fviz_cluster()...
fviz_cluster(km, data = df)

# Of course, you can adjust the ggplot options to make it a little more
# visually appealing than the default...


