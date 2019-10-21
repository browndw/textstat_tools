
library(tidyverse)

# For this excercise, we going to make use two packages that can help
# us plot and manipulate dendrograms...
library(dendextend)
library(ggdendro)

# First, we'll import some functions that will help us work with
# time series or diachronic data...
source("functions/timeseries_functions.R")

# We won't do much with the google_ngram() function because most of Google data
# table are HUGE. Though they are formatted as simple tab-delimited text files,
# they often run in the multiple gigabytes in size. You can access them here:
# http://storage.googleapis.com/books/ngrams/books/datasetsv2.html
#
# With that in mind, we'll do a simple demo of the function with one of the smaller
# 1-gram tables: Q.
# First, we'll make a vector of the word forms we want to count.
# In this case, we'll count 3 common forms of "quiz".
wfs <- c("quiz", "quizzes", "quizzed")

# Then, we'll aggregate counts from the Ngram data. First, we'll do this
# by year
q <- google_ngram(wfs, variety = "eng", by = "year")

# There is also a wrapper function for quick plotting, which includes
# a confidence interval for idenify "peaks and troughs" in the diachronic data.
plot_year(q)

# We can also aggregate the counts by decade.
q <- google_ngram(wfs, variety = "eng", by = "decade")

# And do some quick plotting.
plot_decade(q)

# Just eyeballing the data, it looks like there might be some interesing changes
# in frequency in the middle of the 20th century, and late in the 20th century.
#
# To better understand how these changes group together (what is called "periodization")
# we can turn turn Variability-Based Neighbor clustering.
#
# First, we're just going toke data for the 20th century on.
# And, we'll select the two columns we need: our frequency counts and time.
# Finally, we'll make sure that our data is all formatted at numeric.
q <- q %>% filter(decade > 1899) %>% select(decade, counts_permil) %>%
  mutate_all(as.numeric)

# To get a sense of the clustering and how we might want to cut our dendrogram
# later, we can generate a scree plot...
vnc_scree(q)

# From the scree plot, it looks like we have 2-3 well-formed clusters...
#
# Now, we'll generate the data for our dendrogram.
# Keep in mind this a very specific implementation of heirarchical clustering
# as we need to maintain the order of our time series.
# The distance is based on standard deviations of sequential pairs of time intervals.
# Alternatively, you can set the distance.measure to "cv" for
# to use the coefficient of variation.
hc <- vnc_clust(q, distance.measure = "sd")

# Let's plot the result.
plot(hc, hang = -1)

##
# What does the result suggest to you?
##

# Now let's look at some other data: frequenies of the bigram
# "witch hunt" and the plural "witch hunts".
#
# These also comes from Google Books. You can gather the data yourself
# at a later time using google_ngram(), if you want, but for the purposes
# of this exercise we'll skip that stop to save time.
wh_d <- read_csv("data/time_data/witch_hunt_decade.csv")

# Again, we'll filter the data and select the columns we need.
wh_d <- wh_d %>% filter(decade > 1899) %>% select(decade, counts_permil)

# Generate a scree plot...
vnc_scree(wh_d)

##
# How does that look to you?
##

# And the cluster data...
hc <- vnc_clust(wh_d, distance.measure = "sd")

# Finally, the dondrogram...
plot(hc, hang = -1)

##
# This dendrogram is VERY similar to the one we produced for
# lemmatized "quiz"... Any guesses as to why these tokens
# would periodize in similar ways?
##

# For the next step, we'll cut our dendrogram into 2 clusters.
# Note that we're storing the output into an object "cut_hc".
cut_hc <- rect.hclust(hc, k=2)

# We can now use that output to create a new new data.frame
# called "clust_df".
clust_df <- data.frame(decade=as.numeric(names(unlist(cut_hc))),
  clust=rep(c(paste0("clust_", seq(1:length(cut_hc)))),
  times=sapply(cut_hc,length)))

# Look at the output.
View(clust_df)

# We can now join that data to our original "witch hunt" data.
# This will be useful for later plotting.
clust_df <- clust_df %>% right_join(wh_d, by = "decade")

# We've already plotting our data with base R.
# However, if we want more control, we want to plot in ggplot2.
# To do that, we need to go through a couple of intermediate steps.
#
# First, we'll convert our cluster data into dendrogram data
# using as.dendrogram(). We also MUST maintain the order of our time series.
# There are a variety of ways of doing this, but dendextend has an
# easy function called sort(). We'll take the easy way!
dend <- as.dendrogram(hc) %>% sort

# To get ggplot-friendly data, we have to transform it yet again...
# This time using the ggdendro package's function dendro_data()
dend_data <- dendro_data(dend, type = "rectangle")

# Now let's do some fancy plotting!
# We're going to combine the dendrogram and a time series scatter plot
# like Gries and Hilpert do on pg. 140 of their chapter on VNC.
#
# The first three lines pull data from "clust_df" for the scatter plot
# using the clusters to color each point according to group.
# The geom_segment pulls data from "dend_data" to build the dendrogram.
# For the tick marks we again pull from "dend_data" using the "x"
# column for the breaks and and the "label" column to label the breaks.
ggplot(clust_df, aes(x = as.numeric(rownames(clust_df)), y = counts_permil)) +
  geom_line(linetype = "dotted") +
  geom_point(aes(color = clust), size = 2) +
  geom_segment(data = dend_data$segments, aes(x = x, y = y, xend = xend, yend = yend))+
  scale_x_continuous(breaks = dend_data$labels$x,
    labels=as.character(dend_data$labels$label)) +
  xlab("") + ylab("frequency (per million words") +
  theme_minimal()

##
# The resulting plot highlights both the trajectory of variable
# and the clustering according to period.
# If you wanted to make it even nicer, you might adjust the
# color scheme and the legend.
##

# Let's repeat this process, but with more granular data.
# Again, we'll examine lemmatized "witch hunt", but with
# frequencies by year rather than by decade.
wh_y <- read_csv("data/time_data/witch_hunt_year.csv")

# And we'll filter out the data we want...
wh_y <- wh_y %>% filter(year > 1899) %>% select(year, counts_permil)

# For our by-year data, we have to add a step.
# Look at the first 5 rows... What do you notice?
wh_y[1:5,]

# For VNC to work, we can't have any missing data.
# So we need to fill in those missing years with frequencies of 0.
# We start by generating a sequence of all the years we need.
yrs <- data.frame(year = as.numeric(seq(1900, 2009)))

# Now we join that complete sequence to our existing data.
# The result will produce NAs. So we'll convert those NAs to 0s.
wh_y <- left_join(yrs, wh_y, by = "year") %>%
  mutate_at(vars(counts_permil), ~replace(., is.na(.), 0))

# Now look at the first 5 rows... Better!
wh_y[1:5,]

# Let's proceed with a scree plot. Lot's of data, so messier...
vnc_scree(wh_y)

# And our cluster data...
hc <- vnc_clust(wh_y)

# And generate a dendrogram...
plot(hc, hang = -1)

##
# It's harder to see because of the number of clades,
# but does anything get your attention?
#

# For now, we'll cut our dendrogram into 8 clusters...
cut_hc <- rect.hclust(hc, k=8)

# And use the result to create a new data.frame for plotting.
clust_df <- data.frame(year=as.numeric(names(unlist(cut_hc))),
                       clust=rep(c(paste0("clust_", seq(1:length(cut_hc)))),
                       times=sapply(cut_hc,length)))

# We'll join the result to your original data.
clust_df <- clust_df %>% right_join(wh_y, by = "year")

# Convert our hc data to dendrogram data.
dend <- as.dendrogram(hc) %>% sort

# And make that more friendly for ggplot..
dend_data <- dendro_data(dend, type = "rectangle")

# And generate our plot. This time, we'll use rcolorbrewer for our
# color palette.
ggplot(clust_df, aes(x = as.numeric(rownames(clust_df)), y = counts_permil)) +
  geom_line(linetype = "dotted") +
  geom_point(aes(color = clust), size = 2) +
  scale_color_brewer(palette="Set1") +
  geom_segment(data = dend_data$segments, aes(x = x, y = y, xend = xend, yend = yend))+
  scale_x_continuous(breaks = dend_data$labels$x[seq(1, length(dend_data$labels$x), 5)],
                     labels=as.character(dend_data$labels$label)[seq(1, length(dend_data$labels$x), 5)]) +
  xlab("") + ylab("frequency (per million words") +
  theme_minimal()

##
# Can you explain the results that you see in the plot?
##


