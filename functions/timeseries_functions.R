
# This function extracts frequency data from Google Books' Ngram data:
# http://storage.googleapis.com/books/ngrams/books/datasetsv2.html
# The function is set up to facilitate the counting of lemmas
# and ingnore differences in capitalization.
# The user has control over what to combine into counts with
# the "word_forms argument.
#
# NOTE!!! Google's data tables are HUGE. Sometime running into
# multiple gigabytes for simple text files. Thus, depending
# on the table being accessed, the return time can be slow.
# For example, asscessing the 1-gram Q file should take only a few seconds,
# but the 1-gram T file might take 10 minutes to process.
# The 2-gram, 3-gram, etc. files are even larger and slower to process.

google_ngram <- function(word_forms, variety=c("eng", "gb", "us", "fiction"), by=c("year", "decade")){
  n <- lapply(word_forms, function(x) stringr::str_count(x, "\\w+"))
  n <- unique(n)
  if (length(n) > 1)  stop ("Check spelling. Word forms should be lemmas of the same word (e.g. 'teenager' and 'teenagers' or 'walk' , 'walks' and 'walked'")
  gram <- lapply(word_forms, function(x) substring(x, 1, n))
  gram <- tolower(unique(gram))
  if (length(gram) > 1)  stop ("Check spelling. Word forms should be lemmas of the same word (e.g. 'teenager' and 'teenagers' or 'walk' , 'walks' and 'walked'")
  
  if(variety == "eng") repo <- paste0("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-", n, "gram-20120701-", gram, ".gz")
  if(variety != "eng") repo <- paste0("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-", variety, "-all-", n, "gram-20120701-", gram, ".gz")
  
  all_grams <- suppressWarnings(readr::read_tsv(repo, col_names = FALSE, quote = ""))
  colnames(all_grams) <- c("token", "year", "token_count", "pages")
  
  if(variety == "eng") repo_total <-("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-totalcounts-20120701.txt")
  if(variety != "eng") repo_total <-paste0("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-", variety, "-all-totalcounts-20120701.txt")
  total_counts <- suppressWarnings(read.csv(repo_total, header = FALSE, sep = "\t", quote = ""))
  
  total_counts <- as.data.frame(t(total_counts))
  total_counts <- data.frame(V1 = (total_counts[!is.na(total_counts),]))
  total_counts <- data.frame(do.call("rbind", strsplit(as.character(total_counts$V1), "," ,fixed = TRUE)))
  colnames(total_counts) <- c("year", "total_count", "page_count", "volume_count")
  total_counts$total_count <- as.numeric(as.character(total_counts$total_count))
  total_counts$decade <- gsub("\\d$", "0", total_counts$year)
  if (by == "year") total_counts <- aggregate(total_count ~ year, total_counts, sum)
  if (by == "decade") total_counts <- aggregate(total_count ~ decade, total_counts, sum)
  
  grep_words <- paste0("^", word_forms, "$", collapse = "|")
  all_tokens <- subset(all_grams, grepl(grep_words, all_grams$token, ignore.case=TRUE))
  all_tokens$token <- tolower(all_tokens$token)
  sum_tokens <- aggregate(token_count ~ year, all_tokens, sum)
  sum_tokens$decade <- gsub("\\d$", "0", sum_tokens$year)
  if (by == "decade") sum_tokens <- aggregate(token_count ~ decade, sum_tokens, sum)
  if (by == "year") sum_tokens <- merge(sum_tokens, y = total_counts[,c(1:2)], by = "year")
  if (by == "decade") sum_tokens <- merge(sum_tokens, y = total_counts[,c(1:2)], by = "decade")
  counts_norm <- mapply(function(x,y) (x/y)*1000000, sum_tokens$token_count, sum_tokens$total_count)
  counts_norm <- round(counts_norm, 2)
  sum_tokens$counts_permil <- counts_norm
  return(sum_tokens)
}

# This is a simple wrapper for plotting by decade.

plot_decade <- function(ngram_df, start=1800, end=2000) {
  ngram_df$decade <- as.numeric(ngram_df$decade)
  ngram_df <- ngram_df[ which(ngram_df$decade >= start & ngram_df$decade <= end), ]
  ggplot(ngram_df, aes(x=decade, y=counts_permil)) +
    geom_bar(stat = "identity") +
    labs(x="decade", y = "frequency (per million words)")+ 
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank()) +
    theme(panel.grid.minor.y =   element_blank(),
          panel.grid.major.y =   element_line(colour = "gray",size=0.25)) +
    theme(rect = element_blank()) +
    theme(legend.title=element_blank()) +
    theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=10))
}

# This is a simple wrapper function for plotting by year, with a confidence interval.

plot_year <- function(ngram_df, start=1800, end=2000) {
  ngram_df$year <- as.numeric(ngram_df$year)
  ngram_df <- ngram_df[ which(ngram_df$year >= start & ngram_df$year <= end), ]
  ggplot(ngram_df, aes(x=year, y=counts_permil)) +
    geom_point(size = .5) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size=.25) +
    labs(x="year", y = "frequency (per million words)")+ 
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank()) +
    theme(panel.grid.minor.y =   element_blank(),
          panel.grid.major.y =   element_line(colour = "gray",size=0.25)) +
    theme(rect = element_blank()) +
    theme(legend.title=element_blank()) +
    theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=10))
}

# This function is based on the work of Greis and Hilpert (2012) for 
# Variability-Based Neighbor Clustering. See here:
# https://www.oxfordhandbooks.com/view/10.1093/oxfordhb/9780199922765.001.0001/oxfordhb-9780199922765-e-14
#
# The idea is use hierarchical clustering to aid "bottom up" periodization
# of language change. The functions below are built on their original code here:
# http://global.oup.com/us/companion.websites/fdscontent/uscompanion/us/static/companion.websites/nevalainen/Gries-Hilpert_web_final/vnc.individual.html
# However, rather than producing a plot, this function returns
# an "hclust" object. The advantage, is that an "hclust" object
# can be used to produce not only base R dendrograms, but can be passed
# to other functions for more detailed and controlled plotting.

vnc_clust <- function (x, distance.measure = c("sd", "cv")) {
  
  if (missing(distance.measure)) distance.measure <- "sd"
  if(ncol(x) != 2) stop ("Your data.frame must have 2 columns: one for years and one for a continuous variable.")
  
  l <- lapply(apply(tail(x, -1) - head(x, -1), 2, unique), length) == 1
  if(sum(l, na.rm = TRUE) != 1 & sum(l, na.rm = FALSE) != 1) stop ("Your data doesn't appear to be formatted correctly. You must have 2 columns: one for years (with no missing values) and one for a continuous variable")
  
  input <- as.vector(unlist(x[, -l]))
  years <- as.vector(unlist(x[, l]))
  names(input) <- years
  
  data.collector <- list()
  data.collector[["0"]] <- input
  position.collector <- list()
  position.collector[[1]] <- 0
  overall.distance <- 0
  number.of.steps <- length(input) - 1
  for (i in 1:number.of.steps) {
    cat(i/number.of.steps, "\n", sep = "")
    difference.checker <- numeric()
    for (j in 1:(length(unique(names(input))) - 1)) {
      first.name <- unique(names(input))[j]
      second.name <- unique(names(input))[(j + 1)]
      pooled.sample <- input[names(input) %in% c(first.name, 
                                                 second.name)]
      if(distance.measure == "sd") difference.checker[j] <- ifelse(sum(pooled.sample) == 
                                                                     0, 0, sd(pooled.sample))
      if(distance.measure == "cv") difference.checker[j] <- ifelse(sum(pooled.sample) == 
                                                                     0, 0, sd(pooled.sample)/mean(pooled.sample))
    }
    pos.to.be.merged <- which.min(difference.checker)
    distance <- min(difference.checker)
    overall.distance <- overall.distance + distance
    lower.name <- unique(names(input))[pos.to.be.merged]
    higher.name <- unique(names(input))[(pos.to.be.merged + 
                                           1)]
    matches <- names(input) %in% c(lower.name, higher.name)
    new.mean.age <- round(mean(as.numeric(names(input)[names(input) %in% 
                                                         c(lower.name, higher.name)])), 4)
    position.collector[[(i + 1)]] <- which(names(input) == 
                                             lower.name | names(input) == higher.name)
    names(input)[names(input) %in% c(lower.name, higher.name)] <- as.character(new.mean.age)
    data.collector[[(i + 1)]] <- input
    names(data.collector)[(i + 1)] <- distance
  }
  hc.build <- data.frame(start = unlist(lapply(position.collector, min)),
                         end = unlist(lapply(position.collector, max)))
  
  idx <- seq(1:nrow(hc.build))
  
  y <- lapply(idx, function(i) match(hc.build$start[1:i-1], hc.build$start[i]))
  z <- lapply(idx, function(i) match(hc.build$end[1:i-1], hc.build$end[i]))
  
  merge1 <- lapply(y, function(x) ifelse( !all(is.na(x)), 
                                          max(which(x == 1), na.rm = T) -1, NA))
  
  merge2 <- lapply(z, function(x) ifelse( !all(is.na(x)), 
                                          max(which(x == 1), na.rm = T) -1, NA))
  
  hc.build$merge1 <- lapply(idx, function(i) min(merge1[[i]], merge2[[i]], na.rm = F))
  hc.build$merge2 <- suppressWarnings(lapply(idx, function(i) max(merge1[[i]], merge2[[i]], na.rm = T)))
  hc.build$merge2<- replace(hc.build$merge2, hc.build$merge2 == -Inf, NA)
  
  hc.build$merge1 <- ifelse(is.na(hc.build$merge1) == T & is.na(hc.build$merge2) == T, -hc.build$start, hc.build$merge1)
  hc.build$merge2 <- ifelse(is.na(hc.build$merge2) == T, -hc.build$end, hc.build$merge2)
  
  to.merge <- lapply(idx, function(i) -setdiff(unlist(hc.build[i,1:2]), unlist(hc.build[2:i-1,1:2])))
  
  hc.build$merge1 <- ifelse(is.na(hc.build$merge1) == T, to.merge, hc.build$merge1)
  
  hc.build <- hc.build[-1,]
  
  height <- cumsum(as.numeric(names(data.collector[2:length(data.collector)])))
  order <- seq(1:length(data.collector))
  
  m <- matrix(c(unlist(hc.build$merge1), unlist(hc.build$merge2)), nrow = length(hc.build$merge1))
  hc <- list()
  hc$merge <- m
  hc$height <- height
  hc$order <- order 
  hc$labels <- years
  class(hc) <- "hclust" 
  return(hc)
}

# This is a simple function to return a scree plot based on the VNC algorithm.

vnc_scree <- function (x, distance.measure = c("sd", "cv")) {
  
  if (missing(distance.measure)) distance.measure <- "sd"
  if(ncol(x) != 2) stop ("Your data.frame must have 2 columns: one for years and one for a continuous variable.")
  
  l <- lapply(apply(tail(x, -1) - head(x, -1), 2, unique), length) == 1
  if(sum(l, na.rm = TRUE) != 1 & sum(l, na.rm = FALSE) != 1) stop ("Your data doesn't appear to be formatted correctly. You must have 2 columns: one for years (with no missing values) and one for a continuous variable")
  
  input <- as.vector(unlist(x[, -l]))
  years <- as.vector(unlist(x[, l]))
  names(input) <- years
  
  data.collector <- list()
  data.collector[["0"]] <- input
  position.collector <- list()
  position.collector[[1]] <- 0
  overall.distance <- 0
  number.of.steps <- length(input) - 1
  for (i in 1:number.of.steps) {
    cat(i/number.of.steps, "\n", sep = "")
    difference.checker <- numeric()
    for (j in 1:(length(unique(names(input))) - 1)) {
      first.name <- unique(names(input))[j]
      second.name <- unique(names(input))[(j + 1)]
      pooled.sample <- input[names(input) %in% c(first.name, 
                                                 second.name)]
      if(distance.measure == "sd") difference.checker[j] <- ifelse(sum(pooled.sample) == 
                                                                     0, 0, sd(pooled.sample))
      if(distance.measure == "cv") difference.checker[j] <- ifelse(sum(pooled.sample) == 
                                                                     0, 0, sd(pooled.sample)/mean(pooled.sample))
    }
    pos.to.be.merged <- which.min(difference.checker)
    distance <- min(difference.checker)
    overall.distance <- overall.distance + distance
    lower.name <- unique(names(input))[pos.to.be.merged]
    higher.name <- unique(names(input))[(pos.to.be.merged + 
                                           1)]
    matches <- names(input) %in% c(lower.name, higher.name)
    new.mean.age <- round(mean(as.numeric(names(input)[names(input) %in% 
                                                         c(lower.name, higher.name)])), 4)
    position.collector[[(i + 1)]] <- which(names(input) == 
                                             lower.name | names(input) == higher.name)
    names(input)[names(input) %in% c(lower.name, higher.name)] <- as.character(new.mean.age)
    data.collector[[(i + 1)]] <- input
    names(data.collector)[(i + 1)] <- distance
  }
  plot(rev(names(data.collector)) ~ c(1:length(years)), main = "'Scree' plot", 
       xlab = "Clusters", ylab = "Distance in standard deviations", 
       type = "n")
  grid()
  text(c(1:length(years))[-length(years)], as.numeric(rev(names(data.collector)))[-length(years)], 
       labels = round(as.numeric(rev(names(data.collector))), 2)[-length(years)], cex = 0.8)
  
}


