#' A function to test the "evenness" of a sequence.
#' @param x A vector of integers or numbers.
#' @param ... additional parameters passed to default sequence method.
#' @export
is.sequence <- function(x, tol = sqrt(.Machine$double.eps), ...){
  if(anyNA(x) || any(is.infinite(x)) || length(x) <= 1 || diff(x[1:2]) == 0)
    return(FALSE)
  diff(range(diff(x))) <= tol
}


#' This function is based on the work of Greis and Hilpert (2012) for
#' Variability-Based Neighbor Clustering. See here:
#' https://www.oxfordhandbooks.com/view/10.1093/oxfordhb/9780199922765.001.0001/oxfordhb-9780199922765-e-14
#'
#' The idea is to use hierarchical clustering to aid "bottom up" periodization
#' of language change. The functions below are built on their original code here:
#' http://global.oup.com/us/companion.websites/fdscontent/uscompanion/us/static/companion.websites/nevalainen/Gries-Hilpert_web_final/vnc.individual.html
#' However, rather than producing a plot, this function returns
#' an "hclust" object. The advantage, is that an "hclust" object
#' can be used to produce not only base R dendrograms, but can be passed
#' to other functions for more detailed and controlled plotting.
#' @param time A vector of sequential time intervals like years or decades
#' @param values A vector containing normalized frequency counts
#' @param distance.measure Indicating whether the standard deviation or coefficient of variation should be used in dinstance calculations
#' @return An hclust object
#' @export
vnc_clust <- function (time, values, distance.measure = c("sd", "cv")) {
  
  if (missing(distance.measure)) distance.measure <- "sd"
  
  if(is.sequence(time) == F) stop ("It appears that your time series contains gaps or is not evenly spaced.")
  if(length(time) != length(values)) stop ("Your time a values vectors must be the same length.")
  
  input <- as.vector(values)
  years <- as.vector(time)
  names(input) <- years
  
  data.collector <- list()
  data.collector[["0"]] <- input
  position.collector <- list()
  position.collector[[1]] <- 0
  overall.distance <- 0
  number.of.steps <- length(input) - 1
  for (i in 1:number.of.steps) {
    #cat(i/number.of.steps, "\n", sep = "")
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

#' This is a simple function to return a scree plot based on the VNC algorithm.
#' @param time A vector of sequential time intervals like years or decades
#' @param values A vector containing normaized frequency counts
#' @param distance.measure Indicating whether the standard deviation or coefficient of variation should be used in dinstance calculations
#' @return A scree plot
#' @export
vnc_scree <- function (time, values, distance.measure = c("sd", "cv")) {
  
  if (missing(distance.measure)) distance.measure <- "sd"
  
  if(is.sequence(time) == F) stop ("It appears that your time series contains gaps or is not evenly spaced.")
  if(length(time) != length(values)) stop ("Your time a values vectors must be the same length.")
  
  input <- as.vector(values)
  years <- as.vector(time)
  names(input) <- years
  
  data.collector <- list()
  data.collector[["0"]] <- input
  position.collector <- list()
  position.collector[[1]] <- 0
  overall.distance <- 0
  number.of.steps <- length(input) - 1
  for (i in 1:number.of.steps) {
    #cat(i/number.of.steps, "\n", sep = "")
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
