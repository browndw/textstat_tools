#' Calculate collocational associations by Mutual Information
#' 
#' A function for calculating point-wise mutual information from quanteda tokens.
#' The function requires:
#' - a tokens object
#' - a node word to search for
#' - a window counting to the left
#' - a window counting to the right
#' So collocates_by_MI(my_tokens, "test", 5, 5) would look for collocates words to the left and 5 words to the right of the word "test".
#' 
#' @param target_tkns The target quanteda tokens object.
#' @param node_word The token of interest.
#' @param left A numeric value indicating how many words a span should extend to the left of the node word.
#' @param right A numeric value indicating how many words a span should extend to the right of the node word
#' @return A data.frame containing absolute frequencies and Mutual Information calculations.
#' @export
collocates_by_MI <- function(target_tkns, node_word, left=5, right=5){
  if (class(target_tkns)[1] != "tokens") stop ("Your target must be a quanteda tokens object.")
  if (class(left) != "numeric") stop ("Your span must be numeric.")
  if (class(right) != "numeric") stop ("Your span must be numeric.")
  if (left < 0) stop ("Span values must be positive.")
  if (right < 0) stop ("Span values must be positive.")
  if (left == 0 & right == 0) stop ("The total span must be greater than 0.")
  # Set the span as the sum of our left and right window plus the node
  span <- left + right + 1
  
  # Create regular expressions for use later
  if(left > 0 & right > 0) search_exp <- paste0("(\\S+\\s)", "{", left, "}", node_word, "(\\s\\S+)", "{", right, "}")
  if(left == 0) search_exp <- paste0("^", node_word, "(\\s\\S+)", "{", right, "}")
  if(right == 0) search_exp <- paste0("(\\S+\\s)", "{", left, "}", node_word, "$")
  
  # Generate ngrams the size of the span
  n_grams <- suppressWarnings(quanteda::tokens_ngrams(target_tkns, n=span, concatenator = " "))
  # Convert ngrams to a vector
  n_grams <- unlist(quanteda::as.list(n_grams, target_tkns))
  # Subset the vector to include only ones that contain only the node word first with a fast fixed search then using our regular expression
  n_grams <- n_grams[stringr::str_detect(n_grams, stringr::fixed(node_word))]
  n_grams <- n_grams[stringr::str_detect(n_grams, stringr::regex(search_exp))]
  
  # Extract words to the left and right of the node word
  if(left > 0) tokens_left <- stringr::word(n_grams, start = 1, end = left, sep = stringr::fixed(" "))
  if(right > 0) tokens_right <- stringr::word(n_grams, start = span - right + 1 , end = -1, sep = stringr::fixed(" "))
  
  # Create vectors of all words occurring in our span.
  if(left > 0) tokens_left <- unlist(strsplit(tokens_left, split=" "))
  if(left == 0) tokens_left <- NULL
  if(right > 0) tokens_right <- unlist(strsplit(tokens_right, split=" "))
  if(right == 0) tokens_right <- NULL
  
  # Generate total counts for all words in our corpus.
  totals <- suppressWarnings(quanteda.textstats::textstat_frequency(quanteda::dfm(target_tkns)))

  
  # Create a frequency table and convert it to a data.frame.
  col_freq <- data.frame(table(tolower(c(tokens_left, tokens_right))), stringsAsFactors = F)
  colnames(col_freq) <- c("feature", "c_freq")
  
  # Merge our collocate frequencies with the corpus totals
  col_freq <- merge(col_freq, totals[,1:2], by = "feature", all.x = T)
  
  # Find the frequency of our node word and the total corpus count.
  node_freq <- as.numeric(totals[stringr::str_detect(totals$feature, stringr::regex(paste0("^", node_word, "$"), ignore_case = T)), 2])
  corpus_total <- sum(totals$frequency)
  
  # The function calculates the most common type of Mutual Information.
  # http://corpus.byu.edu/mutualInformation.asp
  #
  # M. Stubbs, Collocations and Semantic Profiles, Functions of Language 2, 1 (1995)
  # MI: http://corpus.byu.edu/mutualInformation.asp
  
  MI1_calc <- function (c_freq, c_total) {
    mi_score <- log2((c_freq/corpus_total)/((c_total/corpus_total)*(node_freq/corpus_total)))
    return(mi_score)
  }
  
  ## Note that you can easily add your own calculations of other MI scores that Brezina discusses:
  ##
  ## MI2_calc <- function (c_freq, c_total){
  ##
  ##    }
  
  col_freq$MI_1 <- MI1_calc(col_freq$c_freq, col_freq$frequency)
  #col_freq$MI_2 <- mapply(MI2_calc, col_freq$c_freq, col_freq$frequency)
  colnames(col_freq) <- c("token", "col_freq", "total_freq", "MI_1")
  col_freq <- col_freq[order(-col_freq$MI_1),]
  col_freq$token <- as.character(col_freq$token)
  rownames(col_freq) <- seq(1:nrow(col_freq))
  attr(col_freq, "node_word") <- node_word
  attr(col_freq, "corpus_total") <- corpus_total
  col_freq <- structure(col_freq, class = c("collocations", "data.frame"))
  return(col_freq)
}

#' Create a table for plotting a collocational network
#' 
#' This function operationalizes the idea of collcational networks described by
#' Brezina, McEnery & Wattam (2015):
#' https://www.jbe-platform.com/content/journals/10.1075/ijcl.20.2.01bre
#' The function takes data.frames produced by the collocates_by_MI() function above
#' and generates a tidygraph data object for plotting in ggraph.
#' 
#' @param col_1 A collocations object produced by collocates_by_MI().
#' @param ... Other collocations objects for plotting.
#' @return A tidygraph table for network plotting.
#' @export
col_network <- function(col_1, ...) {
  
  # put all the collocation data.frames into a list
  all_col <- list(col_1, ...)
  
  # check the object class
  test_class <- lapply(all_col, class)
  if (unique(sapply(test_class, "[[", 1)) != "collocations") stop ("Your data must be collocations objects.")
  
  # extract the node words from attributes
  node_words <- lapply(all_col, function(x) attr(x, "node_word"))
  # extract the corpus totals from the attributes
  corpus_totals <- lapply(all_col, function(x) attr(x, "corpus_total"))
  
  # create an index
  idx <- seq(1:length(all_col))
  
  # normalize frequencies
  all_col <- lapply(idx, function(i) dplyr::mutate(all_col[[i]], col_freq = col_freq/corpus_totals[[i]]))
  # add a column of node words by id
  edges <- lapply(idx, function(i) cbind(node_word = i, all_col[[i]]))
  # bind all data.frames
  edges <- dplyr::bind_rows(edges)
  
  # create a vector of collocation tokens
  col_vect <- lapply(idx, function(i) (all_col[[i]]$token))
  # get unique tokens
  col_vect <- unique(unlist(col_vect))
  # and sort alphabetically
  col_vect <- sort(col_vect)
  
  # calculate the minimum id for collocations based on the number of node words
  id_min <- max(idx) + 1
  # calcuate the max id number
  id_max <- length(col_vect) + (id_min - 1)
  # generate ids for unique collocations
  col_id <- data.frame(id = seq(id_min, id_max), token = col_vect, stringsAsFactors = F)
  # merge to create connections based on ids
  edges <- merge(edges, col_id, by = "token", all=T)
  
  # group all collocates by id
  id_grp <- dplyr::group_by(edges, id)
  # calculate the number of node words each collocation intersects with
  intersects <- dplyr::tally(id_grp)
  # for collocates with multiple intersections, summarize by mean
  freq_norm <- dplyr::summarise(id_grp, freq = mean(col_freq))
  # find the max frequency of all collocates
  freq_max <- max(freq_norm$freq)
  # make a data.frame of node words, setting intersections to 0 and frequency to freq_max
  node_words <- data.frame(id = seq_along(idx), token = unlist(node_words), n = 0, freq = freq_max)
  
  # merge values into a single data.frame, containing all node information
  nodes <- Reduce(function(x, y) merge(x, y, by = "id"), list(col_id, intersects, freq_norm))
  # add node_word values to top of data.frame
  nodes <- rbind(node_words, nodes)
  nodes$token <- as.character(nodes$token)
  nodes$n <- as.factor(nodes$n)
  nodes <- dplyr::rename(nodes, label = token, n_intersects = n, node_weight = freq)
  
  # assemble edge values
  edges <- dplyr::select(edges, id, node_word, MI_1)
  edges <- dplyr::rename(edges, to = id, from = node_word, link_weight = MI_1)
  
  # generate a tidygraph object for plotting
  col_net <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = F)
  return(col_net)
}


