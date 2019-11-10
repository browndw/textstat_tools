
# A function for calculating point-wise mutual information
# from quanteda tokens.
# The function requires:
# - a tokens object
# - a node word to search for
# - a wondow counting to the left
# - a wondow counting to the right
# So collocates_by_MI(my_tokens, "test", 5, 5)
# would look for collocates words to the left
# and 5 words to the right of the word "test".
collocates_by_MI <- function(quant_tokens, node_word, left, right){
  if (class(quant_tokens)[1] != "tokens") stop ("your target must be a quanteda tokens object")
  if (class(left) != "numeric") stop ("your span must be numeric")
  if (class(right) != "numeric") stop ("your span must be numeric")
  # Set the span as the sum of our left and right window
  span <- max(left, right)
  
  # Create a kwic object from quanteda tokens.
  # This takes the uses the maximum of our left and right values to
  # set the window.
  df <- kwic(quant_tokens, node_word, window = span, valuetype = "fixed")
  
  # Generate total counts for all words in our corpus.
  totals <- textstat_frequency(dfm(quant_tokens))
  
  # Using sringr, we can trim down what appeares before or after
  # our node word, if our left and right values are unequal.
  if(min(left, right) == left & left > 0) pre_trim <- lapply(df$pre, function(x) stringr::word(string = x, start = 1, end = left, sep = fixed(" ")))
  if(max(left, right) == left) pre_trim <- df$pre
  
  if(min(left, right) == right & right > 0) post_trim <- lapply(df$post, function(x) stringr::word(string = x, start = 1, end = right, sep = fixed(" ")))
  if(max(left, right) == right) post_trim <- df$post
  
  # Create vectors of all words occuring in our span.
  if(left > 0) pre_words <- unlist(lapply(pre_trim, function(x) strsplit(x, split=" ")))
  if(left == 0) pre_words <- NULL
  if(right > 0) post_words <- unlist(lapply(post_trim, function(x) strsplit(x, split=" ")))
  if(right == 0) post_words <- NULL
  
  # Create a frequency table and convert it to a data.frame.
  col_freq <- data.frame(table(tolower(c(pre_words, post_words))), stringsAsFactors = F)
  colnames(col_freq) <- c("feature", "c_freq")
  
  # Merge our collocate frequencies with the corpus totals
  col_freq <- merge(col_freq, totals[,1:2], by = "feature", all.x = T)
  
  # Find the frequency of our node word and the total corpus count.
  node_freq <- as.numeric(totals[stringr::str_detect(totals$feature, regex(paste0("^", node_word, "$"), ignore_case = T)), 2])
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
  
  col_freq$MI_1 <- mapply(MI1_calc, col_freq$c_freq, col_freq$frequency)
  #col_freq$MI_2 <- mapply(MI2_calc, col_freq$c_freq, col_freq$frequency)
  colnames(col_freq) <- c("feature", "col_freq", "total_freq", "MI_1")
  col_freq <- col_freq[order(-col_freq$MI_1),]
  col_freq$feature <- as.character(col_freq$feature)
  attr(col_freq, "node_word") <- node_word
  attr(col_freq, "corpus_total") <- corpus_total
  col_freq <- structure(col_freq, class = c("collocations", "data.frame"))
  return(col_freq)
}

# This function operationalizes the idea of collcational networks described by
# Brezina, McEnery & Wattam (2015):
# https://www.jbe-platform.com/content/journals/10.1075/ijcl.20.2.01bre
#
# The function takes data.frames produced by the collocates_by_MI() function above
# and generates a tidygraph data object for plotting in ggraph.
col_network <- function(col_1, col_2, ...) {
  
  # put all the collocation data.frames into a list
  all_col <- list(col_1, col_2, ...)
  
  # check the object class
  #test_class <- lapply(all_col, class)
  #if (unique(sapply(test_class, "[[", 1)) != "collocations") stop ("your data must be collocations objects")
  
  # extract the node words from attributes
  node_words <- lapply(all_col, function(x) attr(x, "node_word"))
  # extract the corpus totals from the attritubes
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
  col_vect <- lapply(idx, function(i) (all_col[[i]]$feature))
  # get unique tokens
  col_vect <- unique(unlist(col_vect))
  # and sort alphabetically
  col_vect <- sort(col_vect)
  
  # calculate the minimum id for collocations based on the number of node words
  id_min <- max(idx) + 1
  # calcuate the max id number
  id_max <- length(col_vect) + (id_min - 1)
  # generate ids for unique collocations
  col_id <- data.frame(id = seq(id_min, id_max), feature = col_vect, stringsAsFactors = F)
  # merge to create connections based on ids
  edges <- merge(edges, col_id, by = "feature", all=T)
  
  # group all collocates by id
  id_grp <- dplyr::group_by(edges, id)
  # calculate the number of node words each collocation intersects with
  intersects <- dplyr::tally(id_grp)
  # for collocates with multiple intersections, summarize by mean
  freq_norm <- dplyr::summarise(id_grp, freq = mean(col_freq))
  # find the max frequency of all collocates
  freq_max <- max(freq_norm$freq)
  # make a data.frame of node words, settining intersections to 0 and frequency to freq_max
  node_words <- data.frame(id = seq_along(idx), feature = unlist(node_words), n = 0, freq = freq_max)
  
  # merge values into a single data.frame, containing all node information
  nodes <- Reduce(function(x, y) merge(x, y, by = "id"), list(col_id, intersects, freq_norm))
  # add node_word values to top of data.frame
  nodes <- rbind(node_words, nodes)
  nodes$feature <- as.character(nodes$feature)
  nodes$n <- as.factor(nodes$n)
  nodes <- dplyr::rename(nodes, label = feature, n_intersects = n, node_weight = freq)
  
  # assemble edge values
  edges <- dplyr::select(edges, id, node_word, MI_1)
  edges <- dplyr::rename(edges, to = id, from = node_word, link_weight = MI_1)
  
  # generate a tidygraph object for plotting
  col_net <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = F)
  return(col_net)
}


