
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
  col_freq <- data.frame(table(tolower(c(pre_words, post_words))))
  colnames(col_freq) <- c("feature", "c_freq")
  
  # Merge our collocate frequencies with the corpus totals
  col_freq <- merge(col_freq, totals[,1:2], by = "feature", all.x = T)
  
  # Find the frequency of our node word and the total corpus count.
  node_freq <- as.numeric(totals[stringr::str_detect(totals$feature, paste0("^", node_word, "$")), 2])
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
  return(col_freq)
}



