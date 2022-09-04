#' Log-likelihood calculation
#' 
#' Log-likelihood tests the frequencies of tokens in one corpus vs. another.
#' It is often used instead of a chi-square test, as it has been shown to be
#' more resistant to corpora of varying sizes. For more detail see: http://ucrel.lancs.ac.uk/llwizard.html
#' 
#' @param n_target The raw (non-normalized) token count in the target corpus
#' @param n_reference The raw (non-normalized) token count in the reference corpus
#' @param total_target The total number of tokens in the target corpus
#' @param total_reference The total number of tokens in the reference corpus
#' @return A numeric value representing log-likelihood
#' @export
log_like <- function(n_target, n_reference, total_target, total_reference, correct=FALSE) {
  expected_a <- (n_target + n_reference)*(total_target/(total_target + total_reference))
  expected_b <- (n_target + n_reference)*(total_reference/(total_target + total_reference))
  if(correct==T){ #Perform the "Yates" correction
    n_a <- ifelse(n_target - expected_a > 0.25, n_target - 0.5, n_target)
    n_b <- ifelse(n_target - expected_a > 0.25,  n_reference + 0.5, n_reference)
    
    n_a <- ifelse(expected_a - n_target > 0.25, n_target + 0.5, n_a)
    n_b <- ifelse(expected_a - n_target > 0.25, n_reference - 0.5, n_b)
  }
  else{
    n_a <- n_target
    n_b <- n_reference
  }
  L1 <- ifelse(n_a == 0, 0, n_a*log(n_a/expected_a))
  L2 <- ifelse(n_b == 0, 0, n_b*log(n_b/expected_b))
  likelihood <- 2*(L1 + L2)
  likelihood <- ifelse(n_target/total_target > n_reference/total_reference, likelihood, -likelihood)
  return(likelihood)
}

#' Log-ratio calculation
#' 
#' Take a target column and a reference column, and return an effect size
#' This effect size calculation is called Log Ratio
#' And was proposed by Andrew Hardie: http://cass.lancs.ac.uk/log-ratio-an-informal-introduction/
#' 
#' @param n_target The raw (non-normalized) token count in the target corpus
#' @param n_reference The raw (non-normalized) token count in the reference corpus
#' @param total_target The total number of tokens in the target corpus
#' @param total_reference The total number of tokens in the reference corpus
#' @return A numeric value representing the log ratio
#' @export
log_ratio <- function (n_target, n_reference, total_target, total_reference) {
  total_a <- sum(n_target)
  total_b <- sum(n_reference)
  percent_a <- ifelse(n_target == 0, 0.5 / total_a, n_target/total_target)
  percent_b <- ifelse(n_reference == 0, 0.5 / total_b, n_reference/total_reference)
  ratio <- log2(percent_a / percent_b)
  return(ratio)
}

#' Key of keys calculation
#' 
#' The following function is based on an idea proposed by Mike Scott
#' and used in his concordancer WordSmith:
#' https://lexically.net/downloads/version4/html/index.html?database_info.htm
#' Rather than summing counts from all texts in the target corpus
#' and comparing them to those in a reference corpus,
#' Scott proposes to iterate through each text in the target corpus,
#' calculating keyness values against the reference corpus.
#' Then you find how many texts reach some significance threshold.
#' Essentially, this is a way of accounting for distribution:
#' Are a few texts driving keyness values? Or many?
#' The function returns a data.frame that includes:
#' - the percent of texts in the target corpus for which keyness reaches the specified threshold
#' - the mean keyness value in the target 
#' - the standard deviation of keyness
#' - the mean effect size by log ratio
#' Note that it is easy enough to alter the function to return other values.
#' 
#' @param target_dfm The target document-feature matrix
#' @param reference_dfm The reference document-feature matrix
#' @param threshold The p-value threshold for calculating percentage of documents reaching significance
#' @param yates A logical value indicating whether the "Yates" correction should be performed
#' @return A data.frame containing the percentage of documents reaching significance, mean keyness, and mean effect size
#' @export
key_keys <- function(target_dfm, reference_dfm, threshold=c(0.05, 0.01, 0.001, 0.0001), yates=FALSE){
  if (class(target_dfm)[1] != "dfm") stop ("Your target must be a quanteda dfm object.")
  if (class(reference_dfm)[1] != "dfm") stop ("Your reference must be a quanteda dfm object.")
  
  # Here we just specify the thresholds for p-values with df = 1
  if(missing(threshold)) {
    th <- 3.84} else {
      if(threshold == 0.05) th <- 3.84;
      if(threshold == 0.01) th <- 6.63;
      if(threshold == 0.001) th <- 10.83;
      if(threshold == 0.0001) th <- 15.13}
  
  # Here, we're just restructuring our data.
  # First, we're making sure it's trimmed.
  target_dfm <- suppressWarnings(quanteda::dfm_trim(target_dfm, min_termfreq = 1))
  reference_dfm <-suppressWarnings(quanteda::dfm_trim(reference_dfm, min_termfreq = 1))
  
  # Sum the frequencies for the reference corpus.
  reference_df <- suppressWarnings(quanteda.textstats::textstat_frequency(reference_dfm))
  
  # Prep the target corpus by first converting it to a data.frame
  target_df <- suppressWarnings(quanteda::convert(target_dfm, to = "data.frame"))
  target_docs <- target_df$doc_id
  # There are other ways of doing this, but we need to make sure
  # our data contains ALL tokens for both the target and reference corpora.
  # In base R, we first transform the data.frame.
  target_df <- as.data.frame(t(target_df), stringsAsFactors = FALSE)
  # Then rearrange our row names and column names.
  target_df <- tibble::rownames_to_column(target_df, "feature")
  colnames(target_df) <- c("feature", target_docs)
  target_df <- target_df[-1 ,]
  # Drop the unnecssary columns from our reference.
  reference_df <- reference_df[, -c(3:5)]
  # Now we merge both into a combined data.frame specifying all = T.
  comb_df <- merge(reference_df, target_df, by = "feature", all = TRUE)
  # Convert NAs into zeros.
  comb_df[is.na(comb_df)] <- 0
  # Now we create a vector of our reference counts from a column called "frequency".
  feature <- comb_df$feature
  reference_counts <- comb_df$frequency
  # Drop columns to create the data.frame of our target counts.
  target_counts <- comb_df[, -c(1:2)]
  # Convert from character to numeric values.
  target_counts <- as.data.frame(sapply(target_counts, as.numeric))
  
  target_totals <- as.vector(colSums(target_counts))
  reference_total <- sum(reference_counts)
  
  # This generates an index of columns that we can iterate through.
  idx <- seq(ncol(target_counts))
  
  # This iterates through the columns and generates a data.frame of keyness values.
  # If you want to return this data.frame, it's easy enough to edit the function.
  
  keyness <- as.data.frame(sapply(idx, function(i) quanteda.extras::log_like(target_counts[,i], reference_counts, target_totals[i], reference_total, correct = yates)))
  
  # We're also going to generate a data.frame of effect sizes.
  effect <- as.data.frame(sapply(idx, function(i) quanteda.extras::log_ratio(target_counts[,i], reference_counts, target_totals[i], reference_total)))
  rownames(effect) <- feature
  
  # From these two data.frames we can generate some values.
  #
  # The mean effect sizes:
  effect_mean <- apply(effect, 1, mean)
  # The percentages of texts that reach the p-value threshold:
  key_range <- apply(keyness, 1, function(x) (length(which(x > th))/ncol(keyness))*100)
  # The mean keyness values:
  key_mean <- apply(keyness, 1, mean)
  # Standard deviations for keyness values:
  key_sd <- apply(keyness, 1, sd)
  # Combine those into a report.
  report <- data.frame(cbind(key_range, key_mean, key_sd, effect_mean))
  # Order the report by mean keyness.
  report <- report[order(-report$key_mean),]
  report <- tibble::rownames_to_column(report, "token")
  # And return the report.
  return(report)
}

#' Pairwise keyness values from any number of dfms
#' 
#' This function takes any number of quanteda dfm objects and returns a table of log-likelihood values, effect sizes
#' using Hardie's log ratio and p-values
#' 
#' @param dfm_a A document-feature matrix
#' @param dfm_b A document-feature matrix
#' @param ... Additional document-feature matrices
#' @param yates A logical value indicating whether the "Yates" correction should be performed
#' @return A data.frame containing pairwise keyness comparisons of all dfms
#' @export
keyness_pairs <- function(dfm_a, dfm_b, ..., yates=FALSE){
  all_corpora <- list(dfm_a, dfm_b, ...)
  test_class <- lapply(all_corpora, class)
  if (unique(test_class)[1] != "dfm") stop ("Your corpora must be a quanteda dfm objects.")
  if (length(unique(test_class)) != 1) stop ("Your corpora must be a quanteda dfm objects.")
  
  # Generate frequency lists using textstat_frequency()
  freq_list <- lapply(all_corpora, suppressWarnings(quanteda.textstats::textstat_frequency))
  
  # Subset out the need columns
  freq_list <- lapply(freq_list, function(x) subset(x, select=c("feature", "frequency")))
  # Create an index
  idx <- seq(length(all_corpora))
  # Rename columns so they will be unique using data.table
  new_names <- lapply(idx, function(i) paste("V", i, sep="_"))
  freq_list <- lapply(idx, function(i) data.table::setnames(freq_list[[i]], "frequency", new_names[[i]]))
  # Merge all lists by feature. This ensures that we have ALL features from ALL dfms
  # even in counts in one or more may be zero
  freq_df <- Reduce(function(...) merge(..., by = "feature", all=T), freq_list)
  freq_df <- data.frame(freq_df[,-1], row.names=freq_df[,1])
  # Set NA values to zero for calcutaions
  freq_df[is.na(freq_df)] <- 0
  # Create an index of pairs
  corpora_pairs <- combn(idx, 2)
  pair_idx <- seq(ncol(corpora_pairs))
  # Get the total counts
  total_counts <- colSums(freq_df)
  comp_names <- sapply(pair_idx, function(i) {
    name_pairs <- combn(quanteda.extras::excel_style(idx), 2)
    j <- name_pairs[1,i]
    k <- name_pairs[2,i]
    l <- paste(j, k, sep = "_v_")})
  
  # Calculate log-likeihood
  ll <- as.data.frame(sapply(pair_idx, function(i) {
    j <- corpora_pairs[1,i]
    k <- corpora_pairs[2,i]
    quanteda.extras::log_like(freq_df[,j], freq_df[,k], total_counts[j], total_counts[k], correct = yates)}))
  # Apply column names
  colnames(ll) <- lapply(comp_names, function(x) paste(x, "LL", sep = "_"))
  # Calculate the effect sizes
  lr <- as.data.frame(sapply(pair_idx, function(i) {
    j <- corpora_pairs[1,i]
    k <- corpora_pairs[2,i]
    quanteda.extras::log_ratio(freq_df[,j], freq_df[,k], total_counts[j], total_counts[k])}))
  # Apply column names
  colnames(lr) <- lapply(comp_names, function(x) paste(x, "LR", sep = "_"))
  # Calculate p-values
  pv <- as.data.frame(sapply(pair_idx, function(i) {mapply ((function(x) pchisq(abs(x),1,lower.tail=FALSE)), ll[,i])}))
  # Apply column names
  colnames(pv) <- lapply(comp_names, function(x) paste(x, "PV", sep = "_"))
  # Assemble the table of all values
  key_table <- cbind(ll, lr, pv)
  # Order by names
  key_table <- key_table[ , order(names(key_table))]
  # Add rownames from the frequency counts
  rownames(key_table) <- rownames(freq_df)
  # Reorder by the first column
  key_table <- key_table[order(key_table[,1], decreasing = TRUE),]
  key_table <- tibble::rownames_to_column(key_table, "Token")
  # Return the table
  return(key_table)
}

#' Keyness measures for all tokens in a corpus
#' 
#' The keyness_table() function returns the log-likelihood of the target vs. reference corpus, effect sizes by log ratio, p-values, absolute frequencies, relative frequencies, and deviation of proportions.
#' 
#' @param target_dfm The target document-feature matrix
#' @param reference_dfm The reference document-feature matrix
#' @param yates A logical value indicating whether the "Yates" correction should be performed
#' @return A data.frame containing the log-likelihood, log ratio, absolute frequencies, relative frequencies, and dispersions
#' @export
keyness_table <- function(target_dfm, reference_dfm, yates=FALSE){
  
  if (class(target_dfm)[1] != "dfm") stop ("Your target must be a quanteda dfm object.")
  if (class(reference_dfm)[1] != "dfm") stop ("Your reference must be a quanteda dfm object.")
  
  total_counts <- c(sum(quanteda::ntoken(target_dfm)), sum(quanteda::ntoken(reference_dfm)))
  nf <- quanteda.extras::normalizing_factor(max(total_counts))
  
  freq_table <- function(dfm){
    
    m <- as.matrix(dfm)
    idx <- seq(1:ncol(m))
    total <- sum(rowSums(m))
    #calculte the relative sizes of the parts of the corpus (in percent)
    parts <- rowSums(m)/total
    
    dp <- function(v, s=rep(1/length(v))) {
      
      n <- length(v) # n
      f <- sum(v) # f
      s <- s/sum(s) # s
      
      values <- list()
      values[["AF"]] <- f
      values[[names(nf)]] <- (f/total)*as.numeric(nf)
      values[["DP"]] <- sum(abs((v/f)-s))/2
      values <- as.data.frame(t(as.matrix(unlist(values))))
      return(values)
    }
    
    dsp <- lapply(idx, function(i){dp(m[,i], parts)})
    dsp <- data.frame(data.table::rbindlist(dsp))
    dsp$Token <- colnames(m)
    dsp <- dsp[order(-dsp$AF),]
    return(dsp)
  }
  
  target_df <- freq_table(target_dfm)
  reference_df <- freq_table(reference_dfm)
  
  freq_df <- merge(target_df, reference_df, by = "Token", all = T)
  freq_df <- freq_df[,c(1,2,5,3,6,4,7)]
  freq_df[,2:5][is.na(freq_df[,2:5])] <- 0
  colnames(freq_df) <- gsub("\\.x", "_Tar", colnames(freq_df))
  colnames(freq_df) <- gsub("\\.y", "_Ref", colnames(freq_df))
  
  ll <- quanteda.extras::log_like(freq_df[,2], freq_df[,3], total_counts[1], total_counts[2], correct = yates)
  lr <- quanteda.extras::log_ratio(freq_df[,2], freq_df[,3], total_counts[1], total_counts[2])
  pv <- pchisq(abs(ll),1,lower.tail=FALSE)
  
  freq_df$LL <- ll
  freq_df$LR <- lr
  freq_df$PV <- pv
  freq_df <- freq_df[,c(1, 8:10, 2:7)]
  freq_df <- freq_df[order(freq_df[,2], decreasing = TRUE),]
  rownames(freq_df) <- seq(1:nrow(freq_df))
  return(freq_df)
}

