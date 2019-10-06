# Take a target column and a reference column, and return an effect size
# This effect size calcuation is called Log Ratio
# And was proposed by Andrew Hardie: http://cass.lancs.ac.uk/log-ratio-an-informal-introduction/
log_ratio <- function (n_target, n_reference) {
  total_a <- sum(n_target)
  total_b <- sum(n_reference)
  percent_a <- ifelse(n_target == 0, 0.5 / total_a, n_target/total_a)
  percent_b <- ifelse(n_reference == 0, 0.5 / total_b, n_reference/total_b)
  ratio <- log2(percent_a / percent_b)
  ratio <- round(ratio, 2)
  return(ratio)
}

# The following function is based on an idea proposed by Mike Scott
# and used in his concordancer WordSmith:
# https://lexically.net/downloads/version4/html/index.html?database_info.htm
# Rather than summing counts from all texts in the target corpus
# and comparing them to those in a reference corpus,
# Scott proposes to iterate through each text in the target corpus,
# calculating keyness values against the reference corpus.
# Then you find how many texts reach some significance threshold.
# Essentially, this is a way of accounting for distribution:
# Are a few texts driving keyness values? Or many?
#
# The function returns a data.frame that includes:
# - the percent of texts in the target corpus for which keyness reaches the specified threshold
# - the mean keyness value in the target 
# - the standard deviation of keyness
# - the mean effect size by log ratio
#
# Note that it is easy enough to alter the function to return other values.
# Also, the function works, but hasn't been optimized in any way,
# So it's not particularly efficient.
# Finally, the function requires the package DescTools.
key_keys <- function (target_dfm, reference_dfm, threshold=c(0.05, 0.01, 0.001, 0.0001), correct=TRUE){
  if (class(target_dfm)[1] != "dfm") stop ("your target must be a quanteda dfm object")
  if (class(reference_dfm)[1] != "dfm") stop ("your reference must be a quanteda dfm object")
  
  # Here we just specify the thresholds for p-values with df = 1
  if(missing(threshold)) {
    th <- 3.84} else {
      if(threshold == 0.05) th <- 3.84;
      if(threshold == 0.01) th <- 6.63;
      if(threshold == 0.001) th <- 10.83;
      if(threshold == 0.0001) th <- 15.13}
  # The function defaults to a yates correction. But you can turn it off.
  c <- ifelse(correct==T, "yates", "none")
  
  # Here, we're just restructuring our data.
  # First, we're making sure it's trimmed.
  target_dfm <- dfm_trim(target_dfm, min_termfreq = 1)
  reference_dfm <- dfm_trim(reference_dfm, min_termfreq = 1)
  
  # Sum the frequencies for the reference corpus.
  reference_df <- textstat_frequency(reference_dfm)
  
  # Prep the target corpus by first converting it to a data.frame
  target_df <- convert(target_dfm, to="data.frame")
  target_docs <- target_df$document
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
  
  # The GTest() function requires two pairs of values:
  # the observed counts, and the proportions of the target and reference corpora.
  # So we'll create a vector of the proportion of each text's total count in
  # relation to the reference corpus.
  target_prop <- colSums(target_counts)/(colSums(target_counts)+sum(reference_counts))
  
  # This generates an index of columns that we can iterate through.
  idx <- seq(ncol(target_counts))
  
  # This is the function for calculating log-likehood using the
  # GTest() function.
  # Note that GTest() returns an absolute value, so the end of this
  # function converts that to a postive or negative value
  # depending on whether the relative frequency of a token is higher
  # in the target text or reference corpus.
  log_like <- function(count_a, count_b, prop_a) { 
    observed <- c(count_a, count_b)
    expected <- c(prop_a, 1-prop_a)
    if(sum(observed) == 0) return(0)
    likelihood <- DescTools::GTest(x=observed, p=expected, correct=c)
    likelihood <- unlist(likelihood$statistic)
    likelihood <- ifelse(count_a/prop_a > count_b/(1-prop_a), likelihood, -likelihood)
    likelihood <- round(likelihood, 2)
    return(likelihood)
  }
  
  # This iterates through the columns and generates a data.frame
  # of keyness values.
  # If you want to return this data.frame, it's easy enough
  # to edit the function.
  # Also, though this works, mapply() is probably not the most efficient way of 
  # doing this.
  keyness <- as.data.frame(sapply(idx, 
             function(i) mapply(log_like, target_counts[,i], 
             reference_counts, target_prop[i])))
  rownames(keyness) <- feature
  
  # We're also going to generate a data.frame of effect sizes.
  effect <- as.data.frame(sapply(idx, function(i) log_ratio(target_counts[,i], 
            reference_counts)))
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
  # And return the report.
  return(report)
}

# This function takes any number of quanteda dfm objects
# and returns a table of log-likelihood values, effect sizes
# (using Hardie's log ratio) and p-values
# This function reqires DescTools and data.table
keyness_pairs <- function(dfm_a, dfm_b, ...){
  all_corpora <- list(dfm_a, dfm_b, ...)
  test_class <- lapply(all_corpora, class)
  if (unique(test_class)[1] != "dfm") stop ("your corpora must be a quanteda dfm objects")
  if (length(unique(test_class)) != 1) stop ("your corpora must be a quanteda dfm objects")
  
  # Generate frequency lists using textstat_frequency()
  freq_list <- lapply(all_corpora, textstat_frequency)
  
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
    j <- corpora_pairs[1,i]
    k <- corpora_pairs[2,i]
    l <- paste(j, k, sep = "_v_")})
  # Introduce a function for calculating log-likelihood using DescTools
  log_like <- function(count_a, count_b, total_a, total_b) { 
    prop_a <- total_a/(total_a + total_b)
    observed <- c(count_a, count_b)
    expected <- c(prop_a, 1-prop_a)
    if(sum(observed) == 0) return(0)
    likelihood <- DescTools::GTest(x=observed, p=expected, correct="yates")
    likelihood <- unlist(likelihood$statistic)
    likelihood <- ifelse(count_a/prop_a > count_b/(1-prop_a), likelihood, -likelihood)
    likelihood <- round(likelihood, 2)
    return(likelihood)
  }
  
  # Calculate G2
  ll <- as.data.frame(sapply(pair_idx, function(i) {
    j <- corpora_pairs[1,i]
    k <- corpora_pairs[2,i]
    mapply(log_like, freq_df[,j], freq_df[,k], total_counts[j], total_counts[k])}))
  # Apply column names
  colnames(ll) <- lapply(comp_names, function(x) paste(x, "G2", sep = "_"))
  # Calculate the effect sizes
  lr <- as.data.frame(sapply(pair_idx, function(i) {
    j <- corpora_pairs[1,i]
    k <- corpora_pairs[2,i]
    log_ratio(freq_df[,j], freq_df[,k])}))
  # Apply column names
  colnames(lr) <- lapply(comp_names, function(x) paste(x, "lr", sep = "_"))
  # Calculate p-values
  pv <- as.data.frame(sapply(pair_idx, function(i) {mapply ((function(x) pchisq(abs(x),1,lower.tail=FALSE)), ll[,i])}))
  pv <- format(round(pv, 5), nsmall = 5)
  # Apply column names
  colnames(pv) <- lapply(comp_names, function(x) paste(x, "pv", sep = "_"))
  # Assemble the table of all values
  key_table <- cbind(ll, lr, pv)
  # Order by names
  key_table <- key_table[ , order(names(key_table))]
  # Add rownames from the frequency counts
  rownames(key_table) <- rownames(freq_df)
  # Reorder by the first column
  key_table <- key_table[order(key_table[,1], decreasing = TRUE),]
  # Return the table
  return(key_table)
}

