#' Read texts from a vector of paths
#' 
#' Replaces the readtext::readtext function that reads a lists of text files into a data frame.
#' 
#' @param paths A vector of paths to text files that are to be read in.
#' @return A readtext data.frame
#' @export
readtext_lite <- function(paths) {
  # Get a vector of the file basenames
  doc_ids <- basename(paths)
  # Create a vector collapsing each text file into one element in a character
  # vector
  texts <- vapply(paths, function(i) paste(readLines(i), collapse = "\n"), 
                  FUN.VALUE = character(1))
  text_df <- data.frame(doc_id = doc_ids, text = texts, stringsAsFactors = FALSE)
  rownames(text_df) <- seq(1:nrow(text_df))
  text_df <- structure(text_df, class = c("readtext", "data.frame"))
  return(text_df)
}

#' Pre-process texts
#' 
#' A simple function that requires a readtext object.
#' It then processes the text column using basic regex substitutions.
#' The default is to add a space before possessives and contractions.
#' This will force their tokenization in quanteda.
#' So that "Shakespeare's" will be counted as two tokens rather than a single one.
#' It is easy to add or delete substations as fits your analytical needs.
#' 
#' @param txt A character vector
#' @param contractions A logical value to separate contractions into two tokens
#' @param hypens A logical value to separate hypenated words into two tokens
#' @param punctuation A logical value to remove punctuation
#' @param lower_case A logical value to make all tokens lower case
#' @param accent_replace A logical value to replace accented characters with un-accented ones
#' @param remove_numbers A logical value to remove numbers
#' @return A character vector
#' @export
preprocess_text <- function(txt, contractions=TRUE, hypens=TRUE, punctuation=TRUE, lower_case=TRUE, accent_replace=TRUE, remove_numbers=FALSE){
  cont_replace <- function(x) {
    x <- gsub( "'s\\b", " 's", x)
    x <- gsub( "n't\\b", " n't", x)
    x <- gsub( "'ll\\b", " 'll", x)
    x <- gsub( "'d\\b", " 'd", x)
    x <- gsub( "'re\\b", " 're", x)
    x <- gsub( "'m\\b", " 'm", x)
    x <- gsub( "'ve\\b", " 've", x)
    x <- gsub( "'re\\b", " 're", x)
    return(x)
  }
  if (contractions==TRUE) txt <- cont_replace(txt)
  if (lower_case==TRUE) txt <- tolower(txt)
  if (hypens==TRUE) txt <- gsub( "-", " ", txt)
  if (remove_numbers==TRUE) txt <- stringr::str_remove_all(txt, "\\b[0-9]+\\b")
  if (punctuation==TRUE) txt <- gsub( "(?:(?<![A-Za-z0-9])[[:punct:]]+)|(?:[[:punct:]]+(?![A-Za-z0-9]))", "", txt, perl = T)
  if (accent_replace==TRUE) txt <- stringi::stri_trans_general(txt, "Latin-ASCII")
  txt <- stringr::str_squish(txt)
}

#' A function for expanding letter sequences.
#' @param i Index of alphabetic character
#' @return A vector of character combinations in the style of Excel column headers
#' @export
excel_style <- function(i) {
  base10toA <- function(n, A) {
    stopifnot(n >= 0L)
    N <- length(A)
    j <- n %/% N 
    if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
  }   
  vapply(i-1L, base10toA, character(1L), LETTERS)
}

#' A function for detecting the size of a corpus and setting the narmalizing factor to the nearest power of 10.
#' @param corpus_total The total number of words in the corpus
#' @return A named vector
#' @export
normalizing_factor <- function(x){
  nf <- 10^floor(log10(x))
  nf <- ifelse(nf > 10^6, 10^6, nf)
  nf <- ifelse(nf < 100, 100, nf)
  label <- paste0("Per_", "10", ".", stringr::str_count(formatC(nf, format = "f", digits = 0), "0"))
  names(nf) <- label
  return(nf)
}
