# Helper functions for processing text

# Replaces the readtext::readtext function that reads a lists of text files into
# a data frame
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

# A simple function that requires a readtext object.
# It then processes the text column using basic regex substitutions.
# The default is to add a space before possessives and contractions.
# This will force their tokenization in quanteda.
# So that "Shakespeare's" will be counted as two tokens rather than a single one.
# It is easy to add or delete subsitions as fits your analytical needs.

preprocess_text <- function(text_obj, contractions=TRUE, hypens=FALSE, punctuation=FALSE, lower_case=FALSE){
  if (class(text_obj)[1] != "readtext") stop("your text must be in a readfile data.frame")
  text <- text_obj$text
  doc_id <- text_obj$doc_id
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
  if (contractions==TRUE) text <- cont_replace(text)
  if (lower_case==TRUE) text <- tolower(text)
  if (hypens==TRUE) text <- gsub( "-", " ", text)
  if (punctuation==TRUE) text <- gsub( "[[:punct:]]", "", text)
  text <- gsub(" +", " ", text)
  result <- as.data.frame(cbind(doc_id, text), stringsAsFactors = FALSE)
  return(result)
}
