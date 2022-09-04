#' This is a helper function for reading in the dialogue from Shakespeare plays
#' and screenplays, which are formatted in very simple markup.
#' With this function, either the dialogue or the direction can be
#' execrated in bulk. It can also be used to extract the dialogue of specific characters.
#' @param plays A readtext dataframe containing a doc_id column and a text column.
#' @param extract A character vector specifying what is to be extracted like 'dialogue' or 'romeo'.
#' @return A readtext dataframe with the extracted text.
#' @export
from_play <- function(plays, extract = c("dialogue", "direction")) {
  
  if (missing(extract)) stop("You must specify whether you want to extract dialogue or direction.")
  
  extract <- tolower(extract)
  # A function for reading in the simplified markup and extracting all the text
  # between dialogue nodes.
  get_text <- function(x) {
    t <- xml2::read_html(x, options = "HUGE")
    d <- xml2::xml_find_all(t, paste0("//", extract))
    d <- xml2::xml_text(d)
    d <- stringr::str_squish(d)
    return(d)
  }
  
  idx <- 1:nrow(plays)
  plays$text <- sapply(idx, function(i) paste(get_text(plays$text[i]), collapse = "\n"))
  plays <- plays[plays$text != "",]
  return(plays)
}

