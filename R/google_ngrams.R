#' This function extracts frequency data from Google Books' Ngram data:
#' http://storage.googleapis.com/books/ngrams/books/datasetsv2.html
#' The function is set up to facilitate the counting of lemmas
#' and ignore differences in capitalization.
#' The user has control over what to combine into counts with
#' the word_forms argument.
#'
#' NOTE!!! Google's data tables are HUGE. Sometime running into
#' multiple gigabytes for simple text files. Thus, depending
#' on the table being accessed, the return time can be slow.
#' For example, accessing the 1-gram Q file should take only a few seconds,
#' but the 1-gram T file might take 10 minutes to process.
#' The 2-gram, 3-gram, etc. files are even larger and slower to process.
#'
#' @param word_forms A vector of words or phrases to be searched
#' @param variety The variety of English to be searched
#' @param by Whether the counts should be summed by year or by decade
#' @return A data.frame of counts from Google Books
#' @export
google_ngram <- function(word_forms, variety=c("eng", "gb", "us", "fiction"), by=c("year", "decade")){
  word_forms <- stringr::str_replace_all(word_forms, "([a-zA-z0-9])-([a-zA-z0-9])", "\\1 - \\2")
  word_forms <- stringr::str_squish(word_forms)
  n <- lapply(word_forms, function(x) stringr::str_count(x, "\\S+"))
  n <- unique(n)
  if(length(n) > 1)  stop ("Check spelling. Word forms should be lemmas of the same word (e.g. 'teenager' and 'teenagers' or 'walk' , 'walks' and 'walked'")
  if(n > 5)  stop ("Ngrams can be a maximum of 5 tokens. Hyphenated words are split and include the hyphen, so 'x-ray' would count as 3 tokens.")
  gram <- ifelse(n > 1, lapply(word_forms, function(x) substring(x, 1, 2)), lapply(word_forms, function(x) substring(x, 1, 1)))
  gram <- tolower(unique(gram))
  if(length(gram) > 1)  stop ("Check spelling. Word forms should be lemmas of the same word (e.g. 'teenager' and 'teenagers' or 'walk' , 'walks' and 'walked'")
  if(stringr::str_detect(gram, "^[a-z][^a-z]")) gram <- stringr::str_replace(gram, "[^a-z]", "_")
  if(stringr::str_detect(gram, "^[0-9]")) gram <- substring(gram, 1, 1)
  if(stringr::str_detect(gram, "^[[:punct:]]")) gram <- "punctuation"
  if(any(stringr::str_detect(substring(gram, 1, 1), c("ß", "æ", "ð", "ø", "ł", "œ", "ı", "ƒ", "þ", "ȥ", "ə", "ħ", "ŋ", "ª", "º", "ɣ", "đ", "ĳ", "ɔ", "ȝ", "ⅰ", "ʊ", "ʌ", "ʔ", "ɛ", "ȡ", "ɋ", "ⅱ", "ʃ", "ɇ", "ɑ", "ⅲ")))) gram <- "other"
  gram <- stringi::stri_trans_general(gram, "Latin-ASCII")
  
  if(variety == "eng") repo <- paste0("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-", n, "gram-20120701-", gram, ".gz")
  if(variety != "eng") repo <- paste0("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-", variety, "-all-", n, "gram-20120701-", gram, ".gz")
  
  message("Accessing repository. For larger ones (e.g., ngrams containting 2 or more words) this may take a few minutes. A progress bar should appear shortly...")
  
  word_forms <- stringr::str_replace_all(word_forms, "(\\.|\\?|\\$|\\^|\\)|\\(|\\}|\\{|\\]|\\[|\\*)", "\\\\\\1")
  grep_words <- paste0("^", word_forms, "$", collapse = "|")
  all_grams <- suppressWarnings(readr::read_tsv_chunked(repo, col_names = FALSE, col_types = list(X1 = readr::col_character(), X2 = readr::col_double(), X3 = readr::col_double(), X4 = readr::col_double()), quote = "", callback = readr::DataFrameCallback$new(function(x, pos) subset(x, grepl(grep_words, x$X1, ignore.case=TRUE))), progress = T))
  colnames(all_grams) <- c("token", "Year", "AF", "pages")
  
  if(variety == "eng") total_counts <- ngramr.plus::googlebooks_eng_all_totalcounts_20120701
  if(variety == "gb") total_counts <- ngramr.plus::googlebooks_eng_gb_all_totalcounts_20120701
  if(variety == "us") total_counts <- ngramr.plus::googlebooks_eng_us_all_totalcounts_20120701
  
  if(by == "year") total_counts <- aggregate(Total ~ Year, total_counts, sum)
  if(by == "decade") total_counts$Decade <- gsub("\\d$", "0", total_counts$Year)
  if(by == "decade") total_counts <- aggregate(Total ~ Decade, total_counts, sum)

  all_grams$token <- tolower(all_grams$token)
  sum_tokens <- aggregate(AF ~ Year, all_grams, sum)
  
  if(by == "decade") sum_tokens$Decade <- gsub("\\d$", "0", sum_tokens$Year)
  if(by == "decade") sum_tokens <- aggregate(AF ~ Decade, sum_tokens, sum)
  if(by == "decade") sum_tokens <- merge(sum_tokens, y = total_counts[,c(1:2)], by = "Decade")
  if(by == "decade") sum_tokens$Decade <- as.numeric(sum_tokens$Decade)
  if(by == "year") sum_tokens <- merge(sum_tokens, y = total_counts[,c(1:2)], by = "Year")
  
  counts_norm <- mapply(function(x,y) (x/y)*1000000, sum_tokens$AF, sum_tokens$Total)
  sum_tokens$Per_10.6 <- counts_norm
  return(sum_tokens)
}
