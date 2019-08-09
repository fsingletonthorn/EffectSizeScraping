# Concatinate function that is just a "better" paste0
concat <- function(text) {
  if (length(text) > 1) {
    return(stringr::str_flatten(text))
  } else {
    return(text)
  }
}

# concatinate plus function that is just a "better" paste
concatPlus <- function(text) {
  if (length(text) > 1) {
    return(stringr::str_flatten(text, collapse = " "))
  } else {
    return(text)
  }
}

# Check whether element exists and is not NA
# (or at least that the first element of an object is not NA)
elementExists <- function( full_index_path ){
  tryCatch({
    len_element = length(full_index_path)
    if(is.na(full_index_path)[[1]]) {return(F)}
    exists_indicator = ifelse(len_element > 0, T, F)
    return(exists_indicator)
  }, error = function(e) {
    return(F)
  })
}

# This function is adapted from statcheck https://github.com/MicheleNuijten/statcheck/blob/master/R/htmlImport.R,
# does some final extra cleaning if any tags / weird characters remain
processText <- function(strings){
  
  # Replace html codes:
  strings <- lapply(strings, gsub, pattern = "&#60;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&lt;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#61;", replacement = "=", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#62;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&gt;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#40;", replacement = "(", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#41;", replacement = ")", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&thinsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&nbsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "\n", replacement = "")
  strings <- lapply(strings, gsub, pattern = "\r", replacement = "")
  strings <- lapply(strings, gsub, pattern = "\\s+", replacement = " ")
  strings <- lapply(strings, gsub, pattern = "&minus;", replacement = "-", fixed = TRUE)
  
  # removing newline breaks, non-breaking spaces, '&#x000a0;', &#x00026;
  strings <- lapply(strings, gsub, pattern = "[Ââˆ\\’Ï„œ€$!\\“\u009d]", replacement = " ")
  # replacing unicode minus sign with R recognised minus sign
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\u2212", replacement = "-")
  # replcaing unicode short spaces that are not always picked up above
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\u2009", replacement = " ")
  # replcaing mathematical symbols with interpretable ones 
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d443", replacement = "p")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d45b", replacement = "n")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d6fd", replacement = "beta")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d6fc", replacement = "alpha")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d712", replacement = "chi")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d712", replacement = "chi")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\u2439", replacement = "chi")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U00002AFD", replacement = "=")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\u2b0d", replacement = "<")  
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\ufb01", replacement = "fi")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\ufb00", replacement = "ff")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\ufb02", replacement = "fl")
  
  return(strings)
}
