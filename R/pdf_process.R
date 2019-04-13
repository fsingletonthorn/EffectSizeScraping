# # pdf extract
# library(pdftools)
# library(stringr)
# library(stringi)

# Function to ID whether a cell is > chars, if empty  gives FALSE
true_false <- function(x, chars) {
  output <- x > chars
  output <- ifelse( !is.na(output), output, FALSE ) 
}

# concatinate plus function that is just a "better" paste
concatPlus <- function(text) {
  if (length(text) > 1) {
    return(stringr::str_flatten(text, collapse = " "))
  } else {
    return(text)
  }
}

splitPdf <- function(x, pattern = "\\p{WHITE_SPACE}{3,}") {
  # This function is slightly adapted from pdfsearch - https://github.com/lebebr01/pdfsearch/blob/master/R/split_pdf.r
  x_lines <- stringi::stri_split_lines(x)
  # Removes white space at the begining of the line 
  x_lines <- lapply(x_lines, gsub,
                    pattern = "^\\s{1,20}",
                    replacement = "")
  
  # Splitting into list of dataframes containing columns and rows
  x_page <- lapply(
    x_lines,
    stringi::stri_split_regex,
    pattern = pattern,
    omit_empty = NA,
    simplify = TRUE
  )
  
  # Search for single words which match sections and label here
  ## insert code here 
  
  # number of lines per page 
  page_lines <- unlist(lapply(x_page, nrow))
  # Number of cols per page 
  columns <- ncol(x_page)
  columns <- unlist(lapply(x_page, ncol))
  
  # Number of 
  num_chars <- lapply(x_page, nchar)
  num_chars_tf <- lapply(num_chars, true_false, chars = 3)
  
  output <- lapply(seq_along(x_page), function(xx)
    x_page[[xx]][num_chars_tf[[xx]]])
  
  output <- lapply(output, concatPlus)
  return(output)
}

extractPdf <- function(path) {

extractedText <- pdftools::pdf_text(path)

splitPdf(extractedText)

}

# Figuring out how to extract headings from the file 
temp <- pdftools::pdf_text("https://osf.io/nztsx/download")
tempSplit <- splitPdf(temp)


