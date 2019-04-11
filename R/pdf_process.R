# # pdf extract
# library(pdftools)
# library(stringr)
# library(stringi)

true_false <- function(x, chars) {
  x > chars
}

splitPdf <- function(x, pattern = "\\p{WHITE_SPACE}{3,}") {
  # This function is slightly adapted from pdfsearch - https://github.com/lebebr01/pdfsearch/blob/master/R/split_pdf.r
  x_lines <- stringi::stri_split_lines(x)
  x_lines <- lapply(x_lines, gsub,
                    pattern = "^\\s{1,20}",
                    replacement = "")
  
  x_page <- lapply(
    x_lines,
    stringi::stri_split_regex,
    pattern = pattern,
    omit_empty = NA,
    simplify = TRUE
  )
  
  page_lines <- unlist(lapply(x_page, nrow))
  columns <- unlist(lapply(x_page, ncol))
  
  num_chars <- lapply(x_page, nchar)
  num_chars_tf <- lapply(num_chars, true_false, chars = 3)
  
  for (xx in seq_along(num_chars_tf)) {
    num_chars_tf[[xx]][is.na(num_chars_tf[[xx]])] <- FALSE
  }
  
  output <- lapply(seq_along(x_page), function(xx)
    x_page[[xx]][num_chars_tf[[xx]]])
  
  output <- lapply(output, concatPlus)
  return(output)
}

extractPdf <- function(path) {

extractedText <- pdftools::pdf_text(path)

splitPdf(extractedText)

}
