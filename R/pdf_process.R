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

splitPdf <- function(x, pattern = "(\\p{WHITE_SPACE}{3,})", labelSections = F) {
  # This function is slightly adapted from pdfsearch - 
  # https://github.com/lebebr01/pdfsearch/blob/master/R/split_pdf.r
  x_lines <- stringi::stri_split_lines(x)
  # Removes white space at the begining of lines
  x_lines <- lapply(x_lines, gsub,
                    pattern = "^\\s{1,20}",
                    replacement = "")
  
  # Splitting into list of dataframes containing columns as columns
  x_page <- lapply(
    x_lines,
    stringi::stri_split_regex,
    pattern = pattern,
    omit_empty = NA,
    simplify = TRUE
  )
  
  # counting number of lines and rows per page 
  page_lines <- unlist(lapply(x_page, nrow))
  columns <- unlist(lapply(x_page, ncol))
  
  # Figuring out whether the number of characters per cell > 0,
  # setting chars = 3 would get rid of most page numbers, but also could remove 
  # words, currently = 0.
  num_chars <- lapply(x_page, nchar)
  num_chars_tf <- lapply(num_chars, true_false, chars = 0)
  
  
  #  Removing empty cells and ordering the output
  output <- lapply(seq_along(x_page), function(xx)
    x_page[[xx]][num_chars_tf[[xx]]])
  
  
  # Label sections by their title, if the title is a single word on a line on 
  # its own  in the following list. 
  if(labelSections == T) {
    patterns <-
      paste(
        "abstract",
        # Introduction / aims
        "aims",
        "introduction",
        # Method
        "Material and methods",
        "method",
        "methods",
        "measure",
        "measures",
        "analysis",
        # Results / discussion / conclusion
        "results",
        "discussion",
        "conclusion",
        "Conclusions",
        "summary",
        "Results and Discussion",
        "discussion and results",
        # "study" is not a section, but does indicate new study is being introed
        "Study",
        "experiment", 
        # references
        "references",
        "bibliography",
        # "Acknowledgments" author statement etc. Will also be discarded 
        "Acknowledgments",
        "Conflict of Interests",
        "Conflict of Interest",
        "Conflict of Interest statement",
        sep = "|"
      )
    
# NOTE THIS CURRENTLY READS THE TEXT IN THE WRONG ORDER - NEED TO FIX !!! e.g., call on: https://osf.io/nztsx/, abstract is read as part of introduction
    
    vectorOfText <-  do.call(c, output)
    
    titleLocations <- stringr::str_detect(
      stringr::str_remove_all(vectorOfText, pattern = "[:punct:]|\\d"),
      pattern = stringr::regex(paste0("^\\s*(", patterns, ")\\s*$"), ignore_case = T)
    )

    dataFrameText <- data.frame(text = vectorOfText, titles = 
                          ifelse(titleLocations, vectorOfText, NA),
                          stringsAsFactors = F)
    
    # Labeling with the last identified lable
    dataFrameText$titles <- zoo::na.locf(dataFrameText$titles,na.rm = FALSE)
    # 
    dataFrameText$titles[is.na(dataFrameText$titles)] <- "unlabelled"
    
    outputLabeled <- tapply(dataFrameText$text, 
                           FUN = concatPlus, 
                           INDEX = dataFrameText$titles, 
                           simplify = T)
    
    return(tibble::tibble(names = row.names(outputLabeled), 
                      text = outputLabeled))
  }

  # Concatinating columns
  
  output <- lapply(output, concatPlus)
  
  return(tibble::tibble(names = 1:length(output), text = output))
}

extractPdf <- function(path) {

extractedText <- pdftools::pdf_text(path)

extractedText <- splitPdf(extractedText, labelSections = T)

extractedText$text <- unlist(processText(extractedText$text))

return(extractedText)

}
