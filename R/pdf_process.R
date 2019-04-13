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

splitPdf <- function(x, pattern = "\\p{WHITE_SPACE}{3,}", labelSections = F) {
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
  

  #   # Label sections by titles in list 
  if (labelSections == T) {
    patterns <-
      paste(
        "methods",
        "results",
        "discussion",
        "conclusion",
        "summary",
        "conclusion",
        "aims" ,
        "measure",
        "measures",
        "introduction",
        "abstract",
        sep = "|"
      )
    
    vectorOfText <-  do.call(c, output)
    
    titleLocations <- str_detect(
      str_remove_all(vectorOfText, pattern = "[:punct:]|\\d"),
      pattern = regex(paste0("^\\s*(", patterns, ")\\s*$"), ignore_case = T)
    )
    
    if(sum(titleLocations > 2)) {
    
    dataFrameText <- data.frame(text = vectorOfText, titles = 
                          ifelse(titleLocations, vectorOfText, NA),
                          stringsAsFactors = F)
    
    # Labeling with the last identified lable
    dataFrameText$titles <- zoo::na.locf(dataFrameText$titles,na.rm = FALSE)
    # 
    dataFrameText$titles[is.na(dataFrameText$titles)] <- "unlabelled"
    }
    outputLabled <- tapply(dataFrameText$text, concatPlus, INDEX = dataFrameText$titles)
  }
  
            # ["^(\\w*\\W*)?\\s*[abstract|introduction|Background|conclusion]\\s*(\\w*\\W*)?$"] )    stringr::regex(paste0("^\\s?\\w?\\s?", patterns, "\\s\\w?\\s?"),
 #  ignore_case = T)
  # 
  # 
  #   methodsNames <- "method|aims|measur"
  #   resultsNames <- "result"
  #   discussionNames <- "discussion|conclusion|conclud|summary"
  #   
  #     
  # }
  # Removing line break hyphons (words will still be separated by a space)
  # output <- lapply(x_page, stringr::str_remove, pattern =  "\\-\\Z")
  
  
  # Concatinating columns
  

  output <- lapply(output, concatPlus)
  
  return(output)
}

extractPdf <- function(path) {

extractedText <- pdftools::pdf_text(path)

splitPdf(extractedText)

}

# Figuring out how to extract headings from the file 
# temp <- pdftools::pdf_text("https://osf.io/nztsx/download")
# tempSplit <- splitPdf(temp)




