
# Function to add plus and minus "contextSize" characters for a regex pattern
addContext <- function(extracted, contextSize) {
  if (length(extracted) > 0) {
    newSearchR  <- extracted %>%
      stringr::str_replace_all("\\.", "\\\\.") %>%
      stringr::str_replace_all("\\(", "\\\\(") %>%
      stringr::str_replace_all("\\)", "\\\\)") %>%
      paste0(".{0,", contextSize, "}",  . , ".{0,", contextSize, "}")
  }
  else {
    NA
  }
}

# For this to work it needs to be fed a single string
# function to extract text, remove whitespaces and unicode encodings of the
# minus sign and return test statistic original data plus df
extractTestStats <- function(inputText, context = FALSE, contextSize = 100, sectionName = NA) {

  if (purrr::is_empty(inputText) | !elementExists(inputText)) {
    if (context == F) {
      return(
        tibble::tibble(
          statistic = NA,
          cleaned = NA,
          reported = NA,
          value = NA,
          df1 = NA,
          df2  = NA,
          p = NA
        )
      )
    } else {
      return(
        tibble::tibble(
          statistic = NA,
          cleaned = NA,
          reported = NA,
          context = NA,
          value = NA,
          df1 = NA,
          df2  = NA,
          p = NA
        )
      )
    }
  }

  # Input should already be cleaned, this is important as it will break this function -  processText() on input
  # extracting all text which matches patterns

  # This replaces any "L"s which follow a number or an opening bracket with ones
  inputText <- stringr::str_replace_all(inputText, pattern = "(?<=(\\(\\s{0,2}|\\d\\s{0,2}|\\.\\s{0,2}))l", "1")
  # Also replacing if an l it is followed by a )
  inputText <- stringr::str_replace_all(inputText, pattern = "l(?=(\\s{0,2}\\)|\\d\\s{0,2}|\\.\\s{0,2}))", "1")
  
  
  # Running each of the extractor functions on the text 
  ts <- extractTTests(inputText)
  chis <- extractChiSquare(inputText)
  fs <-  extractFTests(inputText)
  rs <-  extractCorrelations(inputText)
  ESs <- extractES(inputText)
  
  extracted <- dplyr::bind_rows(ts ,
                                chis,
                                fs ,
                                rs ,
                                ESs)
  # The following extracts the context around each extracted part:
  # Putting the following into a single string that will expand to the number of patterns

  if(context == T) {
    patternsContext <- lapply(extracted$reported, addContext, contextSize = contextSize)
    # extracting discovered hits + context
    
    extractedContext <-
      stringr::str_extract(inputText, stringr::regex(
      as.character(patternsContext)))
        
    extracted$context <- unlist(extractedContext)
  }
  
  if(!is.na(sectionName)) {}
    
  return(extracted)
}
