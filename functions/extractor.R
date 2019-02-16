
# F and T test patterns -  
patternT <- "\\bt\\s*\\(\\s*\\d{1,}\\.?\\d*\\s*\\)\\s*=\\s*-?\\s*\\d*\\.?\\d*(\\s*,?\\s*p\\s*[<>=]\\s*\\d?\\.\\d+e?-?\\d*)?"
patternF <-  "\\bF\\s*\\(\\s*\\d{1,},\\s*\\d{1,}\\s*\\)\\s*=\\s*\\d*\\.?\\d*(\\s*,?\\s*p\\s*[<>=]\\s*\\d?\\.\\d+e?-?\\d*)?"
patternR <-  "\\b(((r(?!2)\\s*\\(?\\s*(df|n)?\\s*[=]?\\s*\\d{0,10}\\s*\\)?\\s*[=]\\s*)|((correlation)\\s*(coefficient)?\\s*([=]|(of))\\s*))(\u2212?\\-?\\s?\\d*\\.?\\d{1,}))(\\s*,?\\s*p\\s*[<>=]\\s*\\d?\\.\\d+e?-?\\d*)?"

patternD <- "\\b[dg]\\s*[\\.\\,\\:\\;]?\\s*([=]|(of))\\s*(\\-?\\s*\\d*\\.?\\d{1,})"
patternEta <- "\\b([\u03B7]\\s*p?\\s*2?\\s*)|((partial)?\\s*eta\\s*squared)\\s*([=]|(of))\\s*(\\-?\\s*\\d*\\.?\\d{1,})" 
patternHR <- "\\b((HR)|(hazzard.{1,3}?ratio))\\s*((of)|(=))\\s*(\\-?\\s*\\d*\\.?\\d{1,})"
patternOR <- "\\b((OR)|(odd.{1,3}?ratio))\\s*((of)|(=))\\s*(\\-?\\s*\\d*\\.?\\d{1,})"

# Do not change order of patterns, that will break everything 
patterns <- c(patternT, patternF, patternR, patternD, patternEta, patternHR , patternOR)
 
 ORStrings <- c(
    "odds ratio = 1.3", 
    "odds ratio of 1000.1",
    "OR = 1.2",
    "hazzard ratio of - .1",
    "d = 1.3",
    "a cohen's d of 1.324", 
    "eta squared of 1021.123",
    "partial eta squared of 12.23")
str_extract_all(ORStrings, regex(patternEta, ignore_case = T))


# Function to add + and minus "contextSize" characters for a regex pattern
addContext <- function(extracted, contextSize) {
  if( length(extracted) > 0) {
  newSearchR  <- extracted %>%
   str_replace_all("\\.", "\\\\.") %>%
   str_replace_all("\\(", "\\\\(") %>%
   str_replace_all("\\)", "\\\\)") %>% 
  paste0(".{0,", contextSize,"}",  . ,".{0,", contextSize,"}")
  }
  else {
    NA
  }
}


# For this to work it needs to be fed a single string 
# function to extract text, remove whitespaces and unicode encodings of the minus sign and return test statistic original data plus df
extractTestStats <- function(inputText, context = FALSE, contextSize = 100) {
  # removing newline breaks, non-breaking spaces, '&#x000a0;', &#x00026;
  inputTextCleaned <- str_remove_all(inputText, "\\n")
  inputTextCleaned <- str_remove_all(inputTextCleaned, "[Ââˆ\\’Ï„œ€$!\\“\u009d]")
  
  # replacing unicode minus sign with R recognised hyphen/minus sign
  inputTextCleaned <- str_replace_all(inputTextCleaned, "\u2212", "-")
  
  # extracting all text which matches patterns
  extracted <- str_extract_all(inputTextCleaned , pattern = regex(patterns, ignore_case = TRUE))
 
  # removing whitespace that can be captured in the regular expressions above
  extractedClean <- lapply(extracted,  str_remove_all, pattern = "\\s")
  
  # The following extracts the context around each extracted part:
  # Putting the following into a single string that will expand to the number of patterns
   
  if(context == T) {
    patternsContext <- lapply(extracted, addContext, contextSize = contextSize)
    # extracting discovered hits + context 
    extractedContext <- lapply(patternsContext, function(patternsContext)  
      unlist(str_extract_all(inputTextCleaned, regex(as.character(patternsContext), ignore_case = T))))
    # returning all of this 
    return(rbind(data_frame(statistic = "t",   cleaned = extractedClean[[1]], reported = extracted[[1]], context = extractedContext[[1]]),
                 data_frame(statistic = "F",   cleaned = extractedClean[[2]], reported = extracted[[2]],  context = extractedContext[[2]]),
                 data_frame(statistic = "r",   cleaned = extractedClean[[3]], reported = extracted[[3]],  context = extractedContext[[3]]),
                 data_frame(statistic = "d",   cleaned = extractedClean[[4]], reported = extracted[[4]],  context = extractedContext[[4]]),
                 data_frame(statistic = "eta", cleaned = extractedClean[[5]], reported = extracted[[5]],  context = extractedContext[[5]]),
                 data_frame(statistic = "HR",  cleaned = extractedClean[[6]], reported = extracted[[6]],  context = extractedContext[[6]]),
                 data_frame(statistic = "OR",  cleaned = extractedClean[[7]], reported = extracted[[7]],  context = extractedContext[[7]])))
    } else {
    return(rbind(data_frame(statistic = "t", cleaned = extractedClean[[1]], reported = extracted[[1]]),
                 data_frame(statistic = "F", cleaned = extractedClean[[2]], reported = extracted[[2]]),
                 data_frame(statistic = "r", cleaned = extractedClean[[3]], reported = extracted[[3]]),
                 data_frame(statistic = "d",   cleaned = extractedClean[[4]], reported = extracted[[4]]),
                 data_frame(statistic = "eta", cleaned = extractedClean[[5]], reported = extracted[[5]]),
                 data_frame(statistic = "HR",  cleaned = extractedClean[[6]], reported = extracted[[6]]),
                 data_frame(statistic = "OR",  cleaned = extractedClean[[7]], reported = extracted[[7]])))  
  }
}
 