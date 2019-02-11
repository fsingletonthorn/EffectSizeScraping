## Patterns extracted - strip commas and whitespace? too

# F and T tests - 
# Need to add a negative look behind for both to ensure that there are no letters beforehand

patternT <- "t\\s{0,}\\(\\s{0,}\\d{1,}\\.?\\d{0,}\\s{0,}\\)\\s{0,}=\\s{0,}-?\\s{0,}\\d{0,}\\.?\\d{0,}(\\s{0,},?\\s{0,}p\\s{0,}[<>=]\\s{0,}\\d?\\.\\d+e?-?\\d*)?"
patternF <-  "F\\s{0,}\\(\\s{0,}\\d{1,},\\s{0,}\\d{1,}\\s{0,}\\)\\s{0,}=\\s{0,}\\d{0,}\\.?\\d{0,}(\\s{0,},?\\s{0,}p\\s{0,}[<>=]\\s{0,}\\d?\\.\\d+e?-?\\d*)?"
patternR <-  "((([r]\\s{0,}\\(?\\s{0,}(df|n)?\\s{0,}[=]?\\s{0,}\\d{0,10}\\s{0,}\\)?\\s{0,}[=]\\s{0,})|((correlation)\\s?\\s?(coefficient)?\\s?\\s?([=]|(of))\\s?\\s?))(\u2212?\\-?\\s?\\d*\\.?\\d{1,}))(\\s{0,},?\\s{0,}p\\s{0,}[<>=]\\s{0,}\\d?\\.\\d+e?-?\\d*)?"
patterns <- c(patternT, patternF, patternR)

patternD <- "(?<=((\\s|\\()[dg]\\s?\\s?(//(95% confidence interval//))?(//(95% CI//))?)\\s?\\s?[\\.\\,\\:\\;]?\\s?\\s?([=]|(of))\\s?\\s?)(\\-?\u2212?\\s?\\d*\\.?\\d{1,})"
patternR <-  "(((?<=((\\s|\\(|\\[)([r]s?\\(?\\d{0,10}\\)?\\s?\\s?[=]\\s?\\s?)))|((?<=(correlation)\\s?\\s?(coefficient)?\\s?\\s?([=]|(of))\\s?\\s?)))(\u2212?\\-?\\s?\\d*\\.?\\d{1,}))"
patternEta <- "(((?<=((\\s|\\())([\u03B7]\\s?p?\\s?2?\\s?\\s?([=]|(of))\\s?\\s?))|((?<=((partial)?\\s?eta\\ssquared\\s?\\=?(of)?\\s?))))(\\-?\u2212?\\s?\\d*\\.?\\d{1,}))" 
patternP <- "(?<=(p\\s{0,4}))([=\\<\\>]\\s?\\s?\\-?\u2212?\\s?\\d*\\.?\\d{1,})" 
patternHR <- "((?<=((\\s|\\()((HR)|(hazzard.ratio))\\s{0,4}((of)|(=))\\s{0,4}))(\\-?\u2212?\\s?\\d*\\.?\\d{1,}))"
patternOR <- "((?<=((\\s|\\()((OR)|(odd.{1,3}?ratio))\\s{0,4}((of)|(=))\\s{0,4}))(\\-?\u2212?\\s?\\d*\\.?\\d{1,}))"



# For this to work it needs to be fed a single string 
# function to extract text, remove whitespaces and unicode encodings of the minus sign and return test statistic original data plus df
extractTestStats <- function(inputText, context = FALSE, contextSize = 100) {
  # removing newline breaks, non-breaking spaces, '&#x000a0;', &#x00026;
  inputTextCleaned <- str_remove_all(inputText, "\\n")
  inputTextCleaned <- str_remove_all(inputTextCleaned, "[Ââˆ\\’Ï„œ€$!\\“\u009d]")
  
  # replacing unicode minus sign with R recognised hyphen/minus sign
  inputTextCleaned <- str_replace_all(inputTextCleaned, "\u2212", "-")
  
  # extracting all text which matches patterns
  extracted <- str_extract_all(inputTextCleaned,  regex(patterns, ignore_case = TRUE))
  
  # removing whitespace that can be captured in the regular expressions above
  extractedClean <- lapply(extracted,  str_remove_all, pattern = "\\s")
  # Returing the right thing based on whether context was gathered 
  
  # The following extracts the context around each extracted part:
  # WILL ALSO HAVE TO REMOVE NEGATIVE LOOK AROUNDS IF THEY ARE ADDED !!! 
  if(context == T) { 
    if(length(extracted[[1]]) > 0){
      newSearchT  <- extracted[[1]] %>%
        str_replace_all("\\.", "\\\\.") %>%
        str_replace_all("\\(", "\\\\(") %>%
        str_replace_all("\\)", "\\\\)") %>% 
        paste0(".{0,", contextSize,"}",  . ,".{0,", contextSize,"}")
      
      extractedContextT <- as.character(str_extract(inputTextCleaned,  regex(newSearchT, ignore_case = TRUE)))
    } else {extractedContextT <- list()}
    
    if(length(extracted[[2]]) > 0){
      newSearchF  <- extracted[[2]] %>%
        str_replace_all("\\.", "\\\\.") %>%
        str_replace_all("\\(", "\\\\(") %>%
        str_replace_all("\\)", "\\\\)") %>% 
        paste0(".{0,", contextSize,"}",  . ,".{0,", contextSize,"}")
      
      extractedContextF <- as.character(str_extract(inputTextCleaned,  regex(newSearchF, ignore_case = TRUE)))
    } else {extractedContextF <- list()}
    
    if(length(extracted[[3]]) > 0){
      newSearchR  <- extracted[[3]] %>%
        str_replace_all("\\.", "\\\\.") %>%
        str_replace_all("\\(", "\\\\(") %>%
        str_replace_all("\\)", "\\\\)") %>% 
        paste0(".{0,", contextSize,"}",  . ,".{0,", contextSize,"}")
      
      extractedContextR <- as.character(str_extract(inputTextCleaned,  regex(newSearchR, ignore_case = TRUE)))
    }  else {extractedContextR <- list()}
    
    extractedContextR <- as.character(str_extract(inputTextCleaned,  regex(newSearchR, ignore_case = TRUE)))
    
    return(rbind(data_frame(statistic = "t", cleaned = extractedClean[[1]], reported = extracted[[1]], context = extractedContextT),
                 data_frame(statistic = "F", cleaned = extractedClean[[2]], reported = extracted[[2]],  context = extractedContextF),
                 data_frame(statistic = "r", cleaned = extractedClean[[3]], reported = extracted[[3]],  context = extractedContextR)))
  } else {
    return(rbind(data_frame(statistic = "t", cleaned = extractedClean[[1]], reported = extracted[[1]]),
                 data_frame(statistic = "F", cleaned = extractedClean[[2]], reported = extracted[[2]]),
                 data_frame(statistic = "r", cleaned = extractedClean[[3]], reported = extracted[[3]])))  
  }
}
