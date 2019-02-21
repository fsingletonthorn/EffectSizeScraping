# F and T test patterns -  
patternT <- "\\bt\\s*\\(\\s*\\d{1,}\\.?\\d*\\s*\\)\\s*=\\s*-?\\s*\\d*\\.?\\d*(\\s*,?\\s*p\\s*[<>=]\\s*\\d?\\.\\d+e?-?\\d*)?"
patternF <-  "\\bF\\s*\\(?\\s*\\d{1,},\\s*\\d{1,}\\s*\\)?\\s*=\\s*\\d*\\.?\\d*(\\s*,?\\s*p\\s*[<>=]\\s*\\d?\\.\\d+e?-?\\d*)?"
patternR <-  "\\b(((r(pb)?(?!2)\\s*\\(?\\s*(df|n)?\\s*[=]?\\s*\\d{0,10}\\s*\\)?\\s*[=]\\s*)|((correlation)\\s*(coefficient)?\\s*([=]|(of))\\s*))(\u2212?\\-?\\s?\\d*\\.?\\d{1,}))(\\s*,?\\s*p\\s*[<>=]\\s*\\d?\\.\\d+e?-?\\d*)?"
patternChiSq <- "\\b(chi|\\u03C7|\\u1D712|\\u1D61|\\u1D6a|\\u1D712|\\u1D74C|\\u1D86|\\u1D7C0)\\s*(2|square|squared)?\\s*\\(?\\s*(df|n)?\\s*[=]?\\s*\\d*\\s*\\,?\\s*(df|n)?\\s*\\=?\\s*\\d*\\s*\\)?\\s*([=]|(of))\\s*-?\\s*\\d*\\.?\\d*(\\s*,?\\s*p\\s*[<>=]\\s*\\d?\\.\\d+e?-?\\d*)?"
patternD <- "\\b[dg]\\s*[\\.\\,\\:\\;]?\\s*([=]|(of))\\s*(\\-?\\s*\\d*\\.?\\d{1,})"
patternEta <- "\\b([(partial\\s*)?\u03B7|(eta)]\\s*p?\\s*2?(squared)?\\s*)\\s*([=]|(of))\\s*(\\-?\\s*\\d*\\.?\\d{1,})" 
patternHR <- "\\b((HR)|(hazzard.{1,3}?ratio))\\s*((of)|(=))\\s*(\\-?\\s*\\d*\\.?\\d{1,})"
patternOR <- "\\b((OR)|(odd.{1,3}?ratio))\\s*((of)|(=))\\s*(\\-?\\s*\\d*\\.?\\d{1,})"

# Do not change order of patterns, that will break everything 
patterns <- c(patternT, patternF, patternR, patternChiSq, patternD, patternEta, patternHR , patternOR)

str_extract_all( resultsText, patterns)
# helper function to split the output into constituate parts 
splitTestStatToDF <- function(statistic, cleanedTestStat) {
  if ((length(cleanedTestStat) > 0) & (length(statistic) > 0)) {
    testStatistic <-
      str_extract(str_split(cleanedTestStat, "=", simplify = T)[, 2],
                  "-?\\d*\\.?\\d*")
    df1 <-
      case_when(statistic == "F" ~ c(str_extract(cleanedTestStat, "\\d{1,}(?=,)")),
                TRUE ~ NA_character_)
    # Add other statistics here below in addition to F
    df2 <-
      case_when(
        statistic == "F" ~ str_extract(cleanedTestStat, "(?<=,)\\d{1,}"),
        # If people have reported r(n=x) return as n df2 = n - 2
        str_detect(statistic, "r") & str_detect(cleanedTestStat, "n=") ~ 
          as.character(as.numeric(
            str_extract(cleanedTestStat, "(?<=([a-zA-Z])\\(?(n\\=)?)\\d{1,}")
          ) - 2),
        str_detect(statistic, "T|t|r|R") ~ str_extract(cleanedTestStat, "(?<=([a-zA-Z])\\(?(df\\s{0,10}\\=\\s{0,10})?)\\d{1,}"),
        str_detect(statistic, "chi") ~ str_extract(cleanedTestStat, "(?<=\\((df\\s{0,10}=\\s{0,10})?)\\d{1,}"),
        TRUE ~ NA_character_
      )
    p <-
      str_extract(cleanedTestStat, "(?<=(p=?))[<>]?\\d?\\.\\d+e?-?\\d*")
    data_frame(
      value = testStatistic,
      df1 = df1,
      df2 = df2,
      p = p
    )
  } else
    NA_real_
}

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
 
  if(is_empty(inputText) | !elementExists(inputText)) { 
    if(context == F) { 
      return(data_frame(statistic = NA, cleaned = NA, reported = NA, value = NA,  df1 = NA, df2  = NA, p = NA))
    } else {
      return(data_frame(statistic = NA, cleaned = NA, reported = NA, context = NA, value = NA,  df1 = NA, df2  = NA, p = NA))
    }
  } 
  
  # Input should already be cleaned, this is important as it will break this function -  processHTML() on input
  # extracting all text which matches patterns

  
  extracted <- str_extract_all(inputText , pattern = regex(patterns, ignore_case = TRUE))
  
  # removing whitespace that can be captured in the regular expressions above
  extractedClean <- lapply(extracted,  str_remove_all, pattern = "\\s")
  
  # The following extracts the context around each extracted part:
  # Putting the following into a single string that will expand to the number of patterns
   
  if(context == T) {
    patternsContext <- lapply(extracted, addContext, contextSize = contextSize)
    # extracting discovered hits + context 
    extractedContext <- lapply(patternsContext, function(patternsContext)  
      unlist(str_extract(inputText, regex(as.character(patternsContext), ignore_case = T))))
    # returning all of this 
    statisticalOutput <- rbind(data_frame(statistic = "t",   cleaned = extractedClean[[1]], reported = extracted[[1]], context = extractedContext[[1]]),
                 data_frame(statistic = "F",   cleaned = extractedClean[[2]], reported = extracted[[2]],  context = extractedContext[[2]]),
                 data_frame(statistic = "r",   cleaned = extractedClean[[3]], reported = extracted[[3]],  context = extractedContext[[3]]),
                 data_frame(statistic = "chi", cleaned = extractedClean[[4]], reported = extracted[[4]],  context = extractedContext[[4]]),
                 data_frame(statistic = "d",   cleaned = extractedClean[[5]], reported = extracted[[5]],  context = extractedContext[[5]]),
                 data_frame(statistic = "eta", cleaned = extractedClean[[6]], reported = extracted[[6]],  context = extractedContext[[6]]),
                 data_frame(statistic = "HR",  cleaned = extractedClean[[7]], reported = extracted[[7]],  context = extractedContext[[7]]),
                 data_frame(statistic = "OR",  cleaned = extractedClean[[8]], reported = extracted[[8]],  context = extractedContext[[8]]))
           
    } else {
    statisticalOutput <- rbind(data_frame(statistic = "t", cleaned = extractedClean[[1]], reported = extracted[[1]]),
                 data_frame(statistic = "F", cleaned = extractedClean[[2]], reported = extracted[[2]]),
                 data_frame(statistic = "r", cleaned = extractedClean[[3]], reported = extracted[[3]]),
                 data_frame(statistic = "chi", cleaned = extractedClean[[4]], reported = extracted[[4]]),
                 data_frame(statistic = "d",   cleaned = extractedClean[[5]], reported = extracted[[5]]),
                 data_frame(statistic = "eta", cleaned = extractedClean[[6]], reported = extracted[[6]]),
                 data_frame(statistic = "HR",  cleaned = extractedClean[[7]], reported = extracted[[7]]),
                 data_frame(statistic = "OR",  cleaned = extractedClean[[8]], reported = extracted[[8]]))
    }
 if(is_empty(statisticalOutput[[1]]) |  (nrow(statisticalOutput) < 1)) { 
   if(context == F) { 
     data_frame(statistic = NA, cleaned = NA, reported = NA, value = NA,  df1 = NA, df2  = NA, p = NA) 
     } else {
     data_frame(statistic = NA, cleaned = NA, reported = NA, context = NA, value = NA,  df1 = NA, df2  = NA, p = NA)
     }
   } else {
   cbind(statisticalOutput, splitTestStatToDF(statistic = statisticalOutput$statistic, cleanedTestStat = statisticalOutput$cleaned), stringsAsFactors = F)
  }
}


