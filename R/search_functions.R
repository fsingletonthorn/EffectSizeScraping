# Functions that search text for CIs, Power analysis, that attempt to extract sample sizes

# Check for 95% CIs
checkCIs <- function(input,
                     context = T,
                     contextSize = 0) {
  CI_REGEX  <- "((\\d{1,2}%?)?\\s*(\\bCIs?|\\bconfidence\\s*intervals?))\\s*(of)?=?\\s*\\[?\\d*,?\\s*\\d*\\]?"
  
  CIregContext <-
      paste0(".{0,", contextSize, "}",
      CI_REGEX, ".{0,", contextSize, "}")
  
  # Extract CIs with context
  CIs_context <- stringr::str_extract_all(input,
                                  stringr::regex(CIregContext,
                                                 ignore_case = T), 
                                  simplify = T)
  
  # If CIs is empty, CIs are not captured
  CIsBinary <- elementExists(CIs_context)
  
  if(CIsBinary) {
  # Extract just CIs
  CIs <- stringr::str_extract_all(CIs_context,
                                  stringr::regex(CI_REGEX,
                                                 ignore_case = T))
  } else {CIs <- character(0)}
  
  if (context == T) {
    return(list(CIs, CIs_context, CIsBinary))
  } else {
    return(CIsBinary)
  }
}

findN <- function(input) {
  
  # Converting words to numbers for sample size extraction
  inputProcessed <-  words_to_numbers( input )
  
  # At the moment this does not pull out context 
  
  # Setting up regexs
  SSOf <- "(\\ba?\\s*sample\\s*size\\s*of\\s*\\d+)"
  nOf <- "((\\bN\\s*(\\=|(of))\\s*)\\d+)"
  nPar <- "(\\b\\d+((?!\\.|\\d).)*(participants|participated|volunteer(s|ed)|observations))"
  parWere <- "(\\b(subjects|participants)((?!\\.|\\d).)*\\s*were((?!\\.|\\d).)*\\d+)"
  
  # pullting together regex 
  NRegex <-  paste(SSOf, nOf, nPar, parWere , sep = "|")

  # Sample size strings
  sampleSizeStrings <- stringr::str_extract_all(inputProcessed,
                                                stringr::regex(NRegex, ignore_case = T), 
                                                simplify = T)
  
  if(elementExists(sampleSizeStrings)) {
  sampleSizes <- stringr::str_extract_all(sampleSizeStrings, "\\d{1,}", 
                                          simplify = T)
  } else {sampleSizes <- character(0)}
  
  return(list(string = sampleSizeStrings, N = sampleSizes))
}
# 
# findPowerAnalysis <- {
#   stringr::str_extract_all()
#   }
