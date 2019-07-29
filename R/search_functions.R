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

### This does not work yet be careful
findN <- function(input,
                  context = T,
                  contextSize = 100) {
  
  # Converting words to numbers for sample size extraction
  inputProcessed <-  words_to_numbers( input )
  
  # Setting up regexs
  SSOf <- "(a?\\s*sample\\s*size\\s*of\\s*\\d+)"
  nOf <- "((N\\s*(\\=|(of))\\s*)\\d+)"
  nPar <- "(\\d+((?!\\.).)*(participants|participated|volunteer(s|ed)))|observations"
  parWere <- "((subjects|participants)((?!\\.).)*\\s*were((?!\\.).)*\\d+)"
  
  # pullting together regex 
  N_REGEX <-  paste(SSOf, nOf, nPar, parWere , sep = "|")
  
  # Sample size strings
  sampleSizeStrings <- stringr::str_extract_all(inputProcessed,
                                                stringr::regex(N_REGEX, ignore_case = T), 
                                                simplify = T)
  
  if(elementExists(sampleSizeStrings)) {
  sampleSizes <- stringr::str_extract_all(sampleSizeStrings, "\\d+", 
                                          simplify = T)
  } else {sampleSizes <- character(0)}
  
  return(list(string = sampleSizeStrings, N = sampleSizes))
  # paste(
  #   paste0(".{0,", contextSize, "}((\\b\\d{1,2}%\\s*CI\\b)"),
  #   
  #   paste0("(confidence\\s*interval)).{0,", contextSize, "}"),
  #   sep = "|",
  #   collapse = "|"
  # )
}


