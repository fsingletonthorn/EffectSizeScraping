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
  
  inputProcessed <-  words_to_numbers( input )
  # (\\s*sample\\s*size\\s*of\\s*\\d*)|(N\\s*(\\=|(of))\\s*)\\d*|(\\d*\\s*participants)|
  N_REGEX <-  "(\\d((?!\\.).)*participants)"
  
  stringr::str_extract_all(input, N_REGEX)
  
  
  # paste(
  #   paste0(".{0,", contextSize, "}((\\b\\d{1,2}%\\s*CI\\b)"),
  #   
  #   paste0("(confidence\\s*interval)).{0,", contextSize, "}"),
  #   sep = "|",
  #   collapse = "|"
  # )
}


