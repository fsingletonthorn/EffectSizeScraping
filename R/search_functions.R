# Functions that search text for CIs, Power analysis, that attempt to extract sample sizes



# Check for 95% CIs
checkCIs <- function(input,
                     context = T,
                     contextSize = 100) {
  ciRegex  <- "((CI|Confidence Interval)\\s\\[\\d*,\\s*\\d*\\])"
  
  CIreg <-
    paste(
      paste0(".{0,", contextSize, "}((\\b\\d{1,2}%\\s*CI\\b)"),
      ciRegex,
      paste0("(confidence\\s*interval)).{0,", contextSize, "}"),
      sep = "|",
      collapse = "|"
    )
  
  CIs <- stringr::str_extract_all(input,
                                  stringr::regex(CIreg,
                                                 ignore_case = T))
  
  CIsBinary <- stringr::str_detect(input,
                                   stringr::regex(CIreg,
                                                  ignore_case = T))
  if (context == T) {
    return(list(CIs, CIsBinary))
  } else {
    return(CIsBinary)
  }
}

### This does not work yet be careful
findN <- function(input,
                  context = T,
                  contextSize = 100) {
  
  input <- textclean::num
  findNReg <-  ""

  paste(
    paste0(".{0,", contextSize, "}((\\b\\d{1,2}%\\s*CI\\b)"),
    
    paste0("(confidence\\s*interval)).{0,", contextSize, "}"),
    sep = "|",
    collapse = "|"
  )
}


