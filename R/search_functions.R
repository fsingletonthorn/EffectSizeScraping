# Functions that search text for CIs, Power analysis, that attempt to extract sample sizes


# Check for 95% CIs
checkCIs <- function(input,
                     context = T,
                     contextSize = 100) {
  
CIreg <-
  paste(
    paste0(".{0,", contextSize, "}((\\b\\d{1,2}%\\s*CI\\b)"),
    "((CI|Confidence Interval)\\s\\[\\d*,\\s*\\d*\\])",
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




findN <- function(input,
                  context = T,
                  contextSize = 100) {
  
  findNReg <- 
  
  paste(
    paste0(".{0,", contextSize, "}((\\b\\d{1,2}%\\s*CI\\b)"),
    "((CI|Confidence Interval)\\s\\[\\d*,\\s*\\d*\\])",
    paste0("(confidence\\s*interval)).{0,", contextSize, "}"),
    sep = "|",
    collapse = "|"
  )
}

