#' Extract t tests from text
#' 
#' @param inputText input text 
#' @param contextSize = size of context around extracted test statistics
#' @return A tibble contaitning all detected ttest coefficents, 
#' associated degrees of freedom and p values, with context of size contextSize.
#' 
#' 
#' @examples
#' extractTTests("t(123) = .01, p = < .001")
#' 
#' 

extractTTests <- function(input) {

  # Setting up all possible parts of the regex
  ttestRegex  <- "(\\bt\\b)"
  
  numbericRegex_commas <- "(((\\d{1,3}(?=,)(,\\d{3})*)|\\d+)?)"
  
  numbericRegex_commasdecimals <- "(((\\d{1,3}(?=,)(,\\d{3})*)|\\d+)(\\.\\d+)?)"
  
  degreesOfFreedomRegex_commas <- paste0("((?i)\\(?\\s*((df\\s*\\=?\\s*)|(n\\s*\\=\\s*))?\\s*",
                                         numbericRegex_commas,
                                         "\\s*\\)?(?-i))")
  ofOrEqualsRegex <- "((of)|=|:)"
  
  numbericBelow1Regex <- "((?<![1-9])\\.\\d+|0(\\.\\d+)?|(1\\.0*(?!(0*[1-9])))|((?<![0-9\\.])1(?![\\.0-9])))"
  # additional p value detector
  # additional p value detector
  pValueRegex <- "((?i)((\\s*,?\\s*)(ns))|(p\\s*[<>=(ns)]\\s*((ns)|(\\d?\\.?\\d+e?-?\\d*)|(\\.\\d+)))(?-i))"
  
  # Additional df = detector
  # Additional n = detector
  
  
  ttestExtractionRegex <- paste0(
    ttestRegex,
    # Allowing spaces
    "\\s{0,5}",
    degreesOfFreedomRegex_commas,
    # Making degrees of freedom optional
    "?",
    # Allowing spaces
    "\\s*",
    ofOrEqualsRegex,
    # Allowing spaces or negatives
    "\\s*",
    "\\-?",
    "\\s*",
    numbericRegex_commasdecimals,
    "\\s*",
    "\\;?",
    ",?",
    "\\s*",
    pValueRegex, 
    "?" # Making p values optional
  ) 
  
  ### All values between 0 and 1 with decimals
  ### This one is for ttest coefficents
  detected_ttests <- unlist(stringr::str_extract_all(
    input, ttestExtractionRegex
  ))
  
  # Getting the look behind to extract test statistic values
  # TODO Just remove everything before the numbericBelow1Regex and use that
  
  value_with_p <- stringr::str_remove(
    detected_ttests,
    paste0(
      ttestRegex,
      # Allowing spaces
      "\\s*",
      degreesOfFreedomRegex_commas,
      # Making degrees of freedom optional
      "?",
      # Allowing spaces
      "\\s*",
      ofOrEqualsRegex,
      # Allowing spaces or negatives
      "\\s*"))
  
  
  value <- stringr::str_extract(
    value_with_p, 
    paste0("^",
           "(\\s*",
           "\\-?",
           "\\s*",
           numbericBelow1Regex, ")")
  )
  
  ps <-
    stringr::str_trim(
      stringr::str_extract(
        detected_ttests,
        pValueRegex
      ))
  
  cors_with_ns <- 
    stringr::str_detect(
      detected_ttests, 
      "((?!=[nt])n\\s*\\=\\s*)"
    )
  
  df2 <- 
    stringr::str_remove_all(
      stringr::str_extract(
        detected_ttests,
        paste0(
          "(?<=",
          ttestRegex,
          # Allowing spaces
          ")",
          "\\s{0,5}",
          degreesOfFreedomRegex_commas
        )),
      stringr::regex("[^0-9.]")
    )
  
  df2 <- ifelse(cors_with_ns, 
                as.numeric(df2)-2,
                df2
  )
  
  return(tibble::tibble(statistic = "r",
                        raw = stringr::str_trim(detected_ttests), 
                        df1 = NA,
                        df2 = df2,
                        p = unlist(ps),
                        value = unlist(value)
  ))
}
