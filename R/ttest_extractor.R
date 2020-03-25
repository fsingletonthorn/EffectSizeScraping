#' Extract t tests from text
#' 
#'  Extract t tests from text. 
#'  
#' @param inputText input text 
#' @return A tibble containing all extracted values, with columns identifying the type of statistic extracted, the reported test as reported in the text, the degrees of freedom, the p value and the value reported. 
#' 
#' 
#' @examples
#' extractTTests("t(123) = .01, p = .001")
#' 
#' 

extractTTests <- function(input) {

  # Setting up all possible parts of the regex
  ttestRegex  <- "(\\b(t|T))"
  
  numbericRegex_commas <- "(((\\d{1,3}(?=,)(,\\d{3}){0,99})|\\d{1,99})?)"
  
  numbericRegex_commas_decimals <- "((((\\d{1,3}(?=,)(,\\d{3}){0,99})|\\d{1,99})(\\.\\d{1,99})?)|(\\.\\d{1,99}))"
  
  degreesOfFreedomRegex_commas_decimals <- paste0("((?i)\\(?\\s{0,5}((df\\s{0,5}\\=?\\s{0,5})|(n\\s{0,5}\\=\\s{0,5}))?\\s{0,5}",
                                                  numbericRegex_commas_decimals,
                                         "\\s{0,5}\\)?(?-i))")
  ofOrEqualsRegex <- "((of)|=|:|;)"
  
  numbericBelow1Regex <- "((?<![1-9])\\.\\d{1,99}|0(\\.\\d{1,99})?|(1\\.0{0,99}(?!(0{0,99}[1-9])))|((?<![0-9\\.])1(?![\\.0-9])))"
  # additional p value detector
  # additional p value detector
  pValueRegex <- "((?i)((\\s{0,5},?\\s{0,5})(ns))|(p\\s{0,5}[<>=(ns):]\\s{0,5}((ns)|(\\d?\\.?\\d{1,99}e?-?\\d{0,99})|(\\.\\d{1,99})))(?-i))"
  
  # Additional df = detector
  # Additional n = detector
  
  
  ttestExtractionRegex <- paste0(
    ttestRegex,
    # Allowing spaces
    "\\s{0,5}",
    degreesOfFreedomRegex_commas_decimals,
    # Making degrees of freedom optional
    "?",
    # Allowing spaces
    "\\s{0,5}",
    ofOrEqualsRegex,
    # Allowing spaces or negatives
    "\\s{0,5}",
    "\\-?",
    "\\s{0,5}",
    numbericRegex_commas_decimals,
    # Allowing spaces
    "\\s{0,5}",
    "\\;?",
    ",?",
    "\\s{0,5}",
    paste0("((?i)df\\s*[\\=\\:\\;]\\s*", 
           numbericRegex_commas_decimals,
           "(?-i))?"
    ),
    "\\s{0,5}",
    "\\;?",
    ",?",
    "\\s{0,5}",
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
      "\\s{0,5}",
      degreesOfFreedomRegex_commas_decimals,
      # Making degrees of freedom optional
      "?",
      # Allowing spaces
      "\\s{0,5}",
      ofOrEqualsRegex,
      # Allowing spaces or negatives
      "\\s{0,5}"))
  
  
  value <- 
    stringr::str_remove_all(
    stringr::str_extract(
    value_with_p, 
    paste0("^",
           "(\\s{0,5}",
           "\\-?",
           "\\s{0,5}",
           numbericRegex_commas_decimals, ")")
  ), "\\s*")
  
  ps <-
    stringr::str_trim(
      stringr::str_extract(
        detected_ttests,
        pValueRegex
      ))
  
  cors_with_ns <- 
    stringr::str_detect(
      detected_ttests, 
      "((?!=[nt])n\\s{0,5}\\=\\s{0,5})"
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
          degreesOfFreedomRegex_commas_decimals
        )),
      stringr::regex("[^0-9.]")
    )
  
  
  df2 <- ifelse(df2 == "" | is.na(df2), stringr::str_remove_all(
                stringr::str_extract(detected_ttests,
                  paste0("(?i)df\\s*[\\=\\:\\;]\\s*", 
                         numbericRegex_commas_decimals,
                         "(?-i)"
                         )
                  ),
                stringr::regex("[^0-9.]")), 
                df2
  )
  
  return(
    tibble::tibble(
      statistic = "t",
      reported = stringr::str_trim(detected_ttests),
      df1 = NA,
      df2 = as.numeric(df2),
      p = unlist(ps),
      value = as.numeric(unlist(value))
    )
  )
}

