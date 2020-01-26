#' Extract F tests from text
#' extractTestStats <- function(inputText, context = FALSE, contextSize = 100, sectionName = NA)
#' @param input input text 
#' @return A tibble contaitning all detected F tests, 
#' associated degrees of freedom and p values
#' 
#' @examples
#' extractFTests("F(1, 23) = .01, p = < .001")
#' 
#' 

extractFTests <- function(input) {
  # Setting up all possible parts of the regex
  ftestRegex  <- "(\\bF)"
  
  numbericRegex <- "\\d{1,99})"
  
  numbericRegex_decimals <- "((\\d{1,99}(\\.\\d{1,99})?)|(\\.\\d{1,99}))"
  
  degreesOfFreedomRegex_decimals <-
    paste0("(\\s{0,5}?\\(?\\s{0,5}?",
           numbericRegex_decimals,
           "\\s{0,5}?[,;\\s{1,5}]\\s{0,5}?",
           numbericRegex_decimals,
           "\\s{0,5}\\)?)")
  
  paste0("((?i)\\(?\\s{0,5}((df\\s{0,5}\\=?\\s{0,5})|(n\\s{0,5}\\=\\s{0,5}))?\\s{0,5}",
                                                  numbericRegex_decimals,
                                                  "\\s{0,5}\\)?(?-i))")
  ofOrEqualsRegex <- "((of)|=|:)"
  
  numbericBelow1Regex <- "((?<![1-9])\\.\\d{1,99}|0(\\.\\d{1,99})?|(1\\.0{0,99}(?!(0{0,99}[1-9])))|((?<![0-9\\.])1(?![\\.0-9])))"
  # additional p value detector
  # additional p value detector
  pValueRegex <- "((?i)((\\s{0,5},?\\s{0,5})(ns))|(p\\s{0,5}[<>=(ns)]\\s{0,5}[<>]?\\s{0,5}((ns)|(\\d?\\.?\\d{1,99}e?-?\\d{0,99})|(\\.\\d{1,99})))(?-i))"
  
  # Additional df = detector
  # Additional n = detector
  
  
  ftestExtractionRegex <- paste0(
    ftestRegex,
    # Allowing spaces
    "\\s{0,5}",
    degreesOfFreedomRegex_decimals,
    # Making degrees of freedom optional
    "?",
    # Allowing spaces
    "\\s{0,5}",
    ofOrEqualsRegex,
    # Allowing spaces or negatives
    "\\s{0,5}",
    "\\-?",
    "\\s{0,5}",
    numbericRegex_decimals,
    "\\s{0,5}",
    "\\;?",
    ",?",
    "\\s{0,5}",
    pValueRegex, 
    "?" # Making p values optional
  ) 
  
  ### All values between 0 and 1 with decimals
  ### This one is for ttest coefficents
  detected_ftests <- unlist(stringr::str_extract_all(
    input, ftestExtractionRegex
  ))
  
  # Extracting the value with P
  value_with_p <- stringr::str_remove(
    detected_ftests,
    paste0(
      ftestRegex,
      # Allowing spaces
      "\\s{0,5}",
      degreesOfFreedomRegex_decimals,
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
               numbericRegex_decimals, ")")
      ), "\\s*")
  
  ps <-
    stringr::str_trim(
      stringr::str_extract(
        detected_ftests,
        pValueRegex
      ))
  
  dfs <- 
      stringr::str_extract(
        detected_ftests,
        paste0(
          "(?<=",
          ftestRegex,
          # Allowing spaces
          ")",
          "\\s{0,5}",
          degreesOfFreedomRegex_decimals
        ))
  
  df1 <- stringr::str_extract(dfs, "\\d{1,99}")
  df2 <- stringr::str_extract(dfs, "(?<=\\d{1,99}\\s{0,5}[,;\\s{1,5}]\\s{0,5})\\d+")
  
  
  return(tibble::tibble(statistic = "F",
                        reported = stringr::str_trim(detected_ftests), 
                        df1 = df1,
                        df2 = df2,
                        p = unlist(ps),
                        value = unlist(value)
  ))
}
