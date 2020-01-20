#' Extract  chi square tests from text
#' extractTestStats <- function(inputText, context = FALSE, contextSize = 100, sectionName = NA)
#' @param input input text 
#' @return A tibble contaitning all detected  chi square tests, 
#' associated degrees of freedom and p values
#' 
#' @examples
#' extractChiSquare("chi2(123, n = 12) = .01, p = > .05")
#' 
#' 

extractChiSquare <- function(input) {
  # Setting up all possible parts of the regex
  chiRegex  <- "(\\b(?i)(chi|\\u03C7|\\u1D712|\\u1D61|\\u1D6a|\\u2439|\\u1D712|\\u1D74C|\\u1D86|\\u1D7C0)\\s{0,5}(2|square|squared)?(?-i))"
  
  numbericRegex <- "\\d{1,99})"
  
  numbericRegex_decimals <- "((\\d{1,99}(\\.\\d{1,99})?)|(\\.\\d{1,99}))"
  
  numbericRegex_commas_decimals <- "((((\\d{1,3}(?=,)(,\\d{3}){0,99})|\\d{1,99})(\\.\\d{1,99})?)|(\\.\\d{1,99}))"
  
  degreesOfFreedomRegex_decimals <-
    paste0("((?i)\\s{0,5}?\\(?\\s{0,5}((df\\s{0,5}\\=?\\s{0,5})|(n\\s{0,5}\\=\\s{0,5}))?\\s{0,5}",
           numbericRegex_commas_decimals,
           "\\s{0,5}?[,;\\s{1,5}]?\\s{0,5}(df|n)?\\s{0,5}?(=|;|:)?\\s{0,5}?",
           numbericRegex_commas_decimals,
           "?\\s{0,5}\\)?(?-i))")
  
  ofOrEqualsRegex <- "((of)|=|:)"
  
  numbericBelow1Regex <- 
    
    "((?<![1-9])\\.\\d{1,99}|0(\\.\\d{1,99})?|(1\\.0{0,99}(?!(0{0,99}[1-9])))|((?<![0-9\\.])1(?![\\.0-9])))"
  # additional p value detector
  # additional p value detector
  pValueRegex <- "((?i)((\\s{0,5},?\\s{0,5})(ns))|(p\\s{0,5}[<>=(ns)]\\s{0,5}[<>]?\\s{0,5}((ns)|(\\d?\\.?\\d{1,99}e?-?\\d{0,99})|(\\.\\d{1,99})))(?-i))"
  
  # 
  
  # Additional df = detector
  # Additional n = detector
  
  
  chisquareExtractionRegex <- paste0(
    chiRegex,
    # Allowing spaces
    "\\s{0,5}",
    degreesOfFreedomRegex_decimals,
    # Making degrees of freedom optional,
    "?",
    "\\s{0,5}(?<!(\\n|N|(df))\\)?)", #### ! 
    # Allowing spaces, disallowing n = 
    "\\s{0,5}",
    ofOrEqualsRegex,
    # Allowing spaces
    "\\s{0,5}", 
    numbericRegex_commas_decimals,
    "\\s{0,5}",
    "\\;?",
    ",?",
    "\\s{0,5}",
    pValueRegex, 
    "?" # Making p values optional
  ) 
  
  ### All values between 0 and 1 with decimals
  ### This one is for ttest coefficents
  detected_chisquare <- unlist(stringr::str_extract_all(
    input, chisquareExtractionRegex
  ))
  
  # Getting the look behind to extract test statistic values
  # TODO Just remove everything before the numbericBelow1Regex and use that
  
  value_with_p <- stringr::str_remove(
    detected_chisquare,
    paste0(
      chiRegex,
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
               numbericRegex_decimals, ")")
      ), "\\s*")
  
  ps <-
    stringr::str_trim(
      stringr::str_extract(
        detected_chisquare,
        pValueRegex
      ))
  
  dfs <- 
    stringr::str_extract(
      detected_chisquare,
      paste0(
        "(?<=",
        "((?i)(chi|\\u03C7|\\u1D712|\\u1D61|\\u1D6a|\\u2439|\\u1D712|\\u1D74C|\\u1D86|\\u1D7C0)\\s{0,5}(?-i))",
        # Allowing spaces
        ")",
        "\\s{0,5}",
        degreesOfFreedomRegex_decimals
      ))
  
  df1 <- stringr::str_extract(dfs, paste0("(?<!(n\\s{0,5}?(=|;|:)?\\s{0,5}))", numbericRegex_commas_decimals))
  n <- stringr::str_extract(dfs, paste0("(?<=(n|N\\s{0,5}?(=|;|:)?\\s{0,5}))", numbericRegex_commas_decimals))
  
  return(tibble::tibble(statistic = "r",
                        raw = stringr::str_trim(detected_chisquare), 
                        df1 = NA,
                        df2 = df2,
                        p = unlist(ps),
                        n = n,
                        value = unlist(value)
  ))
}
