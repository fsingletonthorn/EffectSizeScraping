#' Extract  chi square tests from text
#' 
#' Extract  chi square tests from text
#'  
#' @param inputText input text 
#' @return A tibble containing all extracted values, with columns identifying the type of statistic extracted, the reported test as reported in the text, the degrees of freedom, the sample size included in the test, the p value, and the chi square value reported. 
#' 
#' 
#' @examples
#' extractChiSquare("chi2(123, n = 12) = .01, p > .05")
#' 
#' 

extractChiSquare <- function(input) {
  # Setting up all possible parts of the regex
  chiRegex  <- "(\\b((?i)(chi|\\u03C7|\\u1D712|\\u1D61|\\u1D6a|\\u2439|\\u1D712|\\u1D74C|\\u1D86|\\u1D7C0)\\s{0,5}(2|square|squared)(?-i))|(X2))"

  numbericRegex <- "\\d{1,99})"
  
  numbericRegex_decimals <- "((\\d{1,99}(\\.\\d{1,99})?)|(\\.\\d{1,99}))"
  
  numbericRegex_commas_decimals <- "((((\\d{1,3}(?=,)(,\\d{3}){0,99})|\\d{1,99})(\\.\\d{1,99})?)|(\\.\\d{1,99}))"
  
  
  degreesOfFreedomRegex_decimals <-
    paste0("((?i)" ,
           "\\s{0,5}\\((((df)|n\\s{0,5}\\=))?\\s{0,5}",
           numbericRegex_commas_decimals,
           "\\s{0,5}\\)?(\\s{0,5}[,;\\s]\\s{0,5}(df|n)\\s{0,5}(\\=|\\;|\\:)\\s{0,5}",
           numbericRegex_commas_decimals,
           "?\\s{0,5}\\)?)?(?-i))")
  
  chiMisreadRegex <- paste0("\\b((?i)(chi|\\u03C7|\\u1D712|\\u1D61|\\u1D6a|\\u2439|\\u1D712|\\u1D74C|\\u1D86|\\u1D7C0)\\s{0,5}",
  degreesOfFreedomRegex_decimals,
  "\\s{0,5}(2|square|squared)(?-i))")
  
  ofOrEqualsRegex <- "((of)|=|:|;)"
  
  pValueRegex <- "((?i)((\\s{0,5},?\\s{0,5})(ns))|(p\\s{0,5}[<>=(ns):]\\s{0,5}[<>]?\\s{0,5}((ns)|(\\d?\\.?\\d{1,99}e?-?\\d{0,99})|(\\.\\d{1,99})))(?-i))"
  
  chisquareExtractionRegex <- paste0(
    "((",
    chiRegex,
    # Allowing spaces
    "\\s{0,5}",
    degreesOfFreedomRegex_decimals,
    # Making degrees of freedom optional,
    "?",
    "\\s{0,5}",
    # Allowing spaces 
    # "\\s{0,5}",
    ")|",
    chiMisreadRegex,   
    ")\\s{0,5}",
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
  
  # Extrracting all detected
  detected_chisquare <- unlist(stringr::str_extract_all(
    input, chisquareExtractionRegex
  ))
  
  # Getting the look behind to extract test statistic values
  
  value_with_p <- stringr::str_remove(
    detected_chisquare,
    paste0(
      "((",
      chiRegex,
      # Allowing spaces
      "\\s{0,5}",
      degreesOfFreedomRegex_decimals,
      # Making degrees of freedom optional,
      "?",
      "\\s{0,5}",
      # Allowing spaces 
      # "\\s{0,5}",
      ")|",
      chiMisreadRegex,   
      ")\\s{0,5}",
      ofOrEqualsRegex,
      # Allowing spaces
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
        # "(?<=",
        # "((?i)((chi)|\\u03C7|\\u1D712|\\u1D61|\\u1D6a|\\u2439|\\u1D712|\\u1D74C|\\u1D86|\\u1D7C0)(?-i))",
        # "\\s{0,5}",
        # ")",
        degreesOfFreedomRegex_decimals
      ))
  
  df2 <- stringr::str_extract(dfs,paste0("(?<!(\\d|(numbericRegex_commas_decimals)|(n|N)\\s{0,5}(=|;|:)?\\s{0,5}))", numbericRegex_commas_decimals))
  n <- stringr::str_extract(dfs, paste0("(?<=((n|N)\\s{0,5}(=|;|:)?\\s{0,5}))", numbericRegex_commas_decimals))
  
  return(tibble::tibble(statistic = "chi",
                        reported = stringr::str_trim(detected_chisquare), 
                        df1 = NA,
                        df2 = as.numeric(df2),
                        p = as.character(unlist(ps)),
                        n = as.numeric(n),
                        value = as.numeric(unlist(value))
  ))
}

