#' Extract effect sizes from text
#' extractES <- function(inputText)
#' @param inputText input text 
#' @return A tibble contaitning all detected effect sizes in odds ratios, Hazard ratios, d, 
#' associated degrees of freedom and p values, with context of size contextSize.
#' 
#' 
#' @examples
#' extractCorrelations("r(123) = .01, p = < .001")
#' 
#' 
extractCorrelations <- function(input) {
  
  # Setting up all possible parts of the regex
  correlationRegex_symbolic  <- "(\\b(r|\u03c1|rho|((?i)(correlation(\\s{0,5}coefficient)?)(?-i)))\\b)"
  numbericRegex_commas <- "(((\\d{1,3}(?=,)(,\\d{3})*)|\\d+)?)"
  
  numbericRegex_commas_decimals <- "((((\\d{1,3}(?=,)(,\\d{3}){0,99})|\\d{1,99})(\\.\\d{1,99})?)|(\\.\\d{1,99}))"
  
  degreesOfFreedomRegex_commas <- paste0("((?i)\\(?\\s*((df\\s*\\=?\\s*)|(n\\s*\\=\\s*))?\\s*",
                                         numbericRegex_commas,
                                         "\\s*\\)?(?-i))")
  ofOrEqualsRegex <- "((of)|=|:)"
  
  numbericBelow1Regex <- "((?<![1-9])\\.\\d+|0(\\.\\d+)?|(1\\.0*(?!(0*[1-9])))|((?<![0-9\\.])1(?![\\.0-9])))"
  # additional p value detector
  # additional p value detector
  pValueRegex <- "((?i)((\\s*,?\\s*)(ns))|(p\\s*[<>=(ns)]\\s*[<>]?\\s{0,5}((ns)|(\\d?\\.?\\d+e?-?\\d*)|(\\.\\d+)))(?-i))"
  
  # Additional df = detector
  # Additional n = detector
  
  
  correlationExtractorRegex <- paste0(
    correlationRegex_symbolic,
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
    numbericBelow1Regex,
    "\\s*",
    "\\;?",
    ",?",
    "\\s*",
    pValueRegex, 
    "?" # Making p values optional
  ) 
  
  ### All values between 0 and 1 with decimals
  ### This one is for correlation coefficents
  detected_correlations <- unlist(stringr::str_extract_all(
    input, correlationExtractorRegex
  ))
  
  # Getting the look behind to extract test statistic values
  # TODO Just remove everything before the numbericBelow1Regex and use that
  
  value_with_p <- stringr::str_remove(
    detected_correlations,
    paste0(
      correlationRegex_symbolic,
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
        detected_correlations,
        pValueRegex
      ))
  
  cors_with_ns <- 
    stringr::str_detect(
      detected_correlations, 
      "((?!=[nt])n\\s*\\=\\s*)"
    )
  
  df2 <- 
    stringr::str_remove_all(
      stringr::str_extract(
        detected_correlations,
        paste0(
          "(?<=",
          correlationRegex_symbolic,
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
                        raw = stringr::str_trim(detected_correlations), 
                        df1 = NA,
                        df2 = df2,
                        p = unlist(ps),
                        value = unlist(value)
  ))
}




patternD <-
  "\\b[dg]"
"\\s{0,5}"
""


-


\\s*[\\.\\,\\:\\;]?\\s*([\\=\\:]|(of))\\s*(\\-?\\s*\\d*(\\.|(,(?=\\d)))?\\d{1,})"
patternEta <-
  "\\b(partial\\s*)?(\u03B7|(eta))\\s*p?\\s*2?(squared)?\\s*([\\=\\:)]|(of))\\s*-?\\s*\\d*(\\.|(,(?=\\d)))?\\d{1,}"
patternHR <-
  "\\b((HR)|(hazzard.{1,3}?ratio))\\s*((of)|(=|:))\\s*(\\-?\\s*\\d*(\\.|(,(?=\\d)))?\\d{1,})"
patternOR <-
  "\\b((OR)|(odd.{1,3}?ratio))\\s*((of)|(=|:))\\s*(\\-?\\s*\\d*(\\.|(,(?=\\d)))?\\d{1,})"
