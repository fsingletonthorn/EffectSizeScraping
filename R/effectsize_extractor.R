#' Extract cohen's ds from text
#' extractES <- function(inputText)
#' @param inputText input text 
#' @return A tibble contaitning all detected effect sizes in odds ratios, Hazard ratios, d and eta 
#' associated degrees of freedom and p values, with context of size contextSize.
#' 
#' 
#' @examples
#' extractES("a cohen's d of .05, p < .01, and a partial eta squared of .1")
#' 
#' 
#' 
extractES <- function(input) {
  
  # Setting up all possible parts of the regex
  dRegex <-
    "\\b[dg]"

  ORRegex <- "(\\b((OR)|((?i)odds\\s{1,5}?ratio(?-i))))"
  HRRegex <- "(\\b((HR)|((?i)hazard\\s{1,5}?ratio(?-i))))"
  etaRegex <- "(?i)\\b(partial\\s*)?(\u03B7|(eta))\\s*p?\\s*2?(squared)?(?-i)"
  
  numbericRegex_commas <- "(((\\d{1,3}(?=,)(,\\d{3})*)|\\d+)?)"
  
  numbericRegex_commas_decimals <- "((((\\d{1,3}(?=,)(,\\d{3}){0,99})|\\d{1,99})(\\.\\d{1,99})?)|(\\.\\d{1,99}))"
  
  ofOrEqualsRegex <- "((of)|=|:|;)"
  
  numbericBelow1Regex <- "((?<![1-9])\\.\\d+|0(\\.\\d+)?|(1\\.0*(?!(0*[1-9])))|((?<![0-9\\.])1(?![\\.0-9])))"
  # additional p value detector
  # additional p value detector
  pValueRegex <- "((?i)((\\s*,?\\s*)(ns))|(p\\s*[<>=(ns):]\\s*[<>]?\\s{0,5}((ns)|(\\d?\\.?\\d+e?-?\\d*)|(\\.\\d+)))(?-i))"
  
  DPRegex <- paste0(
    dRegex,
    # Allowing spaces
    "\\s{0,5}",
    ofOrEqualsRegex,
    # Making degrees of freedom optional
    # Allowing spaces or negatives
    "\\s{0,5}",
    "\\-?",
    "\\s{0,5}",
    numbericRegex_commas_decimals,
    "\\s*",
    "\\;?",
    ",?",
    "\\s*",
    pValueRegex, 
    "?" # Making p values optional
  ) 
  
  
  ORPRegex <- paste0(
    "(",
    ORRegex,
    "|",
    HRRegex,
    ")",
    # Allowing spaces
    "\\s{0,5}",
    ofOrEqualsRegex, 
    "\\s{0,5}",
    numbericRegex_commas_decimals,
    "\\s{0,5}",
    "\\;?",
    ",?",
    "\\s*",
    pValueRegex, 
    "?" # Making p values optional
  ) 
  
  etaPRegex <- paste0(
    etaRegex,
    # Allowing spaces
    "\\s{0,5}",
    ofOrEqualsRegex,
    # Making degrees of freedom optional
    # Allowing spaces or negatives
    "\\s{0,5}",
    numbericBelow1Regex,
    "\\s*",
    "\\;?",
    ",?",
    "\\s*",
    pValueRegex, 
    "?" # Making p values optional
  )
  
  detected_d <- unlist(stringr::str_extract_all(
    input, DPRegex
  ))
  
  detected_OR <- unlist(stringr::str_extract_all(
    input, ORPRegex
  ))
  
  detected_eta <- unlist(stringr::str_extract_all(
    input, etaPRegex
  ))
  
  
  value_d <- stringr::str_extract(
    detected_d, numbericRegex_commas_decimals)
  
  ps_d <-
    stringr::str_trim(
      stringr::str_extract(
        detected_d,
        pValueRegex
      ))  
  
  value_OR <- stringr::str_extract(
    detected_OR, numbericRegex_commas_decimals)
  
  ps_OR <-
    stringr::str_trim(
      stringr::str_extract(
        detected_OR,
        pValueRegex
      ))
  
  value_eta <- stringr::str_extract(
    detected_eta,
    paste0("(?<=",
      ofOrEqualsRegex,
           "\\s{0,5})",
    numbericRegex_commas_decimals))
  
  ps_eta <-
    stringr::str_trim(
      stringr::str_extract(
        detected_eta,
        pValueRegex
      ))
  
  
  output <- 
    dplyr::bind_rows(
    list(d = tibble::tibble(raw = detected_d,  statistic = "d",  value = value_d, p = ps_d),
         OR = tibble::tibble(raw = detected_OR, statistic = "OR", value = value_OR, p = ps_OR),
         eta = tibble::tibble(raw = detected_eta, statistic = "eta", value = value_eta, p = ps_eta)
                        ))

  return(tibble::tibble(statistic = output$statistic,
                        reported = stringr::str_trim(output$raw), 
                        df1 = NA,
                        df2 = NA,
                        p = output$p,
                        value = as.numeric(output$value)
  ))
}
