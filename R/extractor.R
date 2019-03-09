# helper function to split the output into constituate parts
splitTestStatToDF <- function(statistic, cleanedTestStat) {
  if ((length(cleanedTestStat) > 0) & (length(statistic) > 0)) {
    testStatistic <-
      stringr::str_extract(stringr::str_split(cleanedTestStat,
                                              "=", simplify = T)[, 2],
                           "-?\\d*\\.?\\d*")
    df1 <-
      case_when(statistic == "F" ~ c(stringr::str_extract(cleanedTestStat,
                                                          "\\d{1,}(?=,)")),
                TRUE ~ NA_character_)
    # Add other statistics here below in addition to F
    df2 <-
      dplyr::case_when(
        statistic == "F" ~ stringr::str_extract(cleanedTestStat,
                                                "(?<=,)\\d{1,}"),
        # If people have reported r(n=x) return as n df2 = n - 2
        stringr::str_detect(statistic, "r") &
          stringr::str_detect(cleanedTestStat, "n=") ~
          as.character(as.numeric(
            stringr::str_extract(cleanedTestStat,
                                 "(?<=([a-zA-Z])\\(?(n\\=)?)\\d{1,}")
          ) - 2),
        stringr::str_detect(statistic, "T|t|r|R") ~
          stringr::str_extract(
            cleanedTestStat,
            "(?<=([a-zA-Z])\\(?(df\\s{0,10}\\=\\s{0,10})?)\\d{1,}"
          ),
        stringr::str_detect(statistic, "chi") ~
          stringr::str_extract(
            cleanedTestStat,
            "(?<=\\((df\\s{0,10}=\\s{0,10})?)\\d{1,}"
          ),
        TRUE ~ NA_character_
      )
    p <-
      stringr::str_extract(
        cleanedTestStat,
        "(?<=((p|P)\\=?))[<>]?\\d?\\.?\\d+e?-?\\d*")
    tibble::data_frame(
      value = testStatistic,
      df1 = df1,
      df2 = df2,
      p = p
    )
  } else
    NA_real_
}

# Check whether element exists and is not NA
# (or at least that th first element of an object is not NA)
elementExists <- function( full_index_path ){
  tryCatch({
    len_element = length(full_index_path)
    if(is.na(full_index_path)[[1]]) {return(F)}
    exists_indicator = ifelse(len_element > 0, T, F)
    return(exists_indicator)
  }, error = function(e) {
    return(F)
  })
}

# Function to add + and minus "contextSize" characters for a regex pattern
addContext <- function(extracted, contextSize) {
  if (length(extracted) > 0) {
    newSearchR  <- extracted %>%
      stringr::str_replace_all("\\.", "\\\\.") %>%
      stringr::str_replace_all("\\(", "\\\\(") %>%
      stringr::str_replace_all("\\)", "\\\\)") %>%
      paste0(".{0,", contextSize, "}",  . , ".{0,", contextSize, "}")
  }
  else {
    NA
  }
}

# For this to work it needs to be fed a single string
# function to extract text, remove whitespaces and unicode encodings of the
# minus sign and return test statistic original data plus df
extractTestStats <- function(inputText, context = FALSE, contextSize = 100) {

  # patterns -
  patternT <-
    "\\bt\\s*\\(\\s*\\d{1,}\\.?\\d*\\s*\\)\\s*=\\s*-?\\s*\\d*\\.?\\d*(\\s*,?\\s*p\\s*[<>=]\\s*0?\\.\\d+e?-?\\d*)?"
  patternF <-
    "\\bF\\s*\\(?\\s*\\d{1,},\\s*\\d{1,}\\s*\\)?\\s*=\\s*\\d*\\.?\\d*(\\s*,?\\s*p\\s*[<>=]\\s*0?\\.\\d+e?-?\\d*)?"
  patternR <-
    "\\b(((r(pb)?(?!2)\\s*\\(?\\s*(df|n)?\\s*[=]?\\s*\\d{0,10}\\s*\\)?\\s*[=]\\s*)|((correlation)\\s*(coefficient)?\\s*([=]|(of))\\s*))(\u2212?\\-?\\s?\\d*\\.?\\d{1,}))(\\s*,?\\s*p\\s*[<>=]\\s*0?\\.\\d+e?-?\\d*)?"
  patternChiSq <-
    "\\b(chi|\\u03C7|\\u1D712|\\u1D61|\\u1D6a|\\u1D712|\\u1D74C|\\u1D86|\\u1D7C0)\\s*(2|square|squared)?\\s*\\(?\\s*(df|n)?\\s*[=]?\\s*\\d*\\s*\\,?\\s*(df|n)?\\s*\\=?\\s*\\d*\\s*\\)?\\s*([=]|(of))\\s*-?\\s*\\d*\\.?\\d*(\\s*,?\\s*p\\s*[<>=]\\s*0?\\.\\d+e?-?\\d*)?"
  patternD <-
    "\\b[dg]\\s*[\\.\\,\\:\\;]?\\s*([=]|(of))\\s*(\\-?\\s*\\d*\\.?\\d{1,})"
  patternEta <-
    "\\b(partial\\s*)?(\u03B7|(eta))\\s*p?\\s*2?(squared)?\\s*([=]|(of))\\s*-?\\s*\\d*\\.?\\d{1,}"
  patternHR <-
    "\\b((HR)|(hazzard.{1,3}?ratio))\\s*((of)|(=))\\s*(\\-?\\s*\\d*\\.?\\d{1,})"
  patternOR <-
    "\\b((OR)|(odd.{1,3}?ratio))\\s*((of)|(=))\\s*(\\-?\\s*\\d*\\.?\\d{1,})"


  # Do not change order of patterns, that will break everything
  patterns <-
    c(
      patternT,
      patternF,
      patternR,
      patternChiSq,
      patternD,
      patternEta,
      patternHR ,
      patternOR
    )

  # This is a test of one that does not capture MSEs, actually does not work because of the way that the context capture works
  # stringr::str_extract("F(1,3) = 1232.12, MSE = 1232, p = 1.1232", "\\bF\\s*\\(?\\s*\\d{1,},\\s*\\d{1,}\\s*\\)?(?:\\,\\s{0,5}MSE\\s{0,5}\\,\\s{0,5}\\=\\d{1,}\\.?\\d{1,}\\,)?\\s*=\\s*\\d*\\.?\\d*(\\s*,?\\s*p\\s*[<>=]\\s*\\d?\\.\\d+e?-?\\d*)?")


  if (purrr::is_empty(inputText) | !elementExists(inputText)) {
    if (context == F) {
      return(
        tibble::data_frame(
          statistic = NA,
          cleaned = NA,
          reported = NA,
          value = NA,
          df1 = NA,
          df2  = NA,
          p = NA
        )
      )
    } else {
      return(
        tibble::data_frame(
          statistic = NA,
          cleaned = NA,
          reported = NA,
          context = NA,
          value = NA,
          df1 = NA,
          df2  = NA,
          p = NA
        )
      )
    }
  }

  # Input should already be cleaned, this is important as it will break this function -  processHTML() on input
  # extracting all text which matches patterns

  extracted <-
    stringr::str_extract_all(inputText , pattern = regex(patterns, ignore_case = TRUE))

  # removing whitespace that can be captured in the regular expressions above
  extractedClean <-
    lapply(extracted,  stringr::str_remove_all, pattern = "\\s")

  # The following extracts the context around each extracted part:
  # Putting the following into a single string that will expand to the number of patterns

  if(context == T) {
    patternsContext <- lapply(extracted, addContext, contextSize = contextSize)
    # extracting discovered hits + context
    extractedContext <- lapply(patternsContext, function(patternsContext)
      unlist(stringr::str_extract(inputText, regex(as.character(patternsContext), ignore_case = T))))
    # returning all of this
    statisticalOutput <- rbind(tibble::data_frame(statistic = "t",   cleaned = extractedClean[[1]], reported = extracted[[1]], context = extractedContext[[1]]),
                 tibble::data_frame(statistic = "F",   cleaned = extractedClean[[2]], reported = extracted[[2]],  context = extractedContext[[2]]),
                 tibble::data_frame(statistic = "r",   cleaned = extractedClean[[3]], reported = extracted[[3]],  context = extractedContext[[3]]),
                 tibble::data_frame(statistic = "chi", cleaned = extractedClean[[4]], reported = extracted[[4]],  context = extractedContext[[4]]),
                 tibble::data_frame(statistic = "d",   cleaned = extractedClean[[5]], reported = extracted[[5]],  context = extractedContext[[5]]),
                 tibble::data_frame(statistic = "eta", cleaned = extractedClean[[6]], reported = extracted[[6]],  context = extractedContext[[6]]),
                 tibble::data_frame(statistic = "HR",  cleaned = extractedClean[[7]], reported = extracted[[7]],  context = extractedContext[[7]]),
                 tibble::data_frame(statistic = "OR",  cleaned = extractedClean[[8]], reported = extracted[[8]],  context = extractedContext[[8]]))

    } else {
    statisticalOutput <- rbind(tibble::data_frame(statistic = "t", cleaned = extractedClean[[1]], reported = extracted[[1]]),
                 tibble::data_frame(statistic = "F", cleaned = extractedClean[[2]], reported = extracted[[2]]),
                 tibble::data_frame(statistic = "r", cleaned = extractedClean[[3]], reported = extracted[[3]]),
                 tibble::data_frame(statistic = "chi", cleaned = extractedClean[[4]], reported = extracted[[4]]),
                 tibble::data_frame(statistic = "d",   cleaned = extractedClean[[5]], reported = extracted[[5]]),
                 tibble::data_frame(statistic = "eta", cleaned = extractedClean[[6]], reported = extracted[[6]]),
                 tibble::data_frame(statistic = "HR",  cleaned = extractedClean[[7]], reported = extracted[[7]]),
                 tibble::data_frame(statistic = "OR",  cleaned = extractedClean[[8]], reported = extracted[[8]]))
    }
 if(purrr::is_empty(statisticalOutput[[1]]) |  (nrow(statisticalOutput) < 1)) {
   if(context == F) {
     tibble::data_frame(
       statistic = NA, cleaned = NA, reported = NA, value = NA,
       df1 = NA, df2  = NA, p = NA)
     } else {
     tibble::data_frame(
       statistic = NA, cleaned = NA, reported = NA, context = NA,
       value = NA,  df1 = NA, df2  = NA, p = NA)
     }
   } else {
   cbind(statisticalOutput,
         splitTestStatToDF(statistic = statisticalOutput$statistic,
                           cleanedTestStat = statisticalOutput$cleaned),
         stringsAsFactors = F)
  }
}
