# Functions that search text for CIs, Power analysis, that attempt to extract sample sizes

# Check for 95% CIs
checkCIs <- function(input,
                     context = T,
                     contextSize = 0) {
  
  CI_REGEX  <- "(\\bconfidence\\s*intervals?\\b)|(\\bC\\.?I\\.?s?\\b(?!\\.|!).*(\\(|\\[)\\d+,?\\s*\\-?\\s*\\d+(\\]|\\)))|(\\d{2}\\s*\\%\\s*C\\.?I\\.?\\b)"
  
  CIregContext <-
      paste0(".{0,", contextSize, "}(",
      CI_REGEX, ").{0,", contextSize, "}")
  
  # Extract CIs with context
  CIs_context <- stringr::str_extract_all(input,
                                  stringr::regex(CIregContext,
                                                 ignore_case = T), 
                                  simplify = T)
  
  # If CIs is empty, CIs are not captured
  CIsBinary <- (dim(CIs_context)[2] > 0) & any(!is.na(CIs_context))
  
  if(CIsBinary) {
  # Extract just CIs
  CIs <- stringr::str_extract_all(CIs_context,
                                  stringr::regex(CI_REGEX,
                                                 ignore_case = T), 
                                  simplify = T)
  } else {CIs <- character(0)}
  
  if (context == T & CIsBinary == T) {
    return(tibble::tibble(CIs = CIs, context = CIs_context, CIBinary = CIsBinary))
  } else if (context == T & CIsBinary == F) {
    tibble::tibble(CIs = NA, context = NA, CIBinary = CIsBinary)
  } else {
    return(tibble::tibble( CIBinary = CIsBinary) )
  }
}
 
findN <- function(input) {
  
  # Converting words to numbers for sample size extraction
  inputProcessed <-  words_to_numbers( input )
  
  inputProcessed <- lapply(inputProcessed, stringr::str_replace_all, pattern = "(?<=\\d)\\,(?=\\d)", replacement = "")
  # At the moment this does not pull out context 
  
  # Setting up regexs
  SSOf <- "(\\ba?\\s*sample\\s*size\\s*of\\s*\\d+(?!(\\d{0,}\\s{0,}(%|(percent)|(measure)|(question)|(item)))))"
  nOf <- "((\\bN\\s*(\\=|(of))\\s*)\\d+(?!(\\d{0,}\\s{0,}(%|(percent)|(measure)|(question)|(item)))))"
  nPar <- "(\\b\\d+((?!([:punct:]|(\\d{0,}\\s{0,}(%|(percent)|(measure)|(question)|(item))))).)*((took\\s+part)|participants|participated|volunteer(s|ed)|observations|survey))"
  parWere <- "(\\b(subjects|participants)(?!\\[:punct:]|[\\(\\)\\.\\,])\\s*were\\s*(?!\\[:punct:])\\d+(?!(\\d{0,}\\s{0,}(%|(percent)|(measure)|(question)|(item)))))"
  aTotalofN <- "a\\s+total\\s+of\\s+\\d+(?!(\\d{0,}\\s{0,}(%|(percent)|(measure)|(question)|(item))))((?![\\.\\n]).)*(participants|undergraduate|college|students|rats|mice|subjects|men|woman|people|mturk|samples|took\\s+part|involved|survey|responses|respondants)((?![\\.\\n]).)*"
  
  # pullting together regex 
  NRegex <-  paste(SSOf, nOf, nPar, parWere, aTotalofN , sep = "|")

  # Sample size strings
  sampleSizeStrings <- stringr::str_extract_all(inputProcessed,
                                                stringr::regex(NRegex, ignore_case = T), 
                                                simplify = T)
  
  if(elementExists(sampleSizeStrings)) {
  sampleSizes <- stringr::str_extract(sampleSizeStrings, "\\d{1,}")
  } else {sampleSizes <- character(0)}
  
  return(tibble::tibble(string = t(sampleSizeStrings), N = sampleSizes))
}

# Function to process text output in the categorisation functions of this package
extractNsFromProcessed <- function(text){
  # Text must be a tibble / df with column 1 = "names", column 2 = "text"
dplyr::bind_rows(
  purrr::map2_dfr(text$names, text$text, function(x, y) {
    if(is.na(y)) {return(suppressWarnings(tibble::tibble(name = NA, nString = NA, n = NA)))}
    name <- x
    Ns <- findN(y)
    return(suppressWarnings(tibble::tibble(name = rep(name, nrow(Ns)), nString = Ns[[1]], n = Ns[[2]])))
  }
  )
)
  
}# Function to process text output in the categorisation functions of this package
extractCIFromProcessed <- function(text){
  # Text must be a tibble / df with column 1 = "names", column 2 = "text"
dplyr::bind_rows(
  purrr::map2_dfr(text$names, text$text, function(x, y) {
    # if(is.na(y)) {return(suppressWarnings(tibble::tibble(name = NA, nString = NA, n = NA)))}
    name <- x
    CIs <- checkCIs(y)
    return(suppressWarnings(tibble::tibble(name = rep(name, nrow(CIs)), CIString = CIs$CIs, CIBinary = CIs$CIBinary)))
  }
  )
)
}

# findPowerAnalysis <- {
#   stringr::str_extract_all()
#   }

# findReliabilityCheck <- {
#   stringr::str_extract_all()
# "internal consistency" "reliability of the measures was \\d [not followed by a full stop followed by whitespace (in order to avoid .9 being rejected). ]" etc. 
#   }
