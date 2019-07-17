# words2Nums
### Setting up constants
##  This is inspired by https://github.com/finnfiddle/words-to-numbers

# NEED TO PUT IN CHECK TO MAKE SURE THAT NUMBERS LISTED NEXT TO EACH OTHER ARE NOT CAPTURED IN THE SAME GROUP !!!

words_to_numbers <- function(string) { 

UNIT <- list(
  zero = 0,
#  first = 1,
  one = 1,
#  second = 2,
  two = 2,
#  third = 3,
#  thirteenth = 13,
  thirteen = 13,
  three = 3,
#  fourth = 4,
#  fourteenth = 14,
  fourteen = 14,
  four = 4,
#  fifteenth = 15,
  fifteen = 15,
#  fifth = 5,
  five = 5,
#  sixth = 6,
#  sixteenth = 16,
  sixteen = 16,
  six = 6,
#  seventeenth = 17,
  seventeen = 17,
#  seventh = 7,
  seven = 7,
#  eighteenth = 18,
  eighteen = 18,
#  eighth = 8,
  eight = 8,
#  nineteenth = 19,
  nineteen = 19,
#  ninth = 9,
  nine = 9,
#  tenth = 10,
  ten = 10,
#  eleventh = 11,
  eleven = 11,
#  twelfth = 12,
  twelve = 12
)

TEN <- list(
  twenty = 20,
#  twentieth = 20,
  thirty = 30,
#  thirtieth = 30,
  forty = 40,
#  fortieth = 40,
  fifty = 50,
#  fiftieth = 50,
  sixty = 60,
#  sixtieth = 60,
  seventy = 70,
#  seventieth = 70,
  eighty = 80,
#  eightieth = 80,
  ninety = 90 #,
#  ninetieth = 90
)

MAGNITUDE = list(
  hundred = 100,
#  hundredth = 100,
  thousand = 1000,
  million = 1000000,
  billion = 1000000000,
  trillion = 1000000000000,
  quadrillion = 1000000000000000,
  quintillion = 1000000000000000000,
  sextillion = 1000000000000000000000,
  septillion = 1000000000000000000000000,
  octillion = 1000000000000000000000000000,
  nonillion = 1000000000000000000000000000000,
  decillion = 1000000000000000000000000000000000
)

NUMBER <- c(UNIT, TEN, MAGNITUDE)

UNIT_KEYS <- names(UNIT)
TEN_KEYS <- names(TEN)
MAGNITUDE_KEYS <- names(MAGNITUDE)

NUMBER_WORDS <- c(UNIT_KEYS, TEN_KEYS, MAGNITUDE_KEYS)

# canAddTokenToEndOfSubRegion <- function(tokens, subRegion, currentToken, impliedHundreds){
# split at whitespace
stringSplitVec <-
  stringr::str_split(string,
                     "(?<=[[[:punct:]]|\\s])|(?=[[[:punct:]]|\\s])",
                     simplify = T)

# create binaries for whitespace and for number words
punctuationBinary <-
  stringr::str_detect(stringSplitVec, "[[:punct:]]|\\s|(^and$)")

# Ensuring that "." breaks numbers apart
punctuationBinary[stringr::str_detect(stringSplitVec, "\\.")] <- FALSE

# Detecting numbers 
numberBinary <-
  stringr::str_detect(stringSplitVec, stringr::regex(
    paste("^", NUMBER_WORDS, "$", collapse = "|", sep = ""),
    ignore_case = T
  ))

  # Making tibble
  stringSplit <-
    tibble::tibble(
      id = 1:length(numberBinary),
      stringSplit = as.character(stringSplitVec),
      punctuationBinary,
      numberBinary
    )
  
  # Creating a grouping variable
# punctuationOrNumberNotFirstOrLast <- stringSplit$punctuationBinary | stringSplit$numberBinary
  stringSplit$group <- NA
  stringSplit$group[1] <- ifelse(stringSplit$numberBinary[1], 1, 0) # Rewrtie to figure out if it is a numberic 
  
  # Creating groups of the numbered and unnumbered sections
  if (nrow(stringSplit) > 1) {
    for (i in 2:nrow(stringSplit)) {
      stringSplit$group[i] <-
        ifelse(
          punctuationBinary[i - 1] == punctuationBinary[i],
          stringSplit$group[i - 1],
          stringSplit$group[i - 1] + 1
        )
    }
  }
  
  # NEED TO PUT IN CHECK TO MAKE SURE THAT NUMBERS LISTED NEXT TO EACH OTHER ARE NOT CAPTURED IN THE SAME GROUP !!!
  
  #creating groupin variable
  stringSplit$group <- NA
  
  # Using cumulative sum to count the number of non-number items, not counting punctuation 
  stringSplit$group[!stringSplit$punctuationBinary] <- cumsum(!stringSplit$numberBinary[!stringSplit$punctuationBinary])
  
  # Removing dots and other words from contention to ensure that groups get broken at points 
  stringSplit$group[stringr::str_detect(stringSplit$stringSplit, "\\.") ] <- -1
  stringSplit$group  <- ifelse((stringSplit$numberBinary | stringSplit$punctuationBinary), stringSplit$group, -1)
  
  # Filling in all NAs between two other values - this means that each number is grouped together
  stringSplit$group <-
    ifelse(
      tidyr::fill(stringSplit, group,  .direction = "down")$group == tidyr::fill(stringSplit, group,  .direction =  "up")$group,
      tidyr::fill(stringSplit, group,  .direction = "down")$group,
      NA
    )
  
  stringSplit$number <- NA
  
  #### Extracting all of the numbers - we may as well 
  # tokensArray <- tapply(stringSplit$stringSplit, INDEX = stringSplit$group, FUN = paste0, simplify = T, collapse = " ")[-1]
  # tokens <- tibble::tibble(tokens = tokensArray, region = names(tokensArray), numericToken = NA)
  
  # Pairing down to just those effects that have numerics
  numericStrings <- dplyr::filter(stringSplit, group > -1)

  # Identifying the types of each numberic
  numericStrings$magnitudeType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste(MAGNITUDE_KEYS, collapse = "|"),
                                       ignore_case = T))
  
  numericStrings$tenType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste(TEN_KEYS, collapse = "|"),
                                       ignore_case = T))
  
  numericStrings$unitType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste(UNIT_KEYS, collapse = "|"),
                                       ignore_case = T))
  
  # Helper function for assessing each group of numbers 
  # processedNumerics <- dplyr::filter(numericStrings, group == 1)
  
    identifyNumbers <- function(processedNumerics) {
    # Extracting numbers only
    numericsOnly <- dplyr::filter(processedNumerics, numberBinary)
    # Creating numbers columns
    numericsOnly$number <-
      as.numeric(NUMBER[match(tolower(numericsOnly$stringSplit), NUMBER_WORDS)])
    # For all magnitiude types, count all smaller magnitude types as multipliers of the magnitude value
    if (sum(numericsOnly$magnitudeType) > 0) {
      for (position in which(numericsOnly$magnitudeType)) {
        startCountingFrom <-
          ifelse(any(numericsOnly$number[1:(position - 1)][numericsOnly$magnitudeType[1:(position -
                                                                                           1)]] > numericsOnly$number[position]),
                 max(which(
                   numericsOnly$number[1:(position - 1)] > numericsOnly$number[position]
                 ) + 1),
                 1)
        previousSum <-
          sum(numericsOnly$number[startCountingFrom:(position - 1)]) # begining or previous highest magnitude
        value <- previousSum * numericsOnly$number[position]
        numericsOnly$number[startCountingFrom:(position - 1)] <- 0
        numericsOnly$number[position] <- value
      }
    }
    return(sum(numericsOnly$number))
}
  
  numericedOutput <- stringSplit
    
  for(groups in unique(numericStrings$group)) {
    groupFilter <- which(numericStrings$group == groups)

    numericedOutput$number[which(numericedOutput$group== groups)[1]] <- 
      identifyNumbers(numericStrings[groupFilter,])
    numericedOutput$stringSplit[which(numericedOutput$group== groups)] <- ""
    numericedOutput$stringSplit[which(numericedOutput$group== groups)[1]] <-
      numericedOutput$number[which(numericedOutput$group== groups)[1]]
  }
  
  stringSplit$numer <- numericedOutput$number
return(paste0(numericedOutput$stringSplit, collapse = ""))
}

  