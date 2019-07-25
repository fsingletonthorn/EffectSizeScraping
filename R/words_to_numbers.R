# words2Nums
### Setting up constants
##  This is inspired by https://github.com/finnfiddle/words-to-numbers

# Note that numbers are output in scientific notation if they are high enough
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
  ten = 10,
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

# Quick exit if there are no numbers
if (sum(numberBinary) < 1) {
  return(string)
}
if (length(stringSplitVec) == 1) {
  return(as.character(format(NUMBER[[tolower(string)]], scientific = F)))
}

  # Making tibble
  stringSplit <-
    tibble::tibble(
      id = 1:length(numberBinary),
      stringSplit = as.character(stringSplitVec),
      punctuationBinary,
      numberBinary
    )
  
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
  
  # Checking for number point number and converting it into "number.number"
  ## NEED TO FIX IT SO "ten point five" != "10 .5" but rather "10.5"
  # stringSplit$stringSplit <-
  #   ifelse(c(
  #     # If the number following is a number 
  #     c(T, stringSplit$numberBinary) &
  #       # And the string is whitespace
  #       c(stringSplit$stringSplit == " ", F) &
  #       # And the word following is 
  #       c(
  #         stringr::str_detect(
  #           stringSplit$stringSplit[-1],
  #           stringr::regex("point|dot", ignore_case =
  #                            T)
  #         ),
  #         F,
  #         F
  #       ) &
  #       c(stringSplit$numberBinary[-c(1)], F, F)
  #   )[-(nrow(stringSplit) + 1)],
  #   "", stringSplit$stringSplit)
  # 
  # stringSplit$stringSplit <-
  #   ifelse(
  #     c(stringSplit$numberBinary[-1], F) &
  #       c(stringSplit$stringSplit == " ") &
  #       c(F, stringSplit$numberBinary[-c(1, 2)], F) &
  #       c(F,
  #         stringr::str_detect(
  #           stringSplit$stringSplit[-nrow(stringSplit)],
  #           stringr::regex("point|dot", ignore_case =
  #                            T)
  #         )
  #       )[1:nrow(stringSplit)],
  #     "",
  #     stringSplit$stringSplit
  #   )
  # 
  # stringSplit$stringSplit <-
  #   ifelse((
  #     c(T, T, stringSplit$numberBinary) &
  #       c(stringSplit$numberBinary[-c(1, 2)], F, F, F, F) &
  #       c(
  #         stringr::str_detect(
  #           stringSplit$stringSplit,
  #           stringr::regex("point|dot", ignore_case =
  #                            T)
  #         ),
  #         F,
  #         F
  # #       )
  #   )[1:nrow(stringSplit)],
  #   ".", stringSplit$stringSplit)
  # 
  
  # initiallising number vec
  stringSplit$number <- NA
  
  #### Extracting all of the numbers - we may as well 
  # tokensArray <- tapply(stringSplit$stringSplit, INDEX = stringSplit$group, FUN = paste0, simplify = T, collapse = " ")[-1]
  # tokens <- tibble::tibble(tokens = tokensArray, region = names(tokensArray), numericToken = NA)
  
  # Pairing down to just those effects that have numerics
  numericStrings <- dplyr::filter(stringSplit, group > -1)

  # Identifying the types of each numberic
  numericStrings$magnitudeType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste0("^", MAGNITUDE_KEYS, "$", collapse = "|"),
                                       ignore_case = T))
  
  numericStrings$tenType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste0("^", TEN_KEYS, "$", collapse = "|"),
                                       ignore_case = T))
  
  numericStrings$unitType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste0("^", UNIT_KEYS, "$", collapse = "|"),
                                       ignore_case = T))
  
    
  # Instegating words breaking rules 
  # Two units or unit - tens next to each other should be broken apart 
  for(groups in unique(numericStrings$group)) {
    # Extracting numbers only
    numericsOnly <-
      dplyr::filter(numericStrings, numericStrings$numberBinary, group ==
                      groups)
    if (nrow(numericsOnly) < 2) {
      # If there is only one element it doesn't need to change
      numericsOnly$tochange <- FALSE
      # Else if there are multiple, we need to check that there are not any illegal characters
    } else if (nrow(numericsOnly) > 1) {
      #L <- 1:nrow(numericsOnly)
      pairs_to_test <-
        tibble::tibble(e1 = 1:(nrow(numericsOnly) - 1),
                       e2 = 2:nrow(numericsOnly))
      # Figuring out break points, i.e., splitting where there are (note that 
      # this takes e2 as the position to split at, i.e., e1 is the preceeding
      # value and e2 is the one which is broken at)
    numericsOnly$tochange  <- c(FALSE,
      # Breaking to new number if  a unit type (1-19) was preceeded by a unit type (1-19)
      (numericsOnly$unitType[pairs_to_test$e1] & numericsOnly$unitType[pairs_to_test$e2]) |
      # Also breaking if a ten type (10,20,30, ..., 90) of was preceeded by a unit type
      (numericsOnly$unitType[pairs_to_test$e1] & numericsOnly$tenType[pairs_to_test$e2]) |
        # And breatking if a a ten type is preceeded by a ten type 
    (numericsOnly$tenType[pairs_to_test$e1] & numericsOnly$tenType[pairs_to_test$e2]) | 
     # And breaking if a number is preceeded by itself
    (tolower(numericsOnly$stringSplit[pairs_to_test$e1]) == (tolower(numericsOnly$stringSplit[pairs_to_test$e2])))
      )
    }
    
    if(nrow(numericsOnly) > 3) {
      triplets_to_test <- 
        tibble::tibble(e1 = 1:(nrow(numericsOnly) - 2),
                       e2 = 2:(nrow(numericsOnly) - 1), 
                       e3 = 3:nrow(numericsOnly))
      numericsOnly$tochange <- c( 
        # Note that unlike the doubles, this "centers" on the middle value 
        # (i.e., e2 is still the value at which the break happens, not the last value)
        # This means that we cannot change the first or the last value's break 
        numericsOnly$tochange[c(1)],
        numericsOnly$tochange[-c(1, nrow(numericsOnly))] | 
        #  If a mangnitude is followed by a magnitude, and the latter magnitude is larger than the first
        # (e.g., "twenty thousand, one million" as compared to "one million, twenty thousand")
          as.numeric(NUMBER[tolower(numericsOnly$stringSplit[triplets_to_test$e1])]) <= 
          as.numeric(NUMBER[tolower(numericsOnly$stringSplit[triplets_to_test$e3])]) &
          numericsOnly$magnitudeType[triplets_to_test$e1] & 
          numericsOnly$magnitudeType[triplets_to_test$e3],
        numericsOnly$tochange[nrow(numericsOnly)]
    )
      
      
      # breaking if a non-magnitude (not 100, 1000, 10000 or etc. is preceeded by two non-magnidues) ?
    
    }
    #   numericsOnly$tochange  <- c(FALSE,
    #                               (numericsOnly$unitType[pairs_to_test$e1] & numericsOnly$unitType[pairs_to_test$e2]) |
    #                               (numericsOnly$unitType[pairs_to_test$e1] & numericsOnly$tenType[pairs_to_test$e2]) |
    #                               (numericsOnly$tenType[pairs_to_test$e1] & numericsOnly$tenType[pairs_to_test$e2]) |
    #   )
    #   
    #  }
    numericsOnly$group <- stringr::str_c("a", numericsOnly$group, cumsum(numericsOnly$tochange))
    
    
  # Updating numeric strings withupdated groups
    numericStrings[match(numericsOnly$id, numericStrings$id),] <- dplyr::select(numericsOnly, -tochange)
  
  ## NEED TO ALSO add in same level magnitude check - i.e., one thousand one thousand should be 1000 1000
  }
  # Dropping unchanging tokens (i.e., those that are not required, like punctuation that should not be altered)
  numericStrings <- dplyr::filter(numericStrings, !is.na(groups))
  
    # Reassigning the now ungrouped non-numerics 
  numericStrings$group[!stringr::str_detect(numericStrings$group, "^a\\d")] <- NA
  numericStrings$group <-
    ifelse(
      tidyr::fill(numericStrings, group,  .direction = "down")$group == tidyr::fill(numericStrings, group,  .direction =  "up")$group,
      tidyr::fill(numericStrings, group,  .direction = "down")$group,
      NA
    )
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
        if(position > 1) {
        previousSum <-
          sum(numericsOnly$number[startCountingFrom:(position - 1)]) # begining or previous highest magnitude
        value <- previousSum * numericsOnly$number[position]
        numericsOnly$number[position] <- value
        numericsOnly$number[startCountingFrom:(position - 1)] <- 0
        }
        #  numericsOnly$number[position] <- numericsOnly$number[position]
      }
    }
    return(format(sum(numericsOnly$number), scientific = F))
}
  
  numericedOutput <- stringSplit
  
  for(groups in unique(na.omit(numericStrings$group))) {
    ids <- dplyr::filter(numericStrings, group == groups)$id
    # Blanking out the non-used numbers and repacing strings with numbers
  numericedOutput$number[numericedOutput$id %in% ids][1] <- 
      identifyNumbers(numericStrings[numericStrings$group == groups,])
    numericedOutput$stringSplit[numericedOutput$id %in% ids] <- ""
    numericedOutput$stringSplit[numericedOutput$id %in% ids][1] <-
      numericedOutput$number[numericedOutput$id %in% ids][1]
  }
  
return(paste0(numericedOutput$stringSplit, collapse = ""))
}

