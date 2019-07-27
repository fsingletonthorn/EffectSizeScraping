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
# tenth = 10,  
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
# Detecting numerics
numericBinary <-  !is.na(suppressWarnings( as.numeric(stringSplitVec)) )

# Detecting numbers 
numberBinary <-
  stringr::str_detect(stringSplitVec, stringr::regex(
    paste("^", NUMBER_WORDS, "$", collapse = "|", sep = ""),
    ignore_case = T
  ))

# Quick exit if there are no numbers in words
if (sum(numberBinary[!numericBinary]) < 1) {
  return(string)
}
# And quick exit if there is only one number element
if (length(stringSplitVec) == 1) {
  return(as.character(format(NUMBER[[tolower(string)]], scientific = F)))
}

  # Making tibble
  stringSplit <-
    tibble::tibble(
      id = 1:length(numberBinary),
      stringSplit = as.character(stringSplitVec),
      punctuationBinary,
      # Note that this counts numbers as words or numbers as digits as numbers
      numberBinary = numericBinary | numberBinary,
      numericBinary
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
  last_position <- nrow(stringSplit)
  
  stringSplit$point <- stringr::str_detect(
    stringSplit$stringSplit,
    stringr::regex("point|dot", 
                   ignore_case = T)
    )
  stringSplit$tokenAheadPoint <- c(stringSplit$point[-1], F)
  stringSplit$tokenBehindPoint <-c(F, stringSplit$point[-last_position])
  
  stringSplit$space <- stringSplit$stringSplit == " "
  stringSplit$tokenAheadSpace <- c(stringSplit$space[-1], F)
  stringSplit$tokenBehindSpace <-c(F, stringSplit$space[-last_position])

  stringSplit$tokenAheadNumber <- c(stringSplit$numberBinary[-1], F)
  stringSplit$tokenBehindNumber <-c(F, stringSplit$numberBinary[-last_position])
  
  stringSplit$stringSplit <-
    ifelse(c(
      # If the token is a space AND
      stringSplit$space &
      # ... the token following the space is a point and before is a number, and the point is followed by anything, and then a number
      (stringSplit$tokenAheadPoint & stringSplit$tokenBehindNumber  & c(stringSplit$tokenAheadNumber[-(1:2)], F, F)) |
      # ... the token before the value is a space AND the token ahead is a point, which is in turn followed by a number (three ahead))))
      (stringSplit$tokenAheadNumber & stringSplit$tokenBehindPoint)
    ) 
      ,
    "", stringSplit$stringSplit)


  # Replace  "dot" or "point" with points 
  stringSplit$stringSplit <-
    ifelse(c(T, T, stringSplit$numberBinary[-((last_position-1):last_position)]) &
        c(stringSplit$numberBinary[-c(1, 2)], F, F) &
        stringSplit$point,
    ".", stringSplit$stringSplit)
  
  # initiallising number vector
  stringSplit$number <-
    ifelse(stringSplit$numericBinary, 
           yes = suppressWarnings(as.numeric(stringSplit$stringSplit)), 
           no = NA)
  
  # Pairing down to just those effects that have numerics
  numericStrings <- dplyr::filter(stringSplit, group > -1)

  # Identifying the types of each numberic
  numericStrings$magnitudeType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste0("^", MAGNITUDE_KEYS, "$", collapse = "|"),
                                       ignore_case = T)) | ifelse(!is.na(numericStrings$number), 
                                                                  as.numeric(numericStrings$number) %in% MAGNITUDE, F)
  
  numericStrings$tenType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste0("^", TEN_KEYS, "$", collapse = "|"),
                                       ignore_case = T)) |
    ifelse(!is.na(numericStrings$number),
           (
             as.numeric(numericStrings$number) == 10 |
               (
                 as.numeric(numericStrings$number) %in% TEN
               )
           )
           , F)
  
  numericStrings$unitType <-
    stringr::str_detect(numericStrings$stringSplit,
                        stringr::regex(paste0("^", UNIT_KEYS, "$", collapse = "|"),
                                       ignore_case = T)) |
    ifelse(!is.na(numericStrings$number), 
           # Counting as a unit if the token is a digit between 0 - 99 but not a 10 
           as.numeric(numericStrings$number) %in% seq(0, 99)[-c(seq(11,91, by = 10))], F)
    
  # Helper function to check whether the item is a number or matches a number and returns either the numeric or the string
  token_to_number <- function(tokens) {
    tokens <- tolower(tokens)
    unlist(purrr::map(tokens,
                      function(token) {
                        if (is.null(NUMBER[token][[1]])) {
                          return(as.numeric(token))
                        } else {
                          NUMBER[token]
                        }
                      }))
  }
  
  # Instegating number breaking - apart rules  
  # Two UNITs or UNIT - TEN next to each other should be broken apart 
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
      # Adding break if a larger number in digits is followed by a word in 
      # numbers of a smaller magnitude (e.g., "1000 hundred" or "100" ten)
        (numericsOnly$numericBinary[pairs_to_test$e1] & 
      token_to_number(numericsOnly$stringSplit[pairs_to_test$e2]) <=
      token_to_number(numericsOnly$stringSplit[pairs_to_test$e1]) ) |
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
        # unless the lower number is a hundred in which case we let it slide (one hundred twenty thousand makes sense)
          token_to_number(numericsOnly$stringSplit[triplets_to_test$e1]) <= 
          token_to_number(numericsOnly$stringSplit[triplets_to_test$e3]) &
          numericsOnly$magnitudeType[triplets_to_test$e1] & 
          numericsOnly$magnitudeType[triplets_to_test$e3], # &
          #(tolower(numericsOnly$stringSplit[triplets_to_test$e1]) != "hundred" &
         #    prod(unlist(NUMBER[tolower(numericsOnly$stringSplit[triplets_to_test$e1])])) < 100),
        numericsOnly$tochange[nrow(numericsOnly)]
    )
      
      
    }
  # Updating numeric strings withupdated groups
    numericsOnly$group <- stringr::str_c("a", numericsOnly$group, cumsum(numericsOnly$tochange))
    numericStrings[match(numericsOnly$id, numericStrings$id),] <- dplyr::select(numericsOnly, -tochange)
  }  
  
  ## NEED TO ALSO add in same level magnitude check - i.e., one thousand one thousand should be 1000 1000

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
    numericsOnly$number[is.na(numericsOnly$number)] <- 
      as.numeric(NUMBER[match(tolower(numericsOnly$stringSplit[is.na(numericsOnly$number)]), NUMBER_WORDS)])
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

