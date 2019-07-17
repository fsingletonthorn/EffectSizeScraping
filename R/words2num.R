# words2Nums
### Setting up constants
##  This is inspired by https://github.com/finnfiddle/words-to-numbers
importFrom(magrittr,"%>%")

UNIT <- list(
  zero = 0,
  first = 1,
  one = 1,
  second = 2,
  two = 2,
  third = 3,
  thirteenth = 13,
  thirteen = 13,
  three = 3,
  fourth = 4,
  fourteenth = 14,
  fourteen = 14,
  four = 4,
  fifteenth = 15,
  fifteen = 15,
  fifth = 5,
  five = 5,
  sixth = 6,
  sixteenth = 16,
  sixteen = 16,
  six = 6,
  seventeenth = 17,
  seventeen = 17,
  seventh = 7,
  seven = 7,
  eighteenth = 18,
  eighteen = 18,
  eighth = 8,
  eight = 8,
  nineteenth = 19,
  nineteen = 19,
  ninth = 9,
  nine = 9,
  tenth = 10,
  ten = 10,
  eleventh = 11,
  eleven = 11,
  twelfth = 12,
  twelve = 12
)

TEN <- list(
  twenty = 20,
  twentieth = 20,
  thirty = 30,
  thirtieth = 30,
  forty = 40,
  fortieth = 40,
  fifty = 50,
  fiftieth = 50,
  sixty = 60,
  sixtieth = 60,
  seventy = 70,
  seventieth = 70,
  eighty = 80,
  eightieth = 80,
  ninety = 90,
  ninetieth = 90
)


MAGNITUDE = list(
  hundred = 100,
  hundredth = 100,
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

JOINERS <- 'and'
DECIMALS <- c('point', 'dot')

PUNCTUATION <- c(
  ' ',
  ',',
# Comment out the below later because we actually don't want to capture units 
# across any of these punctuation markers, they have been left 
# 
 '.',
 '\\',
 '#',
 '!',
 '$',
 '%',
 '^',
 '&',
 '/',
 '*',
';',
 ':',
 '{',
 '}',
 '=',
 '-',
 '_',
 '`',
 '~',
 '(',
 ')'
)

TOKEN_TYPE <- list(
  UNIT = 0,
  TEN = 1,
  MAGNITUDE = 2,
  DECIMAL =  3,
  HUNDRED = 4
)

ALL_WORDS <- c(NUMBER_WORDS, JOINERS, DECIMALS)

BLACKLIST_SINGULAR_WORDS <- 'a'

###
# Identify all strings of the above with one or more numbers words, but accepting 
# joiners and punctuation w/out breaking strings

# MOVE THIS TO TESTS AFTERWARDS
example_numerics <- "133. The PRQ is a 12-item, 4-point Likert scale
(from 1 = Never to 4 = Very Often) with 3 sub-scores: 
bullying (PRQ-Bully), being victimized (PRQ-Victim), 
and pro-social behavior (PRQ-Prosocial). A translated, 
backtranslated final Arabic version of the scale was found 
to be accurate showing good internal consistency in this 
sample [“PRQ-Victim” (alpha = .74)
and “PRQ-Bullies” (alpha = 74)]. 1212. And 12"

example <- "One-hundred and thirty three. The PRQ is a twelve-item, four-point Likert scale
(from one = Never to four = Very Often) with three sub-scores: 
bullying (PRQ-Bully), being victimized (PRQ-Victim), 
and pro-social behavior (PRQ-Prosocial). A translated, 
backtranslated final Arabic version of the scale was found 
to be accurate showing good internal consistency in this 
sample [“PRQ-Victim” (alpha = .seventy four) and “PRQ-Bullies” 
(alpha = seventy-four)]. One thousand two hundred. And twelve"


### Parse the numbers
string <- example

# canAddTokenToEndOfSubRegion <- function(tokens, subRegion, currentToken, impliedHundreds){
  # split at whitespace 
  stringSplit <- stringr::str_split(string,  "(?<=[[[:punct:]]|\\s])|(?=[[[:punct:]]|\\s])", simplify = T)
  
  # create binaries for whitespace and for number words
  punctuationBinary <- stringr::str_detect(stringSplit, "[[:punct:]]|\\s|and")
  # Ensuring that "." breaks numbers apart
  punctuationBinary[stringr::str_detect(stringSplit, "\\.") ] <- FALSE
  numberBinary <- stringr::str_detect(stringSplit, stringr::regex(paste( NUMBER_WORDS, collapse = "|"), ignore_case = T))
  
  # concatinate contigous regions
  stringSplit <- tibble::tibble(stringSplit = as.character(stringSplit), punctuationBinary, numberBinary,  stringsAsFactors = FALSE)
  
  # Creating a grouping variable
# punctuationOrNumberNotFirstOrLast <- stringSplit$punctuationBinary | stringSplit$numberBinary
  stringSplit$group <- NA
  stringSplit$group[1] <- ifelse(stringSplit$numberBinary[1], 1, 0) # Rewrtie to figure out if it is a numberic 
  
  # Creating groups of the numbered and unnumbered sections
  for (i in 2:nrow(stringSplit)) {
    stringSplit$group[i] <- ifelse(punctuationOrNumber[i - 1] == punctuationOrNumber[i],
      stringSplit$group[i - 1],stringSplit$group[i - 1] + 1)
  }
  
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
  
  #### Extracting all pf the numbers
  tokensArray <- tapply(stringSplit$stringSplit, INDEX = stringSplit$group, FUN = paste0, simplify = T, collapse = " ")[-1]
  tokens <- tibble::tibble(tokens = tokensArray, region = names(tokensArray), numericToken = NA)
  
  
  tokens[i] <- subRegion
  prevToken <- tokens[i-1]
  if (!prevToken) return true;
  if (
    prevToken.type === TOKEN_TYPE.MAGNITUDE &&
    currentToken.type === TOKEN_TYPE.UNIT
  ) return true;
  if (
    prevToken.type === TOKEN_TYPE.MAGNITUDE &&
    currentToken.type === TOKEN_TYPE.TEN
  ) return true;
  if (
    impliedHundreds &&
    subRegion.type === TOKEN_TYPE.MAGNITUDE &&
    prevToken.type === TOKEN_TYPE.TEN &&
    currentToken.type === TOKEN_TYPE.UNIT
  ) return true;
  if (
    impliedHundreds &&
    subRegion.type === TOKEN_TYPE.MAGNITUDE &&
    prevToken.type === TOKEN_TYPE.UNIT &&
    currentToken.type === TOKEN_TYPE.TEN
  ) return true;
  if (
    prevToken.type === TOKEN_TYPE.TEN &&
    currentToken.type === TOKEN_TYPE.UNIT
  ) return true;
  if (
    !impliedHundreds &&
    prevToken.type === TOKEN_TYPE.TEN &&
    currentToken.type === TOKEN_TYPE.UNIT
  ) return true;
  if (
    prevToken.type === TOKEN_TYPE.MAGNITUDE &&
    currentToken.type === TOKEN_TYPE.MAGNITUDE
  ) return true;
  if (
    !impliedHundreds &&
    prevToken.type === TOKEN_TYPE.TEN &&
    currentToken.type === TOKEN_TYPE.TEN
  ) return false;
  if (
    impliedHundreds &&
    prevToken.type === TOKEN_TYPE.TEN &&
    currentToken.type === TOKEN_TYPE.TEN
  ) return true;
  return false;
};

const getSubRegionType = (subRegion, currentToken) => {
  if (!subRegion) {
    return { type: currentToken.type };
  }
  const prevToken = subRegion.tokens[0];
  const isHundred = (
    (prevToken.type === TOKEN_TYPE.TEN && currentToken.type === TOKEN_TYPE.UNIT) ||
      (prevToken.type === TOKEN_TYPE.TEN && currentToken.type === TOKEN_TYPE.TEN) ||
      (
        prevToken.type === TOKEN_TYPE.UNIT && currentToken.type === TOKEN_TYPE.TEN &&
          NUMBER[prevToken.lowerCaseValue] > 9
      ) ||
      (prevToken.type === TOKEN_TYPE.UNIT && currentToken.type === TOKEN_TYPE.UNIT) ||
      (prevToken.type === TOKEN_TYPE.TEN && currentToken.type === TOKEN_TYPE.UNIT && subRegion.type === TOKEN_TYPE.MAGNITUDE)
  );
  if (subRegion.type === TOKEN_TYPE.MAGNITUDE) return { type: TOKEN_TYPE.MAGNITUDE, isHundred };
  if (isHundred) return { type: TOKEN_TYPE.HUNDRED, isHundred };
  return { type: currentToken.type, isHundred };
};

const checkIfTokenFitsSubRegion = (subRegion, token, options) => {
  const { type, isHundred } = getSubRegionType(subRegion, token);
  if (!subRegion) return { action: START_NEW_REGION, type, isHundred };
  if (canAddTokenToEndOfSubRegion(subRegion, token, options)) {
    return { action: ADD, type, isHundred };
  }
  return { action: START_NEW_REGION, type, isHundred };
};

const getSubRegions = (region, options) => {
  const subRegions = [];
  let currentSubRegion;
  const tokensCount = region.tokens.length;
  let i = tokensCount - 1;
  while (i >= 0) {
    const token = region.tokens[i];
    const { action, type, isHundred } = checkIfTokenFitsSubRegion(currentSubRegion, token, options);
    token.type = isHundred ? TOKEN_TYPE.HUNDRED : token.type;
    switch (action) {
      case ADD: {
        currentSubRegion.type = type;
        currentSubRegion.tokens.unshift(token);
        break;
      }
      case START_NEW_REGION: {
        currentSubRegion = {
          tokens: [token],
          type,
        };
        subRegions.unshift(currentSubRegion);
        break;
      }
      // no default
    }
    i--;
  }
  return subRegions;
};

const canAddTokenToEndOfRegion = (region, currentToken, { impliedHundreds }) => {
  const { tokens } = region;
  const prevToken = tokens[tokens.length - 1];
  if (
    !impliedHundreds &&
    prevToken.type === TOKEN_TYPE.UNIT &&
    currentToken.type === TOKEN_TYPE.UNIT &&
    !region.hasDecimal
  ) return false;
  if (!impliedHundreds && prevToken.type === TOKEN_TYPE.UNIT && currentToken.type === TOKEN_TYPE.TEN) return false;
  if (!impliedHundreds && prevToken.type === TOKEN_TYPE.TEN && currentToken.type === TOKEN_TYPE.TEN) return false;
  return true;
};

const checkIfTokenFitsRegion = (region, token, options) => {
  const isDecimal = DECIMALS.includes(token.lowerCaseValue);
  if ((!region || !region.tokens.length) && isDecimal) {
    return START_NEW_REGION;
  }
  const isPunctuation = PUNCTUATION.includes(token.lowerCaseValue);
  if (isPunctuation) return SKIP;
  const isJoiner = JOINERS.includes(token.lowerCaseValue);
  if (isJoiner) return SKIP;
  if (isDecimal && !region.hasDecimal) {
    return ADD;
  }
  const isNumberWord = NUMBER_WORDS.includes(token.lowerCaseValue);
  if (isNumberWord) {
    if (!region) return START_NEW_REGION;
    if (canAddTokenToEndOfRegion(region, token, options)) {
      return ADD;
    }
    return START_NEW_REGION;
  }
  return NOPE;
};

const checkBlacklist = tokens =>
  tokens.length === 1 &&
  BLACKLIST_SINGULAR_WORDS.includes(tokens[0].lowerCaseValue);

const matchRegions = (tokens, options) => {
  const regions = [];
  
  if (checkBlacklist(tokens)) return regions;
  
  let i = 0;
  let currentRegion;
  const tokensCount = tokens.length;
  while (i < tokensCount) {
    const token = tokens[i];
    const tokenFits = checkIfTokenFitsRegion(currentRegion, token, options);
    switch (tokenFits) {
      case SKIP: {
        break;
      }
      case ADD: {
        if (currentRegion) {
          currentRegion.end = token.end;
          currentRegion.tokens.push(token);
          if (token.type === TOKEN_TYPE.DECIMAL) {
            currentRegion.hasDecimal = true;
          }
        }
        break;
      }
      case START_NEW_REGION: {
        currentRegion = {
          start: token.start,
          end: token.end,
          tokens: [token],
        };
        regions.push(currentRegion);
        if (token.type === TOKEN_TYPE.DECIMAL) {
          currentRegion.hasDecimal = true;
        }
        break;
      }
      case NOPE:
        default: {
          currentRegion = null;
          break;
        }
    }
    i++;
  }
  
  return regions.map(region => ({ ...region, subRegions: getSubRegions(region, options) }));
};

const getTokenType = (chunk) => {
  if (UNIT_KEYS.includes(chunk.toLowerCase())) return TOKEN_TYPE.UNIT;
  if (TEN_KEYS.includes(chunk.toLowerCase())) return TOKEN_TYPE.TEN;
  if (MAGNITUDE_KEYS.includes(chunk.toLowerCase())) return TOKEN_TYPE.MAGNITUDE;
  if (DECIMALS.includes(chunk.toLowerCase())) return TOKEN_TYPE.DECIMAL;
};

export default (text, options) => {
  const tokens = text
  .split(/(\w+|\s|[[:punct:]])/i)
  .reduce((acc, chunk) => {
    const unfuzzyChunk = chunk.length && options.fuzzy && !PUNCTUATION.includes(chunk) ?
      fuzzyMatch(chunk) :
      chunk;
    const start = acc.length ? acc[acc.length - 1].end + 1 : 0;
    const end = start + chunk.length;
    return end !== start ?
      acc.concat({
        start,
        end: end - 1,
        value: unfuzzyChunk,
        lowerCaseValue: unfuzzyChunk.toLowerCase(),
        type: getTokenType(unfuzzyChunk, options),
      }) :
      acc;
  }, []);
  const regions = matchRegions(tokens, options);
  return regions;
};


# Replace with numerics

