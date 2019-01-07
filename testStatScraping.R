# install.packages('XML')
# install.packages("oai")
# install.packages("xml2")
#library(XML)
library(xml2)
library(oai) # for downloading things later
library(magrittr)
library(tidyverse) 
library(stringr) 

# tests are at ("tests.R")

## This is based on https://github.com/titipata/pubmed_parser
# bulk download avaliable at ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/ 

# "... So if you're not sure in advance how many elements you need to store, use a list, and then collect the results at the end with sapply or similar ..."

################### Ideas for app 
# Discard effect sizes that are repeated in different sections of the paper - i.e., only include them once


############## Extraction program

start<- Sys.time()
# Function to read PMC open file as identified by the PM ID 
download.pmc <- function(id = "oai:pubmedcentral.nih.gov:3659440") {
  read_html(paste0("https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=", id, "&metadataPrefix=pmc"))
}

# example with coheon's d's: "oai:pubmedcentral.nih.gov:4879183", correlations: oai:pubmedcentral.nih.gov:5588100

paper<-download.pmc()

## Metadata extraction
# PMID
pmID_node <- xml_find_first(paper, '//article-id[@pub-id-type="pmid"]')
# PMC ID 
pmcID_node <- xml_find_first(paper, '//article-id[@pub-id-type="pmc"]')
# Publisher 
publisher_node <- xml_find_first(paper, '//article-id[@pub-id-type="publisher-id"]')
# DOI 
doi_node <- xml_find_first(paper, '//article-id[@pub-id-type="doi"]')
# Journal name
journalID_node <- xml_find_first(paper, '//front/journal-meta/journal-title-group/journal-title')
# Jounral name abbreviation 
journalIDAbrev_node <- xml_find_first(paper, '//front/journal-meta/journal-id')
# Article issue 
issue_node <- xml_find_first(paper, '//front/article-meta/issue/text()')
# Article volume 
volume <- xml_find_first(paper, "//front/article-meta/volume")
# date print pub
pPub_node <- xml_find_first(paper, '//pub-date[@pub-type="ppub"]')
# date epub
ePub_node <- xml_find_first(paper, '//pub-date[@pub-type="epub"]')
# date release on pmc
pmcPub_node <- xml_find_first(paper, '//pub-date[@pub-type="pmc-release"]')
# Article title
title_node<- xml_find_first(paper, '//front/article-meta/title-group/article-title')
# Article key words
keywords_node <- xml_find_all(paper, '//front/article-meta/kwd-group/kwd')

### Authors and Author Affiliation
# Author afffiliations
# Author institution ID number (links to author list)
affil_ID_node <- xml_find_all(paper, '//aff[@id]/@id')
# Author institution name
affil_Names_node <- xml_find_all(paper, '//aff[@id]')

# author names:
AuthorSurnmes_node <- xml_find_all(paper, '//contrib[@contrib-type="author"]/name/surname/text()')
AuthorFirstNames_node <- xml_find_all(paper, '//contrib[@contrib-type="author"]/name/given-names/text()')
AuthorAfil_node <- xml_find_all(paper, '//contrib-group/contrib[@contrib-type="author"]/xref[@ref-type="aff"]')

# Abstract 
abstract_node <- xml_find_all(paper, "//abstract")
# unlablled paragraphs 
unlabPs_nodes <- xml_find_all(paper, "//body/p")
# article sections
sections <-  xml_find_all(paper, "//article/sec")
# article section titles
titles <-  xml_find_all(sections, "title")

# search strings for each of the sections of the paper 
introNames <- ("Introduction|Background")
methodsNames <- ("method|aims|measur")
resultsNames<-("result")
discussionNames<-("discussion|conclusion|conclud|summary")

# add abstract 
abstractText <- xml_text(abstract_node)

##### The unlab section HAS NOT BEEN TESTED !!! - no articles have had additional sections that should not be includd in the other bits
if(sum(!str_detect(titles, regex(paste(introNames, methodsNames, resultsNames, discussionNames, sep = "|"), ignore_case = T)))>0) {
  index <- which(!str_detect(titles, regex(paste(introNames, methodsNames, resultsNames, discussionNames, sep = "|"), ignore_case = T)))
  unlabSection <- xml_text(sections[index])
}

# Adding in any untitled paragraphs here, not tested !!! 
if(length(unlabPs_nodes)>0) {
  unlabText[[length(unlabSection)+1]] <- xml_text(unlabPs_nodes)
  unlabText <- paste(unlist(unlabSection), collapse =' ')
}

# seperating the sections by, if there are any sections titled matching the section heads
# looping through each of the sections that match the names and putting them in a list -
introText <- NA
if(sum(str_detect(titles,(regex(introNames, ignore_case = T))))>0) {
  index <- which(str_detect(titles, regex(introNames, ignore_case = T)))
  introText <- xml_text(sections[index])
}

methodsText <- NA
if(sum(str_detect(titles,(regex(methodsNames, ignore_case = T))))>0) {
  index <- which(str_detect(titles, regex(methodsNames, ignore_case = T)))
  methodsText <- xml_text(sections[index])
}

resultsText <- NA
if(sum(str_detect(titles,(regex(resultsNames, ignore_case = T))))>0) {
  index <- which(str_detect(titles, regex(resultsNames, ignore_case = T)))
  resultsText <- xml_text(sections[index])
}

discussionText <- NA
if(sum(str_detect(titles,(regex(discussionNames, ignore_case = T))))>0) {
  index <- which(str_detect(titles, regex(discussionNames, ignore_case = T))) 
  discussionText <- xml_text(sections[index])
}


# specifying patterns to match - includes either "-" or "\u2212" which are alternative minus signs, captures whitespace which is removed later
#         begins with((white space or '('))(effect size letter [and possible 'p' and or '2' for eta, possible '(\d\d)' for r, all aspects optional)(whitespace? '=' whitespace? ) before ('-' or 'Minus sign unicode') whitespace? digit(indeterminate length or 0 length) optional decimal, digit(s) indeterminate length required)

## at this point partial eta squared and eta are collected together, may as well seperate those. 
# Same with d and g. 
# Worth collecting R^2 ?? 
# Collect others ~ e.g., Epsilon squared and Omega squared ( which are ~ equivilent to eta), maybe with partial version of each in there. 


## Patterns extracted - strip commas and whitespace? too

# F and T tests - 
# Need to add a negative look behind for both to ensure that there are no letters beforehand

patternT <- "t\\s{0,}\\(\\s{0,}\\d{1,}\\.?\\d{0,}\\s{0,}\\)\\s{0,}=\\s{0,}-?\\s{0,}\\d{0,}\\.?\\d{0,}"
patternF <-  "F\\s{0,}\\(\\s{0,}\\d{1,},\\s{0,}\\d{1,}\\s{0,}\\)\\s{0,}=\\s{0,}\\d{0,}\\.?\\d{0,}"

patterns <- c(patternT, patternF)

# For this to work it needs to be fed a single string 
# function to extract text, remove whitespaces and unicode encodings of the minus sign and return test statistic original data plus df
extractTestStats <- function(inputText, context = FALSE, contextSize = 100) {
  # removing newline breaks, non-breaking spaces, '&#x000a0;', &#x00026;
  inputTextCleaned <- str_remove_all(inputText, "\\n")
  inputTextCleaned <- str_remove_all(inputTextCleaned, "[Ââˆ\\’Ï„œ€$!\\“\u009d]")
  # replacing unicode minus sign with R recognised hyphen/minus sign
  inputTextCleaned <- str_replace_all(inputTextCleaned, "\u2212", "-")

    # extracting all text which matches patterns
  extracted <- str_extract_all(inputTextCleaned,  regex(patterns, ignore_case = TRUE))
  
  # removing whitespace that can be captured in the regular expressions above
  extractedClean <- lapply(extracted,  str_remove_all, pattern = "\\s")
# Returing the right thing based on whether context was gathered 
  
  # The following extracts the context around each extracted part:
    # WILL ALSO HAVE TO REMOVE NEGATIVE LOOK AROUNDS IF THEY ARE ADDED !!! 
if(context == T) {   
  newSearchT  <- extracted[[1]] %>%
    str_replace_all("\\.", "\\\\.") %>%
    str_replace_all("\\(", "\\\\(") %>%
    str_replace_all("\\)", "\\\\)") %>% 
    paste0(".{0,", contextSize,"}",  . ,".{0,", contextSize,"}")
  
  extractedContextT <- as.character(str_extract(inputTextCleaned,  regex(newSearchT, ignore_case = TRUE)))
  
  newSearchF  <- extracted[[2]] %>%
    str_replace_all("\\.", "\\\\.") %>%
    str_replace_all("\\(", "\\\\(") %>%
    str_replace_all("\\)", "\\\\)") %>% 
    paste0(".{0,", contextSize,"}",  . ,".{0,", contextSize,"}")
  
  extractedContextF <- as.character(str_extract(inputTextCleaned,  regex(newSearchF, ignore_case = TRUE)))
 
  return(rbind(data_frame(statistic = "t", cleaned = extractedClean[[1]], reported = extracted[[1]], context = extractedContextT),
               data_frame(statistic = "F", cleaned = extractedClean[[2]], reported = extracted[[2]],  context = extractedContextF))) ## , fCleaned = , FReported =, ))  
} else {
    return(rbind(data_frame(tCleaned = extractedClean[[1]], tReported = extracted[[1]]), data_frame(FCleaned = extractedClean[[2]], FReported = extracted[[2]]))) ## , fCleaned = , FReported =, ))  
  }
}

# Now just need to extract the effect sizes from each 


extractTestStats(unlabText)
extractTestStats(introText)
extractTestStats(methodsText)
extractTestStats(resultsText, context = T)
extractTestStats(discussionText)
# Extracting from sections which were not covered above (later just lapply this across all of the sections, 
# probably also possible to change the above function to test for multiple effects, best to pilot it with the manual V first)
# LATER JUST DO THIS FOR EACH OF THE EFFECT SIZES: 
# texts <- list(unlabText, abstractText, introText, methodsText,  resultsText, discussionText)
# lapply(texts, extractEffects, patternD) # .. etc. (again, probably better to build extraction for multiple effects into the one function, 
# otherwise we're repeating opperations a lot for no reason)

if(exists(unlabText)) {
  extractedDsUnlab <- extractEffects(unlabText, patternD)
  extractedRsUnlab <- extractEffects(unlabText, patternR)
  extractedEtasUnlab <- extractEffects(unlabText, patternEta)
  extractedHRsUnlab <-  extractEffects(unlabText, patternHR)
  extractedORsUnlab <-  extractEffects(unlabText, patternOR)
}

# Extracting from abstract which were not covered above 
extractedDsAbstract <- extractEffects(abstractText, patternD)
extractedRsAbstract <- extractEffects(abstractText, patternR)
extractedEtasAbstract <- extractEffects(abstractText, patternEta)
extractedHRsAbstract <-  extractEffects(abstractText, patternHR)
extractedORsAbstract <-  extractEffects(abstractText, patternOR)

# Extracting from intro
extractedDsIntro <- extractEffects(introText, patternD)
extractedRsIntro <- extractEffects(introText, patternR)
extractedEtasIntro <- extractEffects(introText, patternEta)
extractedHRsIntro<-  extractEffects(introText, patternHR)
extractedORsIntro <-  extractEffects(introText, patternOR)

# Extracting from methods
extractedDsMethods <- extractEffects(methodsText, patternD)
extractedRsMethods  <- extractEffects(methodsText, patternR)
extractedEtasMethods  <- extractEffects(methodsText, patternEta)
extractedHRsMethods <-  extractEffects(methodsText, patternHR)
extractedORsMethods <-  extractEffects(methodsText, patternOR)

# extracting from results
extractedDsResults <- extractEffects(resultsText, patternD)
extractedRsResults  <- extractEffects(resultsText, patternR)
extractedEtasResults  <- extractEffects(resultsText, patternEta)
extractedHRsResults <-  extractEffects(resultsText, patternHR)
extractedORsResults <-  extractEffects(resultsText, patternOR)

# extracting from discussion 
extractedDsDiscussion <- extractEffects(discussionText, patternD)
extractedRsDiscussion  <- extractEffects(discussionText, patternR)
extractedEtasDiscussion  <- extractEffects(discussionText, patternEta)
extractedHRsDiscussion <-  extractEffects(discussionText, patternHR)
extractedORsDiscussion <-  extractEffects(discussionText, patternOR)

end<- Sys.time()
start-end
# later to update it will be possible to just compare the master list with those 

