# install.packages('XML')
# install.packages("oai")
# install.packages("xml2")
library(XML)
# library(xml2)
library(oai) # for downloading things late
library(magrittr)
library(tidyverse) 
library(stringr) 

# remeber for rolling assignment of names: 
for(i in 1:2) {
assign(paste0("abc",i,"xyz"), i)
}
abc1xyz
abc2xyz

# bulk download avaliable at ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/ 


### ADD OPTION TO INCLUDE OR DISCARD EFFECT SIZES IN ABSTRACT / each section
# - AND TO REMOVE THEM IF THEY ARE REPEATED, or just to report if the values show up in both


# starting again: 
# extracting full files

# functioning with xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4547492.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/3929047.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4879183.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/5341264.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/5588100.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4547424.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/3659440.xml")
xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/5110632.xml")

xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC3778923.nxml")



# Extracting xml full ID 
# e.g., "oai:pubmedcentral.nih.gov:4547492"
# manual DL version - ID <- xmlList[["GetRecord"]][["record"]][["header"]][["identifier"]]
ID <- xmlList[["front"]][["article-meta"]][["title-group"]][["article-title"]]

# Extracting PMCID 
# manual DL version - PMCID <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["article-id"]][["text"]]
PMCID <- xmlList[["front"]][["article-meta"]][["article-id"]][["text"]]

# Extracting DOI
# <article-id pub-id-type="doi">10.1016/j.jbtep.2015.05.001</article-id>
DOI <- unlist(xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]])
# extracting the element of the dataframe which is 1 below the "article attribute" called "doi"
DOI <- DOI[which(names(DOI) == "article-id..attrs.pub-id-type")[DOI[which(names(DOI) == "article-id..attrs.pub-id-type")] == "doi"]-1]

# Abbreviated journal ID 
# e.g., "J Behav Ther Exp Psychiatry"
journalId <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["journal-meta"]][["journal-id"]][["text"]]

## Extracting full journal title
journal <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["journal-meta"]][["journal-title-group"]][["journal-title"]]

# issue volume
issue <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["issue"]]
volume <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["volume"]]

# pub date 
pubDate <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["pub-date"]]

# Article title
title<-xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["title-group"]][["article-title"]]

# Article key words
keywords <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["kwd-group"]]
# removing keywords which are "keywords"
if(sum(names(keywords) == "title")>0) keywords <- keywords[-which(names(keywords) == "title")]

# extracting author information 
# counting number of authors 
# nAuthors<-length(xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["contrib-group"]])

# extracting author names
# first extracting the entier
contribGroup <-  unlist(xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["contrib-group"]], use.names = T)
firstNames <- contribGroup[names(contribGroup) == "contrib.name.given-names"]
lastNames <- contribGroup[names(contribGroup) == "contrib.name.surname"]
contribGroup <- NULL

authors <- data.frame(firstNames, lastNames)

# Extracting text 
# probably don't need head ~ head <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]]
body <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["body"]]


# gathering locations of sections 
lCs<-which(names(body)=="sec")

# finding the title of each section
sections <- body[lCs]
titles <- rep(0, length(sections))
for(j in 1:length(sections)){
  titles[j]<-sections[[j]][['title']]
}


# search strings for each of the sections of the paper 
introNames <- ("Introduction|Background")
methodsNames <- ("method|aims|measur")
resultsNames<-("result")
discussionNames<-("discussion|conclusion|conclud")


# gathering locations of 'ps' i.e., paragraphs 
lPs<-which(names(body)=="p")

# anything that is not under a section will be put in unlab (i.e., unlabled) 
unlabSection <- body[lPs]


##### THIS SECTION HAS NOT BEEN TESTED - no articles have had additional sections that should not be includd in the other bits

if(sum(!str_detect(titles, regex(paste(introNames, methodsNames, resultsNames, discussionNames, sep = "|"), ignore_case = T)))>0) {
  for(j in which(str_detect(titles, regex(introNames, ignore_case = T)))) {
    unlabSection[[length(introSection)+1]] <- list(sections[[j]])
  }
}

# seperating the sections by, if there are any sections titled matching the section heads
# looping through each of the sections that match the names and putting them in a list -
introSection <- list()
if(sum(str_detect(titles,(regex(introNames, ignore_case = T))))>0) {
  for(j in which(str_detect(titles, regex(introNames, ignore_case = T)))) {
    introSection[[length(introSection)+1]] <- list(sections[[j]])
  }
}

methodsSection <- list()
if(sum(str_detect(titles,(regex(methodsNames, ignore_case = T))))>0) {
  for(j in which(str_detect(titles, regex(methodsNames, ignore_case = T)))) {
    methodsSection[[length(methodsSection)+1]] <- list(sections[[j]])
  }
}

resultsSection <- list()
if(sum(str_detect(titles,(regex(resultsNames, ignore_case = T))))>0) {
  for(j in which(str_detect(titles, regex(resultsNames, ignore_case = T)))) {
    resultsSection[[length(resultsSection)+1]] <- list(sections[[j]])
  }
}


discussionSection <- list()
if(sum(str_detect(titles,(regex(discussionNames, ignore_case = T))))>0) {
  for(j in which(str_detect(titles, regex(discussionNames, ignore_case = T)))) {
    discussionSection[[length(discussionSection)+1]] <- list(sections[[j]])
  }
}



  # clearing from memory
body <- NULL

# saving the text of each section

abstractText <- paste(unlist(xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["abstract"]]), collapse=' ')
unlabText<- paste(unlist(unlabSection), collapse =' ')
introText <- paste(unlist(introSection), collapse=' ')
methodsText <-paste(unlist(methodsSection), collapse=' ')
resultsText <- paste(unlist(resultsSection), collapse=' ')
discussionText <-  paste(unlist(discussionSection), collapse=' ')

# specifying patterns to match - includes either "-" or "\u2212" which are alternative minus signs, captures whitespace which is removed later
#         begins with((white space or '('))(effect size letter [and possible 'p' and or '2' for eta, possible '(\d\d)' for r, all aspects optional)(whitespace? '=' whitespace? ) before ('-' or 'Minus sign unicode') whitespace? digit(indeterminate length or 0 length) optional decimal, digit(s) indeterminate length required)
patternD <- "(?<=((\\s|\\()[d]\\s?\\s?(//(95% confidence interval//))?(//(95% CI//))?)\\s?\\s?[\\.\\,\\:\\;]?\\s?\\s?([=]|(of))\\s?\\s?)(\\-?\u2212?\\s?\\d*\\.?\\d{1,})"
patternR <-  "(((?<=((\\s|\\(|\\[)([r]s?\\(?\\d{0,10}\\)?\\s?\\s?[=]\\s?\\s?)))|((?<=(correlation)\\s?\\s?(coefficient)?\\s?\\s?([=]|(of))\\s?\\s?)))(\u2212?\\-?\\s?\\d*\\.?\\d{1,}))"
patternEta <- "(((?<=((\\s|\\())([\u03B7]\\s?p?\\s?2?\\s?\\s?([=]|(of))\\s?\\s?))|((?<=((partial)?\\s?eta\\ssquared\\s?\\=?(of)?\\s?))))(\\-?\u2212?\\s?\\d*\\.?\\d{1,}))" 

# it may be possible to add in other text versions of this (e.g., "correlation coefficient of")

# function to extract text, remove whitespaces and unicode encodings of the minus sign and return numeric vector of extracted effect sizes
extractEffects <- function(inputText, pattern) {
  # removing newline breaks
  inputText <- str_remove_all(inputText, "\\n")
  # extracting all text which matches pattern
  extracted <- str_extract_all(inputText,  regex(pattern, ignore_case = TRUE), simplify = T)
  # removing whitespace that can be captured in the regular expressions above
  extracted <- str_remove_all(extracted, "\\s")
  # replacing unicode minus sign with R recognised hyphen/minus sign
  extracted <- str_replace_all(extracted, "\u2212", "-") 
  return(as.numeric(extracted))
}


# Extracting from sections which were not covered above 
extractedDsIntro <- extractEffects(introText, patternD)
extractedRsIntro <- extractEffects(introText, patternR)
extractedEtasIntro <- extractEffects(introText, patternEta)

# Extracting from abstract which were not covered above 
extractedDsAbstract <- extractEffects(abstractText, patternD)
extractedRsAbstract <- extractEffects(abstractText, patternR)
extractedEtasAbstract <- extractEffects(abstractText, patternEta)

# Extracting from intro
extractedDsIntro <- extractEffects(introText, patternD)
extractedRsIntro <- extractEffects(introText, patternR)
extractedEtasIntro <- extractEffects(introText, patternEta)

# Extracting from methods
extractedDsMethods <- extractEffects(methodsText, patternD)
extractedRsMethods  <- extractEffects(methodsText, patternR)
extractedEtasMethods  <- extractEffects(methodsText, patternEta)

# extracting from results
extractedDsResults <- extractEffects(resultsText, patternD)
extractedRsResults  <- extractEffects(resultsText, patternR)
extractedEtasResults  <- extractEffects(resultsText, patternEta)

# extracting from discussion 
extractedDsDiscussion <- extractEffects(discussionText, patternD)
extractedRsDiscussion  <- extractEffects(discussionText, patternR)
extractedEtasDiscussion  <- extractEffects(discussionText, patternEta)


# later to update it will be possible to just compare the master list with those 
record<-get_records(ids = "oai:pubmedcentral.nih.gov:4547492", prefix = "pmc", url =  'https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi')

record <- xmlToList(record[[1]])

