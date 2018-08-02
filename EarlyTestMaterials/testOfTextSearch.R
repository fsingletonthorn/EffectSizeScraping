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



### ADD OPTION TO INCLUDE OR DISCARD EFFECT SIZES IN ABSTRACT / each section - AND TO REMOVE THEM IF THEY ARE REPEATED, or just to report if the values show up in both


# starting again: 
# extracting full files

# functioning with xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4547492.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/3929047.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4879183.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/5341264.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/5588100.xml")
xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4547424.xml")


# Extracting xml full ID 
# e.g., "oai:pubmedcentral.nih.gov:4547492"
ID <- xmlList[["GetRecord"]][["record"]][["header"]][["identifier"]]

# Extracting PMCID 
PMCID <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["article-id"]][["text"]]

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

# issue volume  NEEED TO FIX THIS ! ! ! ! !  ! !  ! ! !
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
# gathering locations of 'ps' i.e., paragraphs, the first file I looked at doesn't have a section head for the intro - this may or may not be consistent 
lPs<-which(names(body)=="p")

# setting these to be the body paragraphs
introSection <- body[lPs]
# gathering locations of sections 
lCs<-which(names(body)=="sec")

# finding the title of each section
sections <- body[lCs]
titles <- rep(0, length(sections))
for(j in 1:length(sections)){
  titles[j]<-sections[[j]][['title']]
}


# seperating the sections out - this will break if these sections are not similarly titled
if(sum(titles == "Introduction")>0) introSection <- list(introSection, sections[[which(titles == "Introduction")]])

if(sum((titles == "Method")|(titles == "Subjects and methods")|(titles == "Materials and Methods")) >0) methodsSection <- sections[[which((titles == "Method")|(titles == "Subjects and methods") |(titles == "Materials and Methods")>0)]]

if(sum(titles == "Results")>0) resultsSection <- sections[[which(titles == "Results")]]

if(sum(titles == "Discussion")>0) discussionSection <- sections[[which(titles == "Discussion")]]

  # clearing from memory
body <- NULL

# saving the text of each section
abstractText <- paste(unlist(xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["abstract"]]), collapse=' ')
introText <- paste(unlist(introSection), collapse=' ')
methodsText <-paste(unlist(methodsSection), collapse=' ')
resultsText <- paste(unlist(resultsSection), collapse=' ')
discussionText <-  paste(unlist(discussionSection), collapse=' ')

# specifying patterns to match - includes either "-" or "\u2212" which are alternative minus signs, captures whitespace which is removed later
#         begins with((white space or '('))(effect size letter [and possible 'p' and or '2' for eta, possible '(df)' for r, all aspects optional)(whitespace? '=' whitespace? ) before ('-' or 'Minus sign unicode') whitespace? digit(indeterminate length or 0 length) optional decimal, digit(s) indeterminate length required)
patternD <- "(?<=((\\s|\\()[d]\\s?(//(95% confidence interval//))?(//(95% CI//))?)\\s?\\s?[\\.\\,\\:\\;]?\\s?\\s?[=]\\s?\\s?)(\\-?\u2212?\\s?\\d*\\.?\\d{1,})"
patternR <-  "(?<=((\\s|\\()([r]s?\\(?\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\d?\\)?\\s?\\s?[=]\\s?\\s?)))(\u2212?\\-?\\s?\\d*\\.?\\d{1,})"
patternEta <- "(?<=((\\s|\\())([\u03B7]\\s?p?\\s?2?\\s?\\s?[=]\\s?\\s?))(\\-?\u2212?\\s?\\d*\\.?\\d{1,})" 

# it may be possible to add in other text versions of this (e.g., "correlation coefficient of")

# function to extract text, remove whitespaces and unicode encodings of the minus sign and return numeric vector of extracted effect sizes
extractEffects <- function(inputText, pattern) {
  # extracting all text which matches pattern
  extracted <- str_extract_all(inputText,  pattern, simplify = T)
  # removing whitespace that can be captured in the regular expressions above
  extracted <- str_remove_all(extracted, "\\s")
  # replacing unicode minus sign with R recognised hyphen/minus sign
  extracted <- str_replace_all(extracted, "\u2212", "-") 
  return(as.numeric(extracted))
}

# Extracting from abstract 
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

