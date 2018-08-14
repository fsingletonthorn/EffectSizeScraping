# install.packages('XML')
# install.packages("oai")
# install.packages("xml2")
library(XML)
# library(xml2)
library(oai) # for downloading things late
library(magrittr)
library(tidyverse) 
library(stringr) 
library(xml2)


# remeber for rolling assignment of names: 
for(i in 1:2) {
assign(paste0("abc",i,"xyz"), i)
}
abc1xyz
abc2xyz

# bulk download avaliable at ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/ 


# "... So if you're not sure in advance how many elements you need to store, use a list, and then collect the results at the end with sapply or similar ..."


### ADD OPTION TO INCLUDE OR DISCARD EFFECT SIZES IN ABSTRACT / each section
# - AND TO REMOVE THEM IF THEY ARE REPEATED, or just to report if the values show up in both

# xmlDoc<- as_list(read_xml("EarlyTestMaterials/ExamplePapers/PMC5399602.nxml"))
# xml_find_all(xmlDoc, '//journal-id')

system.time(xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5472180.nxml"))
system.time(JID <- xml_contents(xml_find_all(xmlDoc, '//journal-title')))

# starting again: 
# extracting full files

# functioning with 
xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4547492.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4547424.xml")
xmlList <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]]
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/3929047.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4879183.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/5341264.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/5588100.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4547424.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/3659440.xml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/5110632.xml")

# xmlListA <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC3778923.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC3898277.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC4359276.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC4867786.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5232417.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5399602.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5472180.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5788455.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5805839.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5963118.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5963119.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5963120.nxml")
# xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/PMC5794850.nxml") 




# it may end up being faster to unlist and run each of the text recognition bits initially before collecting this inforamtion? Although it might be better to have blank


# Extracting PMCID 
# manual DL version - PMCID <- xmlList[["front"]][["article-meta"]][["article-id"]][["text"]]
PMCID <- xmlList[["front"]][["article-meta"]][["article-id"]][["text"]]

# Extracting DOI
# <article-id pub-id-type="doi">10.1016/j.jbtep.2015.05.001</article-id>
DOI <- unlist(xmlList[["front"]][["article-meta"]])

# extracting the element of the dataframe which is 1 below the "article attribute" called "doi"
DOI <- DOI[which(names(DOI) == "article-id..attrs.pub-id-type")[DOI[which(names(DOI) == "article-id..attrs.pub-id-type")] == "doi"]-1]

# Abbreviated journal ID 
# e.g., "J Behav Ther Exp Psychiatry"
journalId <- xmlList[["front"]][["journal-meta"]][["journal-id"]][["text"]]

## Extracting full journal title
journal <- xmlList[["front"]][["journal-meta"]][["journal-title-group"]][["journal-title"]]

# issue volume
issue <- xmlList[["front"]][["article-meta"]][["issue"]]
volume <- xmlList[["front"]][["article-meta"]][["volume"]]

# pub date ---- FIGURE OUT HOW TO EXTRACT THE FIRST OF THESE THAT IS PROVIDED ! ! !
pubDate <- xmlList[["front"]][["article-meta"]][["pub-date"]]

# Article title
title<- xmlList[["front"]][["article-meta"]][["title-group"]][["article-title"]]

# Article key words
keywords <- xmlList[["front"]][["article-meta"]][["kwd-group"]]
# removing keywords which are "keywords"
if(sum(names(keywords) == "title")>0) keywords <- keywords[-which(names(keywords) == "title")]

# extracting author information 
# counting number of authors 
# nAuthors<-length(xmlList[["front"]][["article-meta"]][["contrib-group"]])
start<- Sys.time()

# extracting author names
# first extracting the entier
contribGroup <-  unlist(xmlList[["front"]][["article-meta"]][["contrib-group"]], use.names = T)
firstNames <- contribGroup[names(contribGroup) == "contrib.name.given-names"]
lastNames <- contribGroup[names(contribGroup) == "contrib.name.surname"]
contribGroup <- NULL

authors <- data.frame(firstNames, lastNames)

# Extracting text 
# probably don't need head ~ head <- xmlList[["front"]]
body <- xmlList[["body"]]

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
discussionNames<-("discussion|conclusion|conclud|summary")


# gathering locations of 'ps' i.e., paragraphs 
lPs<-which(names(body)=="p")

# anything that is not under a section will be put in unlab (i.e., unlabled) 
unlabSection <- body[lPs]

##### The unlab section HAS NOT BEEN TESTED - no articles have had additional sections that should not be includd in the other bits
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

  # clearing from memory ## uncomment this later
# body <- NULL

# saving the text of each section

abstractText <- paste(unlist(xmlList[["front"]][["article-meta"]][["abstract"]]), collapse=' ')
unlabText <- paste(unlist(unlabSection), collapse =' ')
introText <- paste(unlist(introSection), collapse=' ')
methodsText <-paste(unlist(methodsSection), collapse=' ')
resultsText <- paste(unlist(resultsSection), collapse=' ')
discussionText <-  paste(unlist(discussionSection), collapse=' ')

# specifying patterns to match - includes either "-" or "\u2212" which are alternative minus signs, captures whitespace which is removed later
#         begins with((white space or '('))(effect size letter [and possible 'p' and or '2' for eta, possible '(\d\d)' for r, all aspects optional)(whitespace? '=' whitespace? ) before ('-' or 'Minus sign unicode') whitespace? digit(indeterminate length or 0 length) optional decimal, digit(s) indeterminate length required)

## at this point partial eta squared and eta are collected together, may as well seperate those. 
# Same with d and g. 
# Worth collecting R^2 ?? 
# Collect others ~ e.g., Epsilon squared and Omega squared ( which are ~ equivilent to eta), maybe with partial version of each in there. 


patternD <- "(?<=((\\s|\\()[dg]\\s?\\s?(//(95% confidence interval//))?(//(95% CI//))?)\\s?\\s?[\\.\\,\\:\\;]?\\s?\\s?([=]|(of))\\s?\\s?)(\\-?\u2212?\\s?\\d*\\.?\\d{1,})"
patternR <-  "(((?<=((\\s|\\(|\\[)([r]s?\\(?\\d{0,10}\\)?\\s?\\s?[=]\\s?\\s?)))|((?<=(correlation)\\s?\\s?(coefficient)?\\s?\\s?([=]|(of))\\s?\\s?)))(\u2212?\\-?\\s?\\d*\\.?\\d{1,}))"
patternEta <- "(((?<=((\\s|\\())([\u03B7]\\s?p?\\s?2?\\s?\\s?([=]|(of))\\s?\\s?))|((?<=((partial)?\\s?eta\\ssquared\\s?\\=?(of)?\\s?))))(\\-?\u2212?\\s?\\d*\\.?\\d{1,}))" 
patternP <- "(?<=(p\\s{0,4}))([=\\<\\>]\\s?\\s?\\-?\u2212?\\s?\\d*\\.?\\d{1,})" 
patternHR <- "((?<=((\\s|\\()((HR)|(hazzard.ratio))\\s{0,4}((of)|(=))\\s{0,4}))(\\-?\u2212?\\s?\\d*\\.?\\d{1,}))"
patternOR <- "((?<=((\\s|\\()((OR)|(odd..ratio))\\s{0,4}((of)|(=))\\s{0,4}))(\\-?\u2212?\\s?\\d*\\.?\\d{1,}))"
patternF <- ## Maybe vectorise to produce 3 cols 


# extract F statistics too. Convert using df1 <- df1; df2 <- df2; FS <- FS; estimatedEtaP <- (df1 * FS)/ (df1 * FS + df2); eta_sq(fir, partial = T)); 
# when extracting F statistics, retain all of the dfs too. Probably do this in two sections, i.e., extract F/(df1,df2) = x.xxx or whatever, and the process it further



# it may be possible to add in other text versions of this (e.g., "correlation coefficient of")

# function to extract text, remove whitespaces and unicode encodings of the minus sign and return numeric vector of extracted effect sizes
extractEffects <- function(inputText, pattern, ...) {
  # removing newline breaks, non-breaking spaces, '&#x000a0;', &#x00026;
  inputText <- str_remove_all(inputText, "\\n")
  inputText <- str_remove_all(inputText, "[Ââˆ\\’Ï„œ€$!\\“\u009d]")
  # extracting all text which matches pattern
  extracted <- str_extract_all(inputText,  regex(pattern, ignore_case = TRUE), simplify = T)
  # removing whitespace that can be captured in the regular expressions above
  extracted <- str_remove_all(extracted, "\\s") 
  # replacing unicode minus sign with R recognised hyphen/minus sign
  extracted <- str_replace_all(extracted, "\u2212", "-")
  return(as.numeric(extracted))
}


# Extracting from sections which were not covered above (later just lapply this across all of the sections, 
# probably also possible to change the above function to test for multiple effects, best to pilot it with the manual V first)
# LATER JUST DO THIS FOR EACH OF THE EFFECT SIZES: 
# texts <- list(unlabText, abstractText, introText, methodsText,  resultsText, discussionText)
# lapply(texts, extractEffects, patternD) # .. etc. (again, probably better to build extraction for multiple effects into the one function, 
# otherwise we're repeating opperations a lot for no reason)

extractedDsUnlab <- extractEffects(unlabText, patternD)
extractedRsUnlab <- extractEffects(unlabText, patternR)
extractedEtasUnlab <- extractEffects(unlabText, patternEta)
extractedHRsUnlab <-  extractEffects(unlabText, patternHR)
extractedORsUnlab <-  extractEffects(unlabText, patternOR)

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

end <- Sys.time()
# later to update it will be possible to just compare the master list with those 

# record<-get_records(ids = "oai:pubmedcentral.nih.gov:4547492", prefix = "pmc", url =  'https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi')

# record <- xmlToList(record[[1]])

