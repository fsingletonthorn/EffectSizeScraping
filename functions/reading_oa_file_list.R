library(tidyverse)

# This is the list of articles avaliable as of download time (2019.01.03)
oaFileList <- read_csv(file = "data/oa_file_list.csv")

# Extracting journals
articles <- str_split(oaFileList$`Article Citation`, "\\.", simplify = T)
oaFileList$Journal <- articles[,1]
# uniq.js <- unique(articles[,1])
rm("articles")

oaJList <- read_csv("data/jlist.csv")

# Checking that these are not psychology journals which have different names
# View(oaJList$`NLM TA`[!oaJList$`NLM TA` %in% uniq.js])

# These are the journals that were added manually # 9th Feb 2019 
# Behavioural Sciences Journals 
BSJs <- c(oaJList$`NLM TA`[as.logical(oaJList$Eligible)],
          "Psychiatry (Edgmont)","Psychopathology", "J Stud Alcohol Drugs Suppl",
          "J Stud Alcohol Drugs", "J Neurol Psychiatry", "J Neurol Psychopathol",
          "J Can Acad Child Adolesc Psychiatr", "Funct Neurol", "Behav Anal Pract",
          "Behav Anal", "Adv Physiol Educ")

# Identifying artilces which are of interest here - Includes ~300000 articles with PLOS articles, 
# 85285 without. For the moment we've removed the PLOS articles, could also remove the 
# Royal soc open science, 
BSJs <- BSJs[BSJs != "PLoS One"]
oaFileList$applicable <- oaFileList$Journal %in% BSJs

oaFileList$PMCID <- str_remove(oaFileList$`Accession ID`, "PMC")

articles <- filter(oaFileList, oaFileList$applicable)
table(articles$Journal)

articles$oaiCall <- paste0("https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:", articles$PMCID,  "&metadataPrefix=pmc")
          
# Cleaning things up because this file is sourced in the main script
rm("oaFileList", "oaJList")
