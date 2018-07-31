install.packages('XML')
install.packages("oai")
library(XML)
library("oai")

# remeber for rolling assignment of names: 
for(i in 1:2) {
assign(paste0("abc",i,"xyz"), i)
}
abc1xyz
abc2xyz




# starting again: 
# extracting full files
xmlList <- xmlToList("EarlyTestMaterials/ExamplePapers/4547492.xml")

# Extracting xml full ID 
# e.g., "oai:pubmedcentral.nih.gov:4547492"
ID <- xmlList[["GetRecord"]][["record"]][["header"]][["identifier"]]

# Extracting PMCID 
PMCID <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["article-id"]][["text"]]

# Extracting DOI
# DOI 
# <article-id pub-id-type="doi">10.1016/j.jbtep.2015.05.001</article-id>
DOI <- unlist(xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][[6]][1])


# Abbreviated journal ID 
# e.g., "J Behav Ther Exp Psychiatry"
journalId <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["journal-meta"]][["journal-id"]][["text"]]

## Extracting full journal title
journal <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["journal-meta"]][["journal-title-group"]][["journal-title"]]

# Article title
title<-xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["title-group"]][["article-title"]]

# extracting author information 
# counting number of authors 
nAuthors<-length(xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["contrib-group"]])

# extracting first and last names of each author 

# first extracting the entier
articleContribGroup <-  unlist(xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]][["article-meta"]][["contrib-group"]], use.names = T)
articleFirstNames <- articleContribGroup[names(articleContribGroup) == "contrib.name.given-names"]
aritlceAuthorsLastNames <- articleContribGroup[names(articleContribGroup) == "contrib.name.surname"]
articleContribGroup <- NULL

# NEXT NEED TO EXTRACT BODY TEXT HERE
articleHead <- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["front"]]
articleBody<- xmlList[["GetRecord"]][["record"]][["metadata"]][["article"]][["body"]]






  
  
  

# later to update it will be possible to just compare the master list with those 

record<-get_records(ids = "oai:pubmedcentral.nih.gov:4547492", prefix = "pmc", url =  'https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi')

record <- xmlToList(record[[1]])
