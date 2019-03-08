# Function to extract details from pub med centrak open access subset
# library(xml2)
# library(magrittr)
# library(tidyverse)
# library(lubridate)
# library(statcheck)

# Concatinate function that is just a better paste
concat <- function(text) {
  if (length(text) > 1) {
    return(str_flatten(text))
  } else {
    return(text)
  }
}

# This function is adapted from statcheck https://github.com/MicheleNuijten/statcheck/blob/master/R/htmlImport.R,
# does some final extra cleaning if any tags / weird characters remain
processHTML <- function(strings){
  # Remove subscripts (except for p_rep)
  strings <- lapply(strings, gsub, pattern = "<sub>(?!rep).*?</sub>", replacement = "", perl = TRUE)

  # Remove HTML tags:
  strings <- lapply(strings, gsub, pattern = "<(.|\n)*?>", replacement = "")

  # Replace html codes:
  strings <- lapply(strings, gsub, pattern = "&#60;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&lt;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#61;", replacement = "=", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#62;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&gt;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#40;", replacement = "(", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#41;", replacement = ")", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&thinsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&nbsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "\n", replacement = "")
  strings <- lapply(strings, gsub, pattern = "\r", replacement = "")
  strings <- lapply(strings, gsub, pattern = "\\s+", replacement = " ")
  strings <- lapply(strings, gsub, pattern = "&minus;", replacement = "-", fixed = TRUE)

  # removing newline breaks, non-breaking spaces, '&#x000a0;', &#x00026;
  strings <- lapply(strings, gsub, pattern = "[Ââˆ\\’Ï„œ€$!\\“\u009d]", replacement = "")
  # replacing unicode minus sign with R recognised minus sign
  strings <- lapply(strings, str_replace_all, pattern = "\\u2212", replacement = "-")
  # replcaing unicode short spaces that are not always picked up above
  strings <- lapply(strings, str_replace_all, pattern = "\\u2009", replacement = " ")
  return(strings)
}

# search strings for each of the sections of the paper - add here for more
introNames <- ("Introduction|Background")
methodsNames <- ("method|aims|measur")
resultsNames <- ("result")
discussionNames <- ("discussion|conclusion|conclud|summary")

# example with coheon's d's: "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4879183&metadataPrefix=pmc",
# correlations: https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5588100&metadataPrefix=pmc,
# F statistics: https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3659440&metadataPrefix=pmc
# https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3172423&metadataPrefix=pmc
# "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3659440&metadataPrefix=pmc"

# call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3659440&metadataPrefix=pmc" articles$oaiCall[ trainingSet ][11]

 # pullAndProcess(call)
# example with F and t stats : articles$oaiCall[7023]

pullPMC <- function(call) {
paper <- read_html(call) #"https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3659440&metadataPrefix=pmc")

## Metadata extraction
PMCID  <-
  paste0("PMC",
         str_extract(call, "(?<=oai:pubmedcentral.nih.gov:)[0-9]*"))
pmcIDCheck <-
  xml_text(xml_find_first(paper, '//article-id[@pub-id-type="pmcid"]'))

# Checking that these match, otherwise we've a problem
if (PMCID != pmcIDCheck) {
  stop("PMCID of database does not match that extracted from call")
}

# DOI
doi <-
  xml_text(xml_find_first(paper, '//article-id[@pub-id-type="doi"]'))
# Journal name
journalID <- xml_text(xml_find_first(paper, '//journal-title'))
# Jounral name abbreviation
journalIDAbrev <- xml_text(xml_find_first(paper, '//journal-id'))
# Article issue  # Check
issue <-
  xml_text(xml_find_first(paper, '//front/article-meta/issue'))
# Article volume
volume <-
  xml_text(xml_find_first(paper, "//front/article-meta/volume"))
# date print pub
pPub <-
  if (!is.na(xml_find_first(paper, '//pub-date[@pub-type="ppub"]'))) {
    parse_date_time(str_remove_all(paste(
      xml_text(
        xml_find_first(paper, '//pub-date[@pub-type="ppub"]/year')
      ), " ",
      xml_text(
        xml_find_first(paper, '//pub-date[@pub-type="ppub"]/month')
      ), " ",
      xml_text(xml_find_first(
        paper, '//pub-date[@pub-type="ppub"]/day'
      ))
    ), "NA"), orders = c("ymd", "ym", "y"), exact = )
  } else {
    NA
  }

# date epub
ePub <-
  if (!is.na(xml_find_first(paper, '//pub-date[@pub-type="epub"]'))) {
    parse_date_time(str_remove_all(paste(
      xml_text(
        xml_find_first(paper, '//pub-date[@pub-type="epub"]/year')
      ), " ",
      xml_text(
        xml_find_first(paper, '//pub-date[@pub-type="epub"]/month')
      ), " ",
      xml_text(xml_find_first(
        paper, '//pub-date[@pub-type="epub"]/day'
      ))
    ), "NA"), orders = c("ymd", "ym", "y"))
  } else {
    NA
  }

title <-
  xml_text(xml_find_first(paper, '//front/article-meta/title-group/article-title'))

# Article key words # possibly trim
keywords <-
  xml_text(xml_find_all(paper, '//front/article-meta/kwd-group/kwd'))

### Authors
# author names:
AuthorSurnames <-
  xml_text(xml_find_all(
    paper,
    '//contrib[@contrib-type="author"]/name/surname/text()'
  ))
AuthorFirstNames <-
  xml_text(
    xml_find_all(
      paper,
      '//contrib[@contrib-type="author"]/name/given-names/text()'
    )
  )

# Abstract
abstract_node <- xml_find_all(paper, "//abstract")
# add abstract text 
abstract <- concat(xml_text(abstract_node))# unlablled paragraphs
# unlablled sections 
unlabPs_nodes <- xml_find_all(paper, "//body/p")
# article sections
sections <-  xml_find_all(paper, "//article/sec")
# article section titles
titles <-  xml_text(xml_find_all(sections, "title"))


# These sections are not necessary now - unless the above bit does not work
# unlabPs_nodes <- xml_find_all(paper, "//p")

# #### Unlabbled here
# if (sum(!str_detect(titles, regex(
#   paste(introNames, methodsNames, resultsNames, discussionNames, sep = "|"),
#   ignore_case = T ))) > 0) {
#   index <-
#     which(!str_detect(titles, regex(
#       paste(
#         introNames,
#         methodsNames,
#         resultsNames,
#         discussionNames,
#         sep = "|"
#       ),
#       ignore_case = T
#     )))
#   unlabSection <- xml_text(sections[index])
# }

# # Adding in any untitled paragraphs here, only slightly testsed
# unlabText <- NA
# if (length(unlabPs_nodes) > 0) {
#   unlabText[[length(unlabSection) + 1]] <- xml_text(unlabPs_nodes)
#   unlabText <- paste(unlist(unlabSection), collapse = ' ')
# } else {unlabText <- unlabSection}

# seperating the sections by, if there are any sections titled matching the section heads
textOutput <- data.frame(titles, xml_text(sections), stringsAsFactors = F)

# Figure out authors information better here
 output <- list(
  metadata = data_frame(
    PMCID,
    doi,
    journalID,
    journalIDAbrev,
    title,
    issue,
    volume,
    pPub,
    ePub,
    abstract = ifelse(is_empty(abstract), NA, abstract),
    call
  ),
  keywords = data_frame(
    PMCID,
    keywords),

  authors = data_frame(
    PMCID,
    surname = AuthorSurnames,
    firstname = AuthorFirstNames),
  
  
  text =
    data_frame(names = processHTML(c("PMCID", "abstract", textOutput[, 1])),
               text = processHTML(c(PMCID, abstract, textOutput[, 2])))
 )
 return(output)
}


# extractTestStats(store$text)

#   # processing all but the PMID with extract test stats
#   statisticalOutput <- lapply(output$text[-1], extractTestStats, context = T)
#     # NA rows removed here using a filter:
#    notNAs <- !is.na( unlist(  lapply(X = statisticalOutput, FUN =  function(x) { slice(x, 1)[1] })))
#     if(any(notNAs)) {
#   output$statisticalOutput <- data.frame(PMCID = output$text[[1]], bind_rows(statisticalOutput[notNAs], .id = "section"))
#   } else {
#     output$statisticalOutput <- NA
#   }
#   
#   statCheckOutput <- lapply(output$text[-1], function(x) {
#     if(length(x) > 0){ if(is.na(x)) { return(NA)} else{statcheck(x)}} else NA
#   }
#     )
#   # NA rows removed here using a filter:
#   notNAs <- unlist(  lapply(X = statCheckOutput, FUN =  elementExists))
#   if(any(notNAs)) {
#     output$statCheckOutput <- data.frame(PMCID = output$text[[1]], bind_rows(statCheckOutput[notNAs], .id = "section"))
#   } else {
#     output$statCheckOutput <- NA
#   }
#   return(output)
#   # }
