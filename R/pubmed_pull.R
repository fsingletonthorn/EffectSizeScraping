# Function to extract details from pub med centrak open access subset
# library(xml2)
# library(magrittr)
# library(tidyverse)
# library(lubridate)
# library(statcheck)

# Concatinate function that is just a "better" paste0
concat <- function(text) {
  if (length(text) > 1) {
    return(stringr::str_flatten(text))
  } else {
    return(text)
  }
}

# concatinate plus function that is just a "better" paste
concatPlus <- function(text) {
  if (length(text) > 1) {
    return(stringr::str_flatten(text, collapse = " "))
  } else {
    return(text)
  }
}

# This function is adapted from statcheck https://github.com/MicheleNuijten/statcheck/blob/master/R/htmlImport.R,
# does some final extra cleaning if any tags / weird characters remain
processText <- function(strings){
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
  strings <- lapply(strings, gsub, pattern = "[Ââˆ\\’Ï„œ€$!\\“\u009d]", replacement = " ")
  # replacing unicode minus sign with R recognised minus sign
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\u2212", replacement = "-")
  # replcaing unicode short spaces that are not always picked up above
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\u2009", replacement = " ")
  # replcaing mathematical symbols with interpretable ones 
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d443", replacement = "p")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d45b", replacement = "n")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d6fd", replacement = "beta")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d6fc", replacement = "alpha")
  strings <- lapply(strings, stringr::str_replace_all, pattern = "\\U0001d712", replacement = "chi")
  
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

call <-"https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3172423&metadataPrefix=pmc"

# call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3659440&metadataPrefix=pmc"  # articles$oaiCall[ trainingSet ][11]

# example with F and t stats : articles$oaiCall[7023]

pullPMC <- function(call) {
paper <- xml2::read_html(call) #"https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3659440&metadataPrefix=pmc")

## Metadata extraction
PMCID  <-
  paste0("PMC",
         stringr::str_extract(call, "(?<=oai:pubmedcentral.nih.gov:)[0-9]*"))
pmcIDCheck <-
  xml2::xml_text(xml2::xml_find_first(paper, '//article-id[@pub-id-type="pmcid"]'))

# Checking that these match, otherwise we've a problem
if (PMCID != pmcIDCheck) {
  stop("PMCID of database does not match that extracted from call")
}

# DOI
doi <-
  xml2::xml_text(xml2::xml_find_first(paper, '//article-id[@pub-id-type="doi"]'))
# Journal name
journalID <- xml2::xml_text(xml2::xml_find_first(paper, '//journal-title'))
# Jounral name abbreviation
journalIDAbrev <- xml2::xml_text(xml2::xml_find_first(paper, '//journal-id'))
# Article issue  # Check
issue <-
  xml2::xml_text(xml2::xml_find_first(paper, '//front/article-meta/issue'))
# Article volume
volume <-
  xml2::xml_text(xml2::xml_find_first(paper, "//front/article-meta/volume"))
# date print pub
pPub <-
  if (!is.na(xml2::xml_find_first(paper, '//pub-date[@pub-type="ppub"]'))) {
    lubridate::parse_date_time(stringr::str_remove_all(paste(
      xml2::xml_text(
        xml2::xml_find_first(paper, '//pub-date[@pub-type="ppub"]/year')
      ), " ",
      xml2::xml_text(
        xml2::xml_find_first(paper, '//pub-date[@pub-type="ppub"]/month')
      ), " ",
      xml2::xml_text(xml2::xml_find_first(
        paper, '//pub-date[@pub-type="ppub"]/day'
      ))
    ), "NA"), orders = c("ymd", "ym", "y"), exact = )
  } else {
    NA
  }

# date epub
ePub <-
  if (!is.na(xml2::xml_find_first(paper, '//pub-date[@pub-type="epub"]'))) {
    lubridate::parse_date_time(stringr::str_remove_all(paste(
      xml2::xml_text(
        xml2::xml_find_first(paper, '//pub-date[@pub-type="epub"]/year')
      ), " ",
      xml2::xml_text(
        xml2::xml_find_first(paper, '//pub-date[@pub-type="epub"]/month')
      ), " ",
      xml2::xml_text(xml2::xml_find_first(
        paper, '//pub-date[@pub-type="epub"]/day'
      ))
    ), "NA"), orders = c("ymd", "ym", "y"))
  } else {
    NA
  }

title <-
  xml2::xml_text(xml2::xml_find_first(paper, '//front/article-meta/title-group/article-title'))

# Article key words # possibly trim
keywords <-
  xml2::xml_text(xml2::xml_find_all(paper, '//front/article-meta/kwd-group/kwd'))

### Authors
# author names:
AuthorSurnames <-
  xml2::xml_text(xml2::xml_find_all(
    paper,
    '//contrib[@contrib-type="author"]/name/surname/text()'
  ))
AuthorFirstNames <-
  xml2::xml_text(
    xml2::xml_find_all(
      paper,
      '//contrib[@contrib-type="author"]/name/given-names/text()'
    )
  )

# Abstract
abstract_node <- xml2::xml_find_all(paper, "//abstract")
# add abstract text 
abstract <- concatPlus(xml2::xml_text(abstract_node))
# unlablled sections 
unlabPs_nodes <- xml2::xml_find_all(paper, "//body/p")
# article sections
sections <-  xml2::xml_find_all(paper, "//article/sec")
# article section titles
titles <-  xml2::xml_text(xml2::xml_find_all(sections, "title"))

# seperating the sections by, if there are any sections titled matching the section heads
textOutput <- data.frame(titles, xml2::xml_text(sections), stringsAsFactors = F)

# Figure out authors information better here
 output <- list(
  metadata = tibble::tibble(
    PMCID,
    doi,
    journalID,
    journalIDAbrev,
    title,
    issue,
    volume,
    pPub,
    ePub,
    abstract = ifelse(purrr::is_empty(abstract), NA, abstract),
    call
  ),
  keywords = tibble::tibble(
    PMCID,
    keywords),

  authors = tibble::tibble(
    PMCID,
    surname = AuthorSurnames,
    firstname = AuthorFirstNames),
  
  
  text =
    tibble::tibble(names = processText(c("PMCID", "abstract", textOutput[, 1])),
               text = processText(c(PMCID, ifelse(purrr::is_empty(abstract), NA, abstract), textOutput[, 2])))
 )
 return(output)
}

processPMC <- function(paper_text_list) {
  # processing all but the PMID with extract test stats
  output <- as.list(paper_text_list)
  
  statisticalOutput <-
    apply(data.frame(unlist(output[1]), unlist(output[2]), stringsAsFactors = F), 1,
          function(x) {
            extractTestStats(x[2], sectionName = x[1], context = T)
          })
  
  # NA rows removed here using a filter:
  notNAs <- unlist(lapply(statisticalOutput, function(x) !is.na(x[[2]][1])))

    if (any(notNAs)) {
    output$statisticalOutput <-
      data.frame(PMCID = output$text[[1]],
                 dplyr::bind_rows(statisticalOutput[notNAs]), 
                 stringsAsFactors = F)
  } else {
    output$statisticalOutput <- NA
  }
  
  statCheckOutput <- lapply(output$text[-1], function(x) {
    if (length(x) > 0) {
      if (is.na(x)) {
        return(NA)
      } else{
        statcheck::statcheck(x)
      }
    } else
      NA
  })
  
  # NA rows removed here using a filter:
   notNAs <-
     unlist(lapply(X = statCheckOutput, FUN =  elementExists))
   if (any(notNAs)) {
     output$statCheckOutput <-
       data.frame(PMCID = output$text[[1]], dplyr::bind_rows(statCheckOutput[notNAs], .id = "section"))
   } else {
     output$statCheckOutput <- NA
   }
  return(output)
}

