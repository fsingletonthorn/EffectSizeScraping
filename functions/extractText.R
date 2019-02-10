# Function to extract details from pub med centrak open access subset
library(xml2)
library(magrittr)
library(tidyverse)
library(lubridate)

# Concatinate function that is just a better paste
concat <- function(text) {
  if (length(text) > 1) {
    return(str_flatten(text))
  } else {
    return(text)
  }
}





# search strings for each of the sections of the paper - add here for more
introNames <- ("Introduction|Background")
methodsNames <- ("method|aims|measur")
resultsNames <- ("result")
discussionNames <- ("discussion|conclusion|conclud|summary")


# example with coheon's d's: "oai:pubmedcentral.nih.gov:4879183", correlations: oai:pubmedcentral.nih.gov:5588100, F statistics: oai:pubmedcentral.nih.gov:3659440

call = articles$oaiCall[7000]

paper <- read_html(call)

## Metadata extraction
PMCID  <-
  paste0("PMC",
         str_extract(call, "(?<=oai:pubmedcentral.nih.gov:)[0-9]{7}"))
pmcIDCheck <-
  xml_text(xml_find_first(paper, '//article-id[@pub-id-type="pmcid"]'))

# Checking that these match, otherwise we've a problem
if (PMCID != pmcIDCheck) {
  return(NA)
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
      ),
      xml_text(
        xml_find_first(paper, '//pub-date[@pub-type="ppub"]/month')
      ),
      xml_text(xml_find_first(
        paper, '//pub-date[@pub-type="ppub"]/day'
      ))
    ), "NA| "), orders = c("ymd", "y"), exact = )
  } else {
    NA
  }

# date epub
ePub <-
  if (!is.na(xml_find_first(paper, '//pub-date[@pub-type="epub"]'))) {
    parse_date_time(str_remove_all(paste(
      xml_text(
        xml_find_first(paper, '//pub-date[@pub-type="epub"]/year')
      ),
      xml_text(
        xml_find_first(paper, '//pub-date[@pub-type="epub"]/month')
      ),
      xml_text(xml_find_first(
        paper, '//pub-date[@pub-type="epub"]/day'
      ))
    ), "NA| "), orders = c("ymd", "y"), exact = )
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
# unlablled paragraphs
unlabPs_nodes <- xml_find_all(paper, "//body/p")
# article sections
sections <-  xml_find_all(paper, "//article/sec")
# article section titles
titles <-  xml_text(xml_find_all(sections, "title"))

# add abstract
abstractText <- xml_text(abstract_node)

##### The unlab section HAS NOT BEEN TESTED !!! - no articles have had additional sections that should not be includd in the other bits
if (sum(!str_detect(titles, regex(
  paste(introNames, methodsNames, resultsNames, discussionNames, sep = "|"),
  ignore_case = T
))) > 0) {
  index <-
    which(!str_detect(titles, regex(
      paste(
        introNames,
        methodsNames,
        resultsNames,
        discussionNames,
        sep = "|"
      ),
      ignore_case = T
    )))
  unlabSection <- xml_text(sections[index])
}

# Adding in any untitled paragraphs here, only slightly testsed
unlabText <- NA
if (length(unlabPs_nodes) > 0) {
  unlabText[[length(unlabSection) + 1]] <- xml_text(unlabPs_nodes)
  unlabText <- paste(unlist(unlabSection), collapse = ' ')
} else {unlabText <- unlabSection}

# seperating the sections by, if there are any sections titled matching the section heads
# looping through each of the sections that match the names and putting them in a list -
introText <- NA
if (sum(str_detect(titles, (regex(
  introNames, ignore_case = T
)))) > 0) {
  index <-
    which(str_detect(titles, regex(introNames, ignore_case = T)))
  introText <- xml_text(sections[index])
}

methodsText <- NA
if (sum(str_detect(titles, (regex(
  methodsNames, ignore_case = T
)))) > 0) {
  index <-
    which(str_detect(titles, regex(methodsNames, ignore_case = T)))
  methodsText <- xml_text(sections[index])
}

resultsText <- NA
if (sum(str_detect(titles, (regex(
  resultsNames, ignore_case = T
)))) > 0) {
  index <-
    which(str_detect(titles, regex(resultsNames, ignore_case = T)))
  resultsText <- xml_text(sections[index])
}

discussionText <- NA
if (sum(str_detect(titles, (regex(
  discussionNames, ignore_case = T
)))) > 0) {
  index <-
    which(str_detect(titles, regex(discussionNames, ignore_case = T)))
  discussionText <- xml_text(sections[index])
}

# Figure out authors information better here
list(AuthorSurnames,
     AuthorFirstNames)


#
list(
  paperInfo = data_frame(
    PMCID,
    doi,
    journalID,
    journalIDAbrev,
    title,
    issue,
    volume,
    pPub,
    ePub,
    call
  ),
  keywords = data_frame(PMCID, keywords),
  authors = data_frame(PMCID,
                       AuthorSurnames,
                       AuthorFirstNames),
  text = data_frame(
    PMCID,
    abstract = concat(abstractText),
    intro = concat(introText),
    methods = concat(methodsText),
    discussion = concat(discussionText),
    results = concat(resultsText),
    unlabelled= concat(unlabText)
  )
)
