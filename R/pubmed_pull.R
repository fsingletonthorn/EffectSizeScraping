# Function to extract details from pub med central open access subset

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
# example from frontiers where the editor and reviewers are identified 
#call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:6379263&metadataPrefix=pmc"
#call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4648576&metadataPrefix=pmc"

# call <-"https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3172423&metadataPrefix=pmc"

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

# Article key words, all new lines replaced
keywords <-
  stringr::str_replace_all(xml2::xml_text(xml2::xml_find_all(paper, '//front/article-meta/kwd-group/kwd')), "\\n", "")

### Authors
# author names:
AuthorSurnames <-
  xml2::xml_text(
    xml2::xml_find_all(
      paper,
      '//contrib[@contrib-type="author"]/name/surname/text()'
    )
  )

AuthorFirstNames <-
  xml2::xml_text(
    xml2::xml_find_all(
      paper,
      '//contrib[@contrib-type="author"]/name/given-names/text()'
    )
  )

### Edited by 
edited_by_in_text <-
  xml2::xml_text(
    xml2::xml_find_all(
      paper,
      '//author-notes/fn[@fn-type="edited-by"]'
    )
  )

edited_by_contrib_node_surname <-
  xml2::xml_text(
    xml2::xml_find_all(
      paper,
      '//contrib[@contrib-type="editor"]//surname'
    )
  )

edited_by_contrib_node_given <-
  xml2::xml_text(
    xml2::xml_find_all(
      paper,
      '//contrib[@contrib-type="editor"]//given-names'
    )
  )

# Abstract
abstract_node <- xml2::xml_find_all(paper, "//abstract")
# add abstract text 
abstract <- concatPlus(xml2::xml_text(abstract_node))
# unlablled sections 
unlabPs <-  concatPlus( xml2::xml_text( xml2::xml_find_all(paper, "//body/p")))
# article sections
sections <-  xml2::xml_find_all(paper, "//article/sec")
# article section titles
titles <-  xml2::xml_text(xml2::xml_find_all(sections, "title"))
# Getting all paragraphs just in case the rest of this has failed
if (!elementExists(concatPlus(c(
  xml2::xml_text(sections),  unlabPs
)))
) {unlabPs <- concatPlus( xml2::xml_text( xml2::xml_find_all(paper, "//p") ) ) }

# seperating the sections by, if there are any sections titled matching the section heads
textOutput <- data.frame(titles = titles, text = xml2::xml_text(sections), stringsAsFactors = F)
if( elementExists(unlabPs) ) {
  textOutput <- dplyr::bind_rows(textOutput, text = tibble::tibble(titles = "unlabelled", text = unlabPs ))
}

# structuring author / article data here 
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
 #   abstract = ifelse(purrr::is_empty(abstract), NA, abstract),
    call
  ),
  keywords = tibble::tibble(
    PMCID,
    keywords),

  authors = tibble::tibble(
    PMCID,
    surname = AuthorSurnames,
    firstname = AuthorFirstNames),
  
 editors = tibble::tibble(
   PMCID,
   surname = edited_by_contrib_node_surname,
   firstname = edited_by_contrib_node_given
   ),
 
 editors_unstructured = tibble::tibble(
   PMCID,
  unstructured_editors = edited_by_in_text
   ),
 
  text =
    tibble::tibble(PMCID = PMCID, 
      names = unlist(cleanText(c("abstract", textOutput[, 1]))),
               text = c(ifelse(purrr::is_empty(abstract), NA, unlist(cleanText(abstract))), unlist(cleanText(textOutput[, 2]))))
 )
 return(output)
}
