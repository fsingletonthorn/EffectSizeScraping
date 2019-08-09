context("test pubmed pull and process")

test_that("extraction works from pdf", {
  pdf <- extractPdf("https://osf.io/v7n6j/download")
  pdf$PMCID <- "test"
  extracted <- processPMC(pdf)
  expect_identical(extracted$statisticalOutput[[7]][extracted$statisticalOutput$statistic == "chi"],
                   c("4.541",
                     "3.421",
                     "2.202",
                     "11.566",
                     "19.236"))
})

test_that("scrapePMC extracts files with correctly labeled sections", {
  # pmcID <- 4710337
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4710337&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/bd/41/PMC4710337.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)
  
  expect_true(
  stringr::str_detect(output$text$text[output$text$names == "unlabelled"],
             "Prompt diagnosis is important as urgent")
  )
  expect_true( is.na(output$text$statisticalOutput) )
})



test_that("scrapePMC doesn't extract from PDFs when it is not necessary", {
  pmcID <- 5393010
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5393010&metadataPrefix=pmc"
  ftpCall  <-"ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/a2/4e/PMC5393010.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)
  
  expect_true(
    stringr::str_detect(output$text$text[output$text$names == "Discussion"],
                        "BEI-PSY is the first study to investigate the prevalence")
  )
  
  expect_true(all(
    output$text$statisticalOutput$statistic == c("chi", "chi", "chi")
  )) 
})


test_that("scrapePMC process excracts stats correctly from pmc5504157", {
  pmcID <- 5504157
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5504157&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/4a/be/PMC5504157.tar.gz"

 output <- scrapePMC(call, ftpCall, statcheck = F)
 
 
 expect_true(all(as.numeric(output$text$statisticalOutput$value[
   output$text$statisticalOutput$statistic == "r"
   ]) %in%
 c(0.10,0.30,0.71,0.69,0.63,0.61,0.59,0.56,0.86,0.32,0.75,0.67,0.51,
 0.45,0.39,0.41,0.30,0.31,0.31,0.59,0.56,0.48,0.52,0.38,0.63,0.41)))
 
 expect_true(all(
   as.numeric(output$text$statisticalOutput$value[output$text$statisticalOutput$statistic == "d"]) %in%
     c(0.07,
       0.44,
       0.20)
 ))
 
 # concatinatedText <- concatPlus(output$text$text)
 expect_true(
 stringr::str_detect(output$text$text[output$text$names == "Results"],
                     "ranged from")
)
 expect_true(
   stringr::str_detect(output$text$text[output$text$names == "Results"],
                       "in the cross-sectional design\\.\\s?$")
 )
})


test_that("scrapePMC extracts the pdf when text is not otherwise avaliable", {
  # pmcID <- 4710337
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5393010&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/a2/4e/PMC5393010.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)

  expect_true(
    stringr::str_detect(output$text$text[output$text$names == "Discussion"],
                        "The findings of this study must be")
  )
})

test_that("scrapePMC extracts the pdf when text is not otherwise avaliable", {
  # pmcID <- 4710337
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5393010&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/a2/4e/PMC5393010.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)

  expect_true(
    stringr::str_detect(output$text$text[output$text$names == "Discussion"],
                        "The findings of this study must be")
  )
})

test_that("scrapePMC extracts the pdf when text is not otherwise avaliable", {
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4311551&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/26/85/PMC4311551.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)

  expect_true(
    stringr::str_detect(output$text$text[output$text$names == "unlabelled"],
                        "The increased interest in the sense of touch over the last decades will hopefully continue as much still remains to be explored\\.$")
  )
})

