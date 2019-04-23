context("test pubmed pull and process")

test_that("extraction works from pdf", {
  pdf <- extractPdf("https://osf.io/v7n6j/download")
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



test_that("scrapePMC extracts files with correctly labeled sections", {
  pmcID <- 5393010
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5393010&metadataPrefix=pmc"
  ftpCall  <-"ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/a2/4e/PMC5393010.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)
  
  expect_true(
    stringr::str_detect(output$text$text[output$text$names == "unlabelled"],
                        "Prompt diagnosis is important as urgent")
  )
  expect_true( is.na(output$text$statisticalOutput) )
})






