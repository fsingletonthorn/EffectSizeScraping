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
  pmcID <- 4710337
  call <- articles$oaiCall[articles$PMCID == pmcID]
  ftpCall  <- articles$tpfCall[articles$PMCID == pmcID]
  output <- scrapePMC(call, ftpCall, statcheck = F)
  
  expect_true(
  str_detect(output$text$text[output$text$names == "unlabelled"],
             "Prompt diagnosis is important as urgent")
  )
  expect_true( is.na(output$text$statisticalOutput) )
})


