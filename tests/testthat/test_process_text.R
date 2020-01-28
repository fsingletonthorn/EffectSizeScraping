context("Test text processing")

call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5588100&metadataPrefix=pmc"
output <- pullPMC(call)

test_that("processPMC extracts correct values", {
  statisticalOutput <- processText(output$text)$statistics
  expect_equal(statisticalOutput$PMCID[1], "PMC5588100")
  expect_equal(statisticalOutput$statistic[1], "r")
  expect_equal(statisticalOutput$reported[1], "r=0.676, p<0.05")
  }
)

test_that("extraction works from pdf", {
  pdf <- extractPdf("https://osf.io/v7n6j/download")
  pdf$PMCID <- "test"
  extracted <- processText(pdf)$statistics
  expect_identical(extracted$value[extracted$statistic == "OR"],
                   c(71.79,  3.76))
})
