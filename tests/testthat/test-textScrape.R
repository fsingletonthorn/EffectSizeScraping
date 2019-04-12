context("Scraping functions")

test_that("extractPdf extracts all pages", {
pdf <- extractPdf("https://osf.io/nztsx/download")
pdf2 <- extractPdf("https://osf.io/nztsx/download")

 expect_true(stringr::str_count( pdf[[1]], "or") == 29)
 expect_true(length(pdf) == 10)
})

## Need to test PMC version too
test_that("pullPMC extracts correct text", {
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5588100&metadataPrefix=pmc"
  output <- pullPMC(call)
expect_true(output$text$text[1] == "PMC5588100")
expect_true(stringr::str_detect(output$text$text[2], stringr::fixed("which were significantly higher than those in cancer-adjacent normal tissues of the control group (p0.05).")))
            }
)

test_that("pullPMC extracts correct text", {
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5588100&metadataPrefix=pmc"
  output <- pullPMC(call)
  expect_true(output$text$text[1] == "PMC5588100")
  expect_true(stringr::str_detect(output$text$text[2], stringr::fixed("which were significantly higher than those in cancer-adjacent normal tissues of the control group (p0.05).")))
  }
)


