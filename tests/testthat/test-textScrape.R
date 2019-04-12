context("Scraping functions")

test_that("extractPdf extracts all pages", {
pdf <- extractPdf("https://osf.io/nztsx/download")
pdf2 <- extractPdf("https://osf.io/nztsx/download")

 expect_true(str_count( pdf[[1]], "or") == 29)
 expect_true(length(pdf) == 10)
})

## Need to test PMC version too