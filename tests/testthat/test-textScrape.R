context("Scraping functions")
suppressWarnings( library(stringr))
suppressWarnings(library(stringi))
suppressWarnings(library(pdftools))

test_that("extractPdf extracts correctly", {
pdf <- extractPdf("https://osf.io/nztsx/download")
pdf2 <- extractPdf("https://osf.io/nztsx/download")

 expect_true(str_count( pdf[[1]], "or") == 29)
 expect_true(length(pdf) == 10)
})