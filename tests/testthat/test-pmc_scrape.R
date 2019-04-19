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


test_that("extractPdf extracts files with correctly labeled sections", {
  pdf <- extractPdf("https://osf.io/dzm7v/download")
  
})