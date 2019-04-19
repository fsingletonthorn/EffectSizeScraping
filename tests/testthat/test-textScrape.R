context("Scraping functions")

test_that("extractPdf extracts all pages", {
  pdf <- extractPdf("https://osf.io/nztsx/download")
  # pdf2 <- extractPdf("https://osf.io/kcbnu/download")
  # expect_true(sum(lapply(pdf$text , stringr::str_count, pattern = "or")))
  expect_true(
    stringr::str_detect(
      pdf[[6, 2]],
      stringr::fixed(
        "participants may have given inaccurate or exaggerated responses due to poor awareness and/or social desirability bias"
      )
    ) & stringr::str_detect(
        pdf[[6, 2]],
        stringr::fixed("In this experimental study, we manipulated")
      )
  )
})