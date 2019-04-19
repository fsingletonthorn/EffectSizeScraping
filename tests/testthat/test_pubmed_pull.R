context("Scraping functions")


call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5588100&metadataPrefix=pmc"
output <- pullPMC(call)

test_that("pullPMC extracts correct text", {
expect_true(output$text$text[1] == "PMC5588100")
expect_true(stringr::str_detect(output$text$text[2], stringr::fixed("which were significantly higher than those in cancer-adjacent normal tissues of the control group (p0.05).")))
            }
)

test_that("pullPMC extracts correct text", {
  expect_true(output$text$text[1] == "PMC5588100")
  expect_true(stringr::str_detect(output$text$text[2], stringr::fixed("which were significantly higher than those in cancer-adjacent normal tissues of the control group (p0.05).")))
  expect_true(stringr::str_detect(output$text$text[6], stringr::fixed("ancer tissues of the patient group had no correlation with the existences of lump and localized density-increased shadow (p>0.05), but were associated with manifestations of architectural distortion, calcification as well as skin and nipple depression (p<0.05).")))
  }
)

test_that("processPMC extracts correct values", {
  processedText <- processPMC(output$text)
  expect_equal(processedText$statisticalOutput$PMCID[1], "PMC5588100")
  expect_equal(processedText$statisticalOutput$names[1], "Discussion")
  expect_equal(processedText$statisticalOutput$statistic[1], "r")
  expect_equal(processedText$statisticalOutput$reported[1], "r=0.676")
  expect_equal(processedText$statisticalOutput$cleaned[1], "r=0.676")
  }
)
