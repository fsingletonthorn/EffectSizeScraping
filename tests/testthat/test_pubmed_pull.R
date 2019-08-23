context("Test pullPMC")

call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5588100&metadataPrefix=pmc"
output <- pullPMC(call)

test_that("pullPMC extracts correct text", {
expect_true(output$text$PMCID[[1]] == "PMC5588100")
expect_true(stringr::str_detect(output$text$text[output$text$names == "abstract"], stringr::fixed("which were significantly higher than those in cancer-adjacent normal tissues of the control group")))
            }
)

test_that("pullPMC extracts correct text", {
  expect_true(output$text$PMCID[1] == "PMC5588100")
  expect_true(stringr::str_detect(output$text$text[output$text$names == "abstract"], stringr::fixed("which were significantly higher than those in cancer-adjacent normal tissues of the control group")))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "Discussion"], stringr::fixed("ancer tissues of the patient group had no correlation with the existences of lump and localized density-increased shadow (p>0.05), but were associated with manifestations of architectural distortion, calcification as well as skin and nipple depression (p<0.05).")))
  }
)

test_that("pull PMC extracts and includes unlabelled text", {
  processedText <- pullPMC(call =  "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:2964795&metadataPrefix=pmc")
  expect_true(stringr::str_detect(processedText$text$text[processedText$text$names== "unlabelled"], stringr::fixed("Sir,")))
  expect_true(stringr::str_detect(processedText$text$text[processedText$text$names== "unlabelled"], stringr::fixed("cutaneous features")))
})

test_that("pubmebPull doesn't bring line breaks with keywords", {
  processedText <- pullPMC(call =  "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5414266&metadataPrefix=pmc")
  expect_false(any(stringr::str_detect(processedText$keywords$keywords, "\\n")))
  })
