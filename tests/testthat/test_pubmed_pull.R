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

# Editor checks:
#Plos one edi
test_that("pullPMC can find editors", {
  output <- pullPMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4648576&metadataPrefix=pmc") 
  expect_equal(dim( output$editors), c(1,3) )
  expect_equal( output$editors$surname, "Voracek" )
  expect_equal( output$editors$firstname, "Martin" )
}
)

# Frontiers in Behaviorual neuroscience
test_that("pullPMC can find editors", {
output <- pullPMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:6379263&metadataPrefix=pmc")
expect_equal(dim( output$editors), c(0,3) )
expect_true( stringr::str_detect(output$editors_unstructured[1,2], "Antonella Gasbarri"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Nadja Schroder"))
}
)

# Frontiers in neuroscience
test_that("pullPMC can find editors", {
output <- pullPMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:6018545&metadataPrefix=pmc")
expect_equal(dim( output$editors), c(0,3) )
expect_true( stringr::str_detect(output$editors_unstructured[1,2], "Graziella Madeo"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Paolo Gubellini"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Nicolas Xavier Tritsch"))
}
)

# Frontiers in Human Neuroscience
test_that("pullPMC can find editors", {
output <- pullPMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3882720&metadataPrefix=pmc")
expect_equal(dim( output$editors), c(0,3) )
expect_true( stringr::str_detect(output$editors_unstructured[1,2], "Edited by: Merim Bilalic, University Tübingen"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Reviewed by: E. Glenn Schellenberg, University of Toronto, Canada"))
}
)

# "Frontiers in Psychiatry"
test_that("pullPMC can find editors", {
output <- pullPMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4033190&metadataPrefix=pmc")
expect_equal(dim( output$editors), c(0,3) )
expect_true( stringr::str_detect(output$editors_unstructured[1,2], "Edited by: Elizabeth Clare Temple, Federation University Australia, Australia"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Reviewed by: Thomas Hillemacher, Hannover Medical School"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Elizabeth Clare Temple, Federation University Australia"))
}
)

# Frontiers in Human Neuroscience
test_that("pullPMC can find editors", {
output <- pullPMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3882720&metadataPrefix=pmc")
expect_equal(dim( output$editors), c(0,3) )
expect_true( stringr::str_detect(output$editors_unstructured[1,2], "Edited by: Merim Bilalic, University Tübingen"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Reviewed by: E. Glenn Schellenberg, University of Toronto, Canada"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Reviewed by: E. Glenn Schellenberg, University of Toronto, Canada"))
}
)

# Frontiers in psychology example
test_that("pullPMC can find editors", {
output <- pullPMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:6543906&metadataPrefix=pmc")
expect_equal(dim( output$editors), c(0,3) )
expect_true( stringr::str_detect(output$editors_unstructured[1,2], "Edited by:"))
expect_true( stringr::str_detect(output$editors_unstructured[1,2], "Michele Biasutti, University of Padova, Italy"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Reviewed by:"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Catherine Alexandra Lebel"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "Floris Tijmen Van Vugt"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "University of Calgary, Canada"))
expect_true( stringr::str_detect(output$editors_unstructured[2,2], "McGill University, Canada"))
}
)



