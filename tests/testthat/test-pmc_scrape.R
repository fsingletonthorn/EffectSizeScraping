context("test pmc_scrape")

test_that("scrapePMC extracts files with correctly labeled sections", {
  # pmcID <- 4710337
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4710337&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/bd/41/PMC4710337.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)
  
  expect_true(
  stringr::str_detect(output$text$text[output$text$names == "unlabelled"],
             "Prompt diagnosis is important as urgent")
  )
  expect_true(is.na( output$statisticalTests ) )
})

test_that("scrapePMC doesn't extract from PDFs when it is not necessary", {
  # pmcID <- 5393010
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5393010&metadataPrefix=pmc"
  ftpCall  <-"ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/a2/4e/PMC5393010.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)
  
  expect_true(
    stringr::str_detect(output$text$text[output$text$names == "Discussion"],
                        "BEI-PSY is the first study to investigate the prevalence")
  )
  
  expect_true(all(
    output$statisticalTests$statistic == "chi"
  )) 
})


test_that("scrapePMC process excracts stats correctly from pmc5504157", {
  # pmcID <- 5504157
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5504157&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/4a/be/PMC5504157.tar.gz"

 output <- scrapePMC(call, ftpCall, statcheck = F)
 
 
 expect_true(all(as.numeric(output$statisticalTests$value[
   output$statisticalTests$statistic == "r"
   ]) %in%
 c(0.10,0.30,0.71,0.69,0.63,0.61,0.59,0.56,0.86,0.32,0.75,0.67,0.51,
 0.45,0.39,0.41,0.30,0.31,0.31,0.59,0.56,0.48,0.52,0.38,0.63,0.41)))
 
 expect_true(all(
   as.numeric(output$statisticalTests$value[output$statisticalTests$statistic == "d"]) %in%
     c(0.07,
       0.44,
       0.20)
 ))
 
 # concatinatedText <- concatPlus(output$text$text)
 expect_true(
 stringr::str_detect(output$text$text[output$text$names == "Results"],
                     "ranged from")
)
 expect_true(
   stringr::str_detect(output$text$text[output$text$names == "Results"],
                       "in the cross-sectional design\\.\\s?$")
 )
})


test_that("scrapePMC extracts the pdf when text is not otherwise avaliable", {
  # pmcID <- 4710337
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5393010&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/a2/4e/PMC5393010.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)

  expect_true(
    stringr::str_detect(output$text$text[output$text$names == "Discussion"],
                        "The findings of this study must be")
  )
})

test_that("scrapePMC extracts the pdf when text is not otherwise avaliable", {
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4311551&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/26/85/PMC4311551.tar.gz"
  output <- scrapePMC(call, ftpCall, statcheck = F)

  expect_true(
    stringr::str_detect(output$text$text[output$text$names == "unlabelled"],
                        "The increased interest in the sense of touch over the last decades will hopefully continue as much still remains to be explored\\.$")
  )
})


test_that("test pmc extracts statistical test correctly", {
  pmcID <- "PMC5504157"
  call <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5504157&metadataPrefix=pmc"
  ftpCall  <- "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/4a/be/PMC5504157.tar.gz"
  output <- scrapePMC(call, ftpCall)
  
  expect_true(output$metadata$PMCID == pmcID)
  expect_identical(as.numeric(output$statisticalTests$value[output$statisticalTests$statistic == "d"]), c(.07, .44, .20))
  
  })


test_that("initial test set gives accurate information", {
 output <- scrapePMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4311551&metadataPrefix=pmc",
          ftpCall =  "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/26/85/PMC4311551.tar.gz", statcheck = F)
  expect_equal(output$metadata$title, "Body in mind")
  expect_equal(output$metadata$doi, "10.3389/fpsyg.2015.00056")
  expect_true(all(is.na(output$sampleSizes)))
  expect_true(all(is.na(output$statisticalTests)))
  
  output <- scrapePMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5572533&metadataPrefix=pmc",
                      ftpCall =  "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/b1/cd/PMC5572533.tar.gz", statcheck = T)
  expect_equal(output$metadata$title, "Expression differences in Aphidius ervi (Hymenoptera: Braconidae) females reared on different aphid host species")
  expect_equal(output$metadata$doi, "10.7717/peerj.3640")
  expect_equal(output$sampleSizes$n, c("20", "422", "422", "3445", "2944"))
  expect_true(all(output$keywords$keywords %in% c("Phenotypic plasticity", "Local adaptation", "Parasitoid wasps", "Transcriptome", "Aphid pest control")) & 
                    all(c("Phenotypic plasticity", "Local adaptation", "Parasitoid wasps", "Transcriptome", "Aphid pest control") %in% output$keywords$keywords))
  
    output <- scrapePMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5106936&metadataPrefix=pmc",
                      ftpCall =  "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/fe/62/PMC5106936.tar.gz", statcheck = T)
  expect_equal(output$metadata$title, "Mental Defectiveness")
  expect_true(output$metadata$pPub == as.Date("1928-04-15 UTC"))
  expect_true(is.na(output$metadata$doi))
  expect_true(is.na(output$sampleSizes$n))
  expect_equal(dim(output$keywords)[1], 0)
  expect_true(all(is.na(output$statisticalTests)))
  expect_false(any(output$CIs$CIBinary))
  
  
    output <- scrapePMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3704508&metadataPrefix=pmc",
                      ftpCall = "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/ea/88/PMC3704508.tar.gz", statcheck = T)
  expect_equal(output$metadata$title, "Modeling ripple oscillations in the hippocampus")
  expect_true(output$metadata$ePub == as.Date("2013-07-08 UTC"))
  expect_true(is.na(output$sampleSizes$n))
  expect_equal(dim(output$keywords)[1], 0)
  expect_true(is.na(output$statisticalTests)[1])
  expect_false(any(output$CIs$CIBinary))
  

output <- scrapePMC(call =  "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:5414266&metadataPrefix=pmc",
                      ftpCall =  "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/3c/98/PMC5414266.tar.gz", statcheck = T)
  expect_equal(output$metadata$title, "Multiple phase transitions in an agent-based evolutionary model with neutral fitness")

  expect_equal(output$metadata$doi, "10.1098/rsos.170005")
  expect_true(is.na(output$sampleSizes$n[1]))
  expect_equal(output$keywords$keywords, c("phase transition", "extinction", "agent-based model", "neutral theory", "clustering", "speciation"))
  expect_true(is.na(output$sampleSizes$n[1]))
  expect_false(any(output$CIs$CIBinary))  

output <- scrapePMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:4901434&metadataPrefix=pmc",
                      ftpCall = "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/d3/78/PMC4901434.tar.gz", statcheck = T)
  expect_equal(output$authors$surname, c("Azale", "Fekadu", "Hanlon"))
  
  expect_equal(output$authors$firstname, c("Telake", "Abebaw", "Charlotte"))
  expect_equal(output$metadata$PMCID, "PMC4901434")
  expect_equal(output$metadata$doi, "10.1186/s12888-016-0892-8")
  expect_equal(output$metadata$journalID, "BMC Psychiatry")
  expect_equal(output$metadata$journalIDAbrev, "BMC Psychiatry")
  expect_equal(output$metadata$title, "Treatment gap and help-seeking for postpartum depression in a rural African setting")
  expect_true(is.na(output$metadata$issue))
  expect_equal(output$metadata$volume,  "16")
  expect_equal(output$metadata$ePub,  as.POSIXlt.character("2016-06-10", tz = "UTC"))

  expect_equal(max(as.numeric(output$sampleSizes$n)), 3147)
  expect_true(all(as.numeric(output$sampleSizes$n) %in% c(385, 3147, 16,  49, 231, 374, 363, 271, 365, 353, 385, 196, 171, 104, 95, 
                                                          16, 236, 12, 385, 239, 293, 257, 216, 194, 385,  15,  2))
  )
  expect_true(is.na(output$keywords$keywords[1]))
  expect_true(any(output$CIs$CIBinary))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "Conclusion"], "Creating public awareness about PPD, its causes and consequences, and the need for help seeking are necessary steps to support the integration of mental health into primary care-based maternal health care."))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "Discussion"], "In this population-based survey of postpartum women"))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "abstract"], "Postpartum depression"))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "abstract"], "care services within primary care"))

  
output <- scrapePMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3172542&metadataPrefix=pmc",
                      ftpCall = "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/64/34/PMC3172542.tar.gz", statcheck = T)
 
  expect_equal(output$authors$surname, "Channabasavanna")
  expect_equal(output$authors$firstname, "S.M.")
  expect_equal(output$metadata$PMCID, "PMC3172542")
  expect_true(is.na(output$metadata$doi))
  expect_equal(output$metadata$journalID, "Indian Journal of Psychiatry")
  expect_equal(output$metadata$journalIDAbrev, "Indian J Psychiatry")
  expect_equal(output$metadata$title, "THE NARCOTIC DRUGS ACT AND THE PSYCHIATRIST")
  expect_equal(output$metadata$issue, "2")
  expect_equal(output$metadata$volume,  "28")
  expect_equal(output$metadata$pPub,  as.POSIXlt.character("1986-01-01", tz = "UTC"))

  expect_true(is.na(output$sampleSizes$n))
  expect_true(is.na(output$keywords$keywords[1]))
  expect_false(any(output$CIs$CIBinary))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "unlabelled"], "The legislation to make stringent provision"))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "unlabelled"], "hether any difficulties arise out of enforcing the Act"))

  
  
output <- scrapePMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3172542&metadataPrefix=pmc",
                      ftpCall = "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/64/34/PMC3172542.tar.gz", statcheck = T)
 
  expect_equal(output$authors$surname, "Channabasavanna")
  expect_equal(output$authors$firstname, "S.M.")
  expect_equal(output$metadata$PMCID, "PMC3172542")
  expect_true(is.na(output$metadata$doi))
  expect_equal(output$metadata$journalID, "Indian Journal of Psychiatry")
  expect_equal(output$metadata$journalIDAbrev, "Indian J Psychiatry")
  expect_equal(output$metadata$title, "THE NARCOTIC DRUGS ACT AND THE PSYCHIATRIST")
  expect_equal(output$metadata$issue, "2")
  expect_equal(output$metadata$volume,  "28")
  expect_equal(output$metadata$pPub,  as.POSIXlt.character("1986-01-01", tz = "UTC"))

  expect_true(is.na(output$sampleSizes$n))
  expect_true(is.na(output$keywords$keywords[1]))
  expect_false(any(output$CIs$CIBinary))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "unlabelled"], "The legislation to make stringent provision"))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "unlabelled"], "hether any difficulties arise out of enforcing the Act"))
  
output <- scrapePMC(call = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3172542&metadataPrefix=pmc",
                      ftpCall = "ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_package/64/34/PMC3172542.tar.gz", statcheck = T)
 
  expect_equal(output$authors$surname, "Channabasavanna")
  expect_equal(output$authors$firstname, "S.M.")
  expect_equal(output$metadata$PMCID, "PMC3172542")
  expect_true(is.na(output$metadata$doi))
  expect_equal(output$metadata$journalID, "Indian Journal of Psychiatry")
  expect_equal(output$metadata$journalIDAbrev, "Indian J Psychiatry")
  expect_equal(output$metadata$title, "THE NARCOTIC DRUGS ACT AND THE PSYCHIATRIST")
  expect_equal(output$metadata$issue, "2")
  expect_equal(output$metadata$volume,  "28")
  expect_equal(output$metadata$pPub,  as.POSIXlt.character("1986-01-01", tz = "UTC"))

  expect_true(is.na(output$sampleSizes$n))
  expect_true(is.na(output$keywords$keywords[1]))
  expect_false(any(output$CIs$CIBinary))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "unlabelled"], "The legislation to make stringent provision"))
  expect_true(stringr::str_detect(output$text$text[output$text$names == "unlabelled"], "hether any difficulties arise out of enforcing the Act"))
  }
)




