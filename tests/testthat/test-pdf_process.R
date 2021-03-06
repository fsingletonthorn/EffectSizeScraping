context("pdf reading functions")

test_that("extractPdf extracts all pages", {
  pdf <- extractPdf("https://osf.io/nztsx/download")
  # pdf2 <- extractPdf("https://osf.io/kcbnu/download")
  # expect_true(sum(lapply(pdf$text , stringr::str_count, pattern = "or")))
  expect_true(
    stringr::str_detect(
      pdf[stringr::str_detect(pdf$names, "Discussion"),2],
      stringr::fixed(
        "for example, might evoke many of the same"
      )
    ) & stringr::str_detect(
        pdf[[6, 2]],
        stringr::fixed("In this experimental study, we manipulated")
      )
  )
})

# Also at: "data_/trainingSet/PMC5393010.pdf"
test_that("extractPdf extracts files with correctly labeled sections", {
  pdf <- extractPdf("https://osf.io/v7n6j/download")
  # pdf2 <- extractPdf("https://osf.io/kcbnu/download")
  # expect_true(sum(lapply(pdf$text , stringr::str_count, pattern = "or")))
  expect_true(any(pdf[1]=="Abstract"))
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1]=="Abstract"), 2]],
      stringr::fixed(
        "This study aims to investigate the prevalence, correlates and treatment seeking behavior related to ADHD among adolescents from Lebanon."
      )
    ) & stringr::str_detect(
      pdf[[which(pdf[1]=="Abstract"), 2]],
      stringr::fixed("ADHD among adolescents in Lebanon warrants closer attention, mainly increased awareness in the larger public, and stronger commitment to increase treatment resources to the community.")
    )
  )
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1]=="Background"), 2]],
      stringr::fixed(
        "Symptoms need to be present in two settings at least and have to"
      )
    ) & stringr::str_detect(
      pdf[[which(pdf[1]=="Background"), 2]],
      stringr::fixed("The results are discussed vis-à-vis regional")
    )
  )
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1]=="Discussion"), 2]],
      stringr::fixed(
        "BEI-PSY is the first study to investigate the prevalence"
      )
    ) & stringr::str_detect(
      pdf[[which(pdf[1]=="Discussion"), 2]],
      "but also with the affected adolescent\\.$")
    )
  
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1] == "Data analysis"), 2]],
      "Analysis was conducted using the statistical package for the social sciences \\(SPSS\\) \\[version 22.0\\]\\.$"
    ) & stringr::str_detect(
      pdf[[which(pdf[1] == "Methods"), 2]],
      "BEI-PSY is a cross-sectional survey that targeted Arabic speaking adolescents"
    )
  )
})


# Also at: "data_/trainingSet/PMC5420575.pdf"
test_that("extractPdf extracts files with correctly labeled sections (PMC5420575)", {
  pdf <- extractPdf("https://osf.io/dzm7v/download")
  # expect_true(any(pdf[1]=="Abstract"))
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1]=="DISCUSSION"), 2]],
      stringr::fixed(
        "In our modern and occidental societies competition and maximizing own profit are common"
      )
    ) & 
      stringr::str_detect(
        pdf[[which(pdf[1]=="DISCUSSION"), 2]],
          "considering practical ways of promoting pro-environmental behavior\\.$"
      )
  )
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1]=="CONCLUSION"), 2]],
      stringr::fixed(
        "This investigation has explored the role of prosocialness and trust"
      )
    ) & stringr::str_detect(
      pdf[[which(pdf[1]=="CONCLUSION"), 2]],
      "both the individual and collective level, which, consequently, would have benefits for the environment\\.$")
  )
  
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1] == "MATERIALS AND METHODS"), 2]],
      "The participants were 107 students of Environmental Sciences Degrees from the University"
    ) & stringr::str_detect(
      pdf[[which(pdf[1] == "MATERIALS AND METHODS"), 2]],
      "by Seibert and Vis \\(2012\\) and Ewen and Seibert \\(2016\\)\\.$"
    )
  )
  
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1] == "Procedure"), 2]],
      "Participation in the study contributed toward participants"
    ) & stringr::str_detect(
      pdf[[which(pdf[1] == "Procedure"), 2]],
      "scarcity of natural resources\\.$"
    )
  )
  
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1] == "Measures"), 2]],
      "was measured using a short version of the Prosocialness Scale"
    ) & stringr::str_detect(
      pdf[[which(pdf[1] == "Measures"), 2]],
      "through the 10 years\\.$"
    )
  )
  
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1] == "RESULTS"), 2]],
      "The repeated measure analyses \\(by including the 10 years"
    ) & stringr::str_detect(
      pdf[[which(pdf[1] == "RESULTS"), 2]],
      "H5 was confirmed\\.$"
    )
  )
})


# Also at: "data_/trainingSet/PMC5504157.pdf"
test_that("extractPdf extracts files with correctly labeled sections (PMC5504157)", {
  pdf <- extractPdf("https://osf.io/5nxjw/download")
  # expect_true(any(pdf[1]=="Abstract"))
  expect_true(
    stringr::str_detect(
      pdf[[which(pdf[1]=="INTRODUCTION"), 2]],
      stringr::fixed(
        "Research results show, that medical students"
      )
    ) & 
      stringr::str_detect(
        pdf[[which(pdf[1]=="INTRODUCTION"), 2]],
        "in time\\.$"
      )
  )

  expect_true(
    stringr::str_detect(
      pdf$text[[which(pdf$names=="RESULTS")]],
      "CIT scores and the VIA-120 total score were") &
    stringr::str_detect(
      pdf$text[[which(pdf$names=="RESULTS")]],
  "but not in the cross-sectional design\\.$")
  )
})
