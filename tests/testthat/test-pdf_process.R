context("pdf reading functions")

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
      pdf[[which(pdf[1] == "Methods"), 2]],
      "Analysis was conducted using the statistical package for the social sciences \\(SPSS\\) \\[version 22.0\\]\\.$"
    ) & stringr::str_detect(
      pdf[[which(pdf[1] == "Methods"), 2]],
      "BEI-PSY is a cross-sectional survey that targeted Arabic speaking adolescents"
    )
  )
})

# Also at: "data_/trainingSet/PMC5420575.pdf"
test_that("extractPdf extracts files with correctly labeled sections", {
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
          "environmental issues and prosocial disposition would also be useful\\.$"
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
      "Analysis was conducted using the statistical package for the social sciences \\(SPSS\\) \\[version 22.0\\]\\.$"
    ) & stringr::str_detect(
      pdf[[which(pdf[1] == "MATERIALS AND METHODS"), 2]],
      "BEI-PSY is a cross-sectional survey that targeted Arabic speaking adolescents"
    )
  )
})





