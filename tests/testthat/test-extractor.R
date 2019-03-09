context("Esffect size / test statistic extraction")

suppressWarnings( library(stringr))
suppressWarnings( library(base))
suppressWarnings( library(dplyr))
suppressWarnings( library(tibble))

# test sets
testF <- c("F(1, 12345) = 12.42345",
           "F(10,12345)=12.42345",
           "F(23, 12345) = 12.42345",
           "F (1, 1234) = 12.42345",
           "F ( 1, 1235 ) = 1",
           "F ( 1, 12345 ) = .42345",
           "F ( 2, 1245 ) = 12.",
           "F ( 1, 12345 ) = 12.,  p = .01",
           "F ( 1, 1345 ) = 12 p = 0.01",
           "F(1, 12345) = 12.42345, p < .01",
           "F1, 12345 = 12.42345, p < .01",
           "F1, 12345 = 12.42345, P < .01")

testT <- c("t(15) = 12.42345",
           "t(1)=12.42345",
           "t(15) = 12.42345",
           "t(145) = 12.42345",
           "t(1345 ) = 1",
           "t(1345 ) = .42345",
           "t(1345 ) = 12.",
           "t(15) = -12.42345",
           "t(1)=-12.42345",
           "t(15) = -12.42345",
           "t(145) = -12.42345",
           "t(1345 ) = -1",
           "t(1345 ) = -.42345",
           "t(1345 ) = -12",
           "t(15) = - 12.42345",
           "t(1)=- 12.42345",
           "t(15) = - 12.42345, p = .123",
           "t(145) = - 12.42345",
           "t(1345 ) = - 1",
           "t(1345 ) = - .42345, p = 0.00121",
           "t(1345 ) = - 12",
           "t(15) = 12.42345",
           "t (1)=12.42345",
           "t (15) = 12.42345",
           "t (145) = 12.42345",
           "t (1345 ) = 1",
           "t (1345 ) = .42345",
           "t (1345 ) = 12.",
           "t (1345 ) = 12.0, p = .99",
           "t (1345 ) = 12.0, P = .99")

testR <- c("r(1) = .42345",
           "r(2)= .1345",
           "r(3) = 0.4234",
           "r( 4 ) = 0.45",
           "r(df = 5) = 1",
           "r (n=6) = .45",
           "r = .7",
           "correlation of 8",
           "R2 = .12", # note this should not be included
           "correlation of 8, p = .009")

testChi <- c("chi square = 12.32",
             "chi2 = 123.32",
             "χ2(123) = 1232.23, p < .05",
             "χ2 = 122.23,p = .13",
             "chi2(123) = 123.2, p < .001",
             "χ2(1, N = 320) = 22.31, p < 0.001")


testD <- c("g = 12.32",
           "d = 1",
           "d = 1232.23")

testEta <- c("η2 = .3213",
             "eta squared = 1.232",
             "η2 = .3213")

testTString  <- str_flatten(testT, collapse = " ")
testFString  <- str_flatten(testF, collapse = " ")
testRString  <- str_flatten(testR, collapse = " ")
testChiString  <- str_flatten(testChi, collapse = " ")
testDString <- str_flatten(testD, collapse = " ")
testEtaString <- str_flatten(testEta, collapse = " ")


test_that("t test extractor works", {
  extracted <- extractTestStats(testTString)
  
  expect_identical(extracted[[3]], testT)
  
  expect_identical(extracted[[2]],
                     str_remove_all(testT, "\\s"))
  
  expect_identical(extracted[[4]],
                   str_remove_all(
                     str_extract(
                       testT,
                       "(?<=t\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\s{0,3}\\)?\\s{0,3}\\=\\s{0,3})\\s{0,3}-?\\s{0,3}\\d*\\.?\\d*"
                     ),
                     "\\s"
                   ))
  
  expect_true(all(is.na(extracted[[5]])))
  
  expect_identical(extracted[[6]],
                     str_remove_all(str_extract(
                       testT,
                       "t\\s{0,3}\\(?\\d*"
                     ),
                     "t\\s{0,3}\\(?"
                     )
  )
  
  
  expect_identical(extracted[[7]],
                   str_remove_all(
                     str_extract(
                       testT,
                       "(?<=((p|P)\\s{0,5}\\=?\\s{0,5}))(<\\s*)?(>\\s*)?0?\\.\\d*"
                     ),
                     "\\s"
                   ))
  
})

test_that("F test extractor works", {
  extracted <- extractTestStats(testFString)
    
  testthat::expect_identical(extracted[[2]],
                     stringr::str_remove_all(testF, "\\s"))
  
  expect_identical(extracted[[3]], testF)

  expect_identical(extracted[[4]],
                   str_remove_all(
                     str_extract(
                       testF,
                       "(?<=F\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\,\\s{0,3}\\d{0,10}\\s{0,3}\\)?\\s{0,3}\\=\\s{0,3})\\s{0,3}-?\\s{0,3}\\d*\\.?\\d*"
                     ),
                     "\\s"
                   ))
    
  expect_identical(extracted[[5]],
                   str_remove_all(
                     str_remove_all(str_extract(
                       testF,
                       "F\\s{0,3}\\(?\\s{0,3}\\d{0,10}"
                     ),
                     "\\s"
                     )
                     , "F\\(?")
  )
  
  expect_identical(extracted[[6]],
                   str_remove_all(str_extract(
                     testF,
                     "(?<=F\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\s{0,3},)\\s*\\d*"
                   ),
                   "\\s*"
                   )
  )
  
expect_identical(extracted[[7]],
                str_remove_all(str_extract(testF, "(?<=((p|P)\\s{0,5}\\=?\\s{0,5}))(<\\s*)?(>\\s*)?0?\\.\\d*"), "\\s"))
})

test_that("correlation extractor works", {
  extracted <- extractTestStats(testRString)
  expect_identical(extracted[[3]], testR[-9])
  expect_identical(extracted[[2]],
                     str_remove_all(testR, "\\s")[-9])
})

test_that("chi squared test extractor works", {
  extracted <- extractTestStats(testChiString)
  expect_identical(extracted[[3]], testChi)
  expect_identical(extracted[[2]],
                     str_remove_all(testChi, "\\s"))
  expect_identical(extracted[[4]],
                   str_remove_all(
                     str_extract(
                       testChi,
                       "(?<=((chi square)|(χ2)|(<U\\+03C7>)|(chi squared)|(chisquared)|(chisquare)|(chi2?))\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\s{0,3},?\\s{0,3}N?\\s{0,3}\\=?\\s{0,3}\\d{0,10}\\s{0,3}\\)?\\s{0,3}\\=\\s{0,3})\\s{0,3}-?\\s{0,3}\\d*\\.?\\d*"
                     ),
                     "\\s"
                   ))
  
  expect_true(all(is.na(extracted[[5]])))
  
  expect_identical(extracted[[6]],
                   str_remove_all(str_extract(testChi,
                                              "((chi square)|(χ2)|(<U\\+03C7>)|(chi squared)|(chisquared)|(chisquare)|(chi2?))\\s{0,3}\\(\\d*"),
                                  "(chi square)|(χ2)|(<U\\+03C7>)|(chi squared)|(chisquared)|(chisquare)|(chi2?)\\s{0,3}\\(|\\("))
  
  
  expect_identical(extracted[[7]],
                   str_remove_all(
                     str_extract(
                       testChi,
                       "(?<=((p|P)\\s{0,5}\\=?\\s{0,5}))(<\\s*)?(>\\s*)?0?\\.\\d*"
                     ),
                     "\\s"
                   ))
})

test_that("eta squared extractor works", {
  extracted <- extractTestStats(testEtaString)
  expect_identical(extracted[[3]], testEta)
  expect_identical(extracted[[2]],
                   str_remove_all(testEta, "\\s"))
  expect_identical(extracted[[4]], 
                   str_remove_all(testEta, "(η2)?(η)?[a-zA-Z]*\\s*\\=*\\s*")) 
  expect_true(all(is.na(extracted[[5]])))
})

test_that("cohen's d extractor works", {
  extracted <- extractTestStats(testDString)
  expect_identical(extracted[[3]], testD)
  expect_identical(extracted[[2]],
                   str_remove_all(testD, "\\s"))
  expect_identical(extracted[[4]], str_remove_all(testD, "[a-zA-Z]*\\s*\\=*\\s*")) 
  expect_true(all(is.na(extracted[[5]])))
}
)


