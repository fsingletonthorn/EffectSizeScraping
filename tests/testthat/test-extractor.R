context("Effect size / test statistic extraction")

# suppressWarnings( library(stringr))
# suppressWarnings( library(dplyr))
# suppressWarnings( library(tibble))

# test sets
testF <- c("F(1, 12345) = 12.42345",
           "F(10,12345)=12.42345",
           "F(23, 12345) = 12.42345",
           "F (1, 1234) = 12.42345",
           "F ( 1, 1235 ) = 1",
           "F ( 1, 12345 ) = .42345",
           "F ( 2, 1245 ) = 12",
           "F ( 1, 12345 ) = 12,  p = .01",
           "F ( 1, 1345 ) = 12 p = 0.01",
           "F(1, 12345) = 12.42345, p < .01",
           "F1, 12345 = 12.42345, p < .01",
           "F1, 12345 = 12.42345, P < .01",
           "F = 12.42345, p < .01",
           "F = 12.42345, P < .01")

testT <- c("t(15) = 12.42345",
           "t(1)=12.42345",
           "t(15) = 12.42345",
           "t(145) = 12.42345",
           "t(1345 ) = 1",
           "t(1345 ) = .42345",
           "t(1345 ) = 12",
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
           "t (1345 ) = 12",
           "t (1345 ) = 12.0, p = .99",
           "t (1345 ) = 12.0, P = .99",
           "t = 12.0, p = .99",
           "t = 12.0, P = .99")

testR <- c("r(1) = .42345",
           "r(2)= .1345",
           "r(3) = 0.4234",
           "r( 4 ) = 0.45",
           "r(df = 5) = 1",
           "r (n=6) = .45",
           "r = .7",
           "correlation of .8", 
           # note the following should not be included in tests (it is removed)
           "R2 = .12",
           "correlation of .8, p = .009")

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
             "eta squared = .1232",
             "η2 = .3213")


testTString  <- stringr::str_flatten(testT, collapse = " ")
testFString  <- stringr::str_flatten(testF, collapse = " ")
testRString  <- stringr::str_flatten(testR, collapse = " ")
testChiString  <- stringr::str_flatten(testChi, collapse = " ")
testDString <- stringr::str_flatten(testD, collapse = " ")
testEtaString <- stringr::str_flatten(testEta, collapse = " ")


test_that("t test extractor works", {
  extracted <- extractTestStats(testTString)
  
  expect_identical(extracted$reported, testT)
  
  expect_identical(extracted$value,
                   as.numeric(
                   stringr::str_remove_all(
                     stringr::str_extract(
                       testT,
                       "(?<=t\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\s{0,3}\\)?\\s{0,3}\\=\\s{0,3})\\s{0,3}-?\\s{0,3}\\d*\\.?\\d*"
                     ),
                     "\\s"
                   )))
  
  expect_true(all(is.na(extracted$df1)))
  
  expect_identical(extracted$df2[1:30],
                   as.numeric(
                     stringr::str_remove_all(stringr::str_extract(
                       testT,
                       "t\\s{0,3}\\(?\\d*"
                     ),
                     "t\\s{0,3}\\(?"
                     )[1:30]
  )  )
  
  expect_true(all(is.na(extracted$df2[31:32])))
  
  expect_identical(extracted$p,
                     stringr::str_extract(
                       testT,
                       "(p|P)\\s{0,5}\\=?\\s{0,5}(<\\s*)?(>\\s*)?0?\\.\\d*"
                     ))
  
})

test_that("F test extractor works", {
  extracted <- extractTestStats(testFString)
  
  expect_identical(extracted$reported, testF)

  expect_identical(extracted$value,
                   as.numeric(
                   stringr::str_extract(
                     testF,
                     "(?<=F\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\,\\s{0,3}\\d{0,10}\\s{0,3}\\)?\\s{0,3}\\=\\s{0,3})\\s{0,3}-?\\s{0,3}\\d*\\.?\\d*"
                   )  %>%
                     ifelse(is.na(.), "12.42345", . ) %>%
                     stringr::str_remove_all("\\s")
                   ))

  expect_identical(extracted$df1,
                   as.numeric(
                   stringr::str_remove_all(
                     stringr::str_remove_all(stringr::str_extract(
                       testF,
                       "F\\s{0,3}\\(?\\s{0,3}\\d{0,10}"
                     ),
                     "\\s"
                     ) 
                     , "F\\(?") %>%
                     ifelse(.=="", NA, .)
  ))
  
  expect_identical(extracted$df2,
                   as.numeric(
                   stringr::str_remove_all(stringr::str_extract(
                     testF,
                     "(?<=F\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\s{0,3},)\\s*\\d*"
                   ),
                   "\\s*"
                   )
  ))
  
expect_identical(extracted$p,
                stringr::str_extract(testF, "((p|P)\\s{0,5}\\=?\\s{0,5})(<\\s*)?(>\\s*)?0?\\.\\d*"))
})

test_that("correlation extractor works", {
  extracted <- extractTestStats(testRString)
  expect_identical(extracted$reported, testR[-9])
})

# Setting up chi square values
chis <- c(stringr::str_remove_all(
  stringr::str_extract(
    testChi,
    "(?<=((chi square)|(χ2)|(<U\\+03C7>)|(chi squared)|(chisquared)|(chisquare)|(chi2?))\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\s{0,3},?\\s{0,3}N?\\s{0,3}\\=?\\s{0,3}\\d{0,10}\\s{0,3}\\)?\\s{0,3}\\=\\s{0,3})\\s{0,3}-?\\s{0,3}\\d*\\.?\\d*"
  ),
  "\\s"
))
chis[6] <- 22.31

test_that("chi squared test extractor works", {
  extracted <- extractTestStats(testChiString)
  expect_identical(extracted$reported, testChi)
  
  expect_identical(extracted$value,
                    as.numeric(chis))
  expect_true(all(is.na(extracted$df1)))
  expect_identical(extracted$df2,
                   as.numeric(
                   stringr::str_remove_all(stringr::str_extract(testChi,
                                              "((chi square)|(χ2)|(<U\\+03C7>)|(chi squared)|(chisquared)|(chisquare)|(chi2?))\\s{0,3}\\(\\d*"),
                                  "(chi square)|(χ2)|(<U\\+03C7>)|(chi squared)|(chisquared)|(chisquare)|(chi2?)\\s{0,3}\\(|\\(")))
  
  
  expect_identical(extracted$p,
                     stringr::str_extract(
                       testChi,
                       "((p|P)\\s{0,5}\\=?\\s{0,5})(<\\s*)?(>\\s*)?0?\\.\\d*"
                     ))
})

test_that("eta squared extractor works", {
  extracted <- extractTestStats(testEtaString)
  expect_identical(extracted$reported, testEta)
  expect_identical(extracted$value, 
                   as.numeric(
                   stringr::str_remove_all(testEta, "(η2)?(η)?[a-zA-Z]*\\s*\\=*\\s*")))
  
  expect_true(all(is.na(extracted$df1)))
})

test_that("cohen's d extractor works", {
  extracted <- extractTestStats(testDString)
  expect_identical(extracted$reported, testD)
  expect_identical(extracted$value, as.numeric(stringr::str_remove_all(testD, "[a-zA-Z]*\\s*\\=*\\s*")) )
  expect_true(all(is.na(extracted$df1)))
}
)

test_that("basic labelling works", {
  extracted <- extractTestStats(testDString, sectionName =  "as")
  expect_identical(extracted$sectionName[1], "as")
}
)

test_that("extractor can deal with real world examples", { 

  # testString <- "Results. Our most fundamental prediction was th of causation and blame would be based more on whet behavior was good or bad than whether it was normat normative. The results confirmed this prediction. Gra as less causal when his behavior was good (M = 3.37) t was bad (M = 5.20), F(l, 178) = 17.12, p < .0001, an blamed less when his behavior was good (M = 2.92) tha bad (M = 5.19), F(l, 178) = 27.98, p < .0001. Overall, main effect of whether his behavior was normative or counternormative on causal ratings, F(l, 178) = 2.10, p< .15, or on blame, /\"(l, 178) = 1.59, p < .21."
  
  expect_equal(extractTestStats(  "F(l, 178) = 17.12, p < .0001" )$statistic, "F")
  expect_equal(extractTestStats(  "F(l, 17l) = 17.12, p < .0001" )$statistic, "F")
  expect_equal(extractTestStats(  "F(1, l78) = 17.12, p < .0001" )$statistic, "F")
  
  expect_equal(extractTestStats(  "F(  l, 178  ) = 17.12, p < .0001" )[[1]], "F")
  expect_equal(extractTestStats("F( l , 17l ) = 17.12, p < .0001" , context = T)[[1]], "F")
  expect_equal(extractTestStats(  "F( 1, l78 ) = 17.12, p < .0001" , context = T)[[1]], "F")
  }) 

test_that("extractor can extract context or not", { 
  expect_equal(extractTestStats(  "F(  1, 178  ) = 17.12, p < .0001")$value, 17.12)
  expect_equal(extractTestStats("test patern F( 1, 178 ) = 17.12, p < .0001 test pattern" , context = T, contextSize = 5)$context, 
               "tern F( 1, 178 ) = 17.12, p < .0001 test")
}) 

test_that("t test extractor doesn't extract values without test statistic values",{
  expect_equal(extractTestStats("t(1) = 1, p = .8, and t(12) = .12")$value, c(1,.12)) 
  expect_true(all(is.na(extractTestStats("t(1), p = .8, and t(c) = .12"))))
})

test_that("chi square can deal with annoying characters", {
expect_equal(extractTestStats("X2(12) = .12")$value, c(.12)) 
})

test_that("R = 1 is not captured as r = ", {
expect_true(purrr::is_empty(extractTestStats("R = 1.1")$value)) 
})

test_that("t test extractor picks up ideosyncratically reported DFs", {
expect_equal(extractTestStats("t = 2, df = 42,  p <.02")$df2, 42) 
expect_equal(extractTestStats("t = 2, df = 42,  p <.02")$value, 2) 
})

test_that("F test extractor picks up ideosyncratically reported DFs", {
expect_equal(extractTestStats("F1,2 = 2.202")$df2, 2) 
})


test_that("some additional ideosyncractic methods of reporting work", {

  test <- extractTestStats("F(1, 2) : 3, p : .04")
  expect_true(all(test$statistic == "F", 
                  test$reported =="F(1, 2) : 3, p : .04", test$value == "3", 
                  test$df1 == "1", test$df2 == "2", test$p == "p : .04"))  
  test <- extractTestStats("T(1) : 3, p : .04")
  expect_true(all(test$statistic == "t", test$value == 3, 
                  test$df2 == 1, test$p == "p : .04"))
  test <- extractTestStats("Chi2(3) : 1, p : .04")
  expect_true(all(test$statistic == "chi", test$value == "1", 
                  test$df2 == "3", test$p == "p : .04"))
  test <- extractTestStats("r(df : 12) : .99999, p : .04")
  expect_true(all(test$statistic == "r", test$value == ".99999", 
                  test$df2 == "12", test$p == "p : .04"))
  
  expect_true(extractTestStats("t(df=22) = 2.202")$df2 == 22)
  expect_true(extractTestStats("t(df=22) = 2.202")$value == 2.202)
  
  expect_equal( extractTestStats("ρ(n = 123) = 0.98, p < .05")$df2, 121)
  expect_equal( extractTestStats("ρ(n = 123) = 0.98, p < .05")$value, 0.98)
  
  }
)

test_that("; does not break the function", {
  expect_silent(
  test <- extractTestStats("F; 400")
  )
  expect_silent(
    test <- extractTestStats("F; 400 F;400")
  )
})  

test_that("; are accepted in test stats", {
  expect_true(
    nrow(extractTestStats("F; 400F; 400")) == 0)
})  
