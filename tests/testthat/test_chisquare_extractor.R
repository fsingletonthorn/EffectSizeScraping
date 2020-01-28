context("chisquare_extractor")


testChi <- c("chi square = 12.32",
             "chi2 = 123.32",
             "χ2(1234) = 1232.23, p < .05",
             "χ2 = 122.23,p = .13",
             "chi2(12345) = 123.2, p < .001",
             "χ2(1, N = 320) = 22.31, p < 0.001", 
             "χ2(n = 320, df =12) = 23.31, p < 0.001",
             "χ2( 12399) = 1232.23, p < .05",
             "χ2(12399 ) = 1232.23, p < .05", 
             "χ2(  12399) = 1232.23, p < .05") 



testChiString  <- stringr::str_flatten(testChi, collapse = " ")


# Setting up chi square values
chis <- c(stringr::str_remove_all(
  stringr::str_extract(
    testChi,
    "(?<=((chi square)|(χ2)|(<U\\+03C7>)|(chi squared)|(chisquared)|(chisquare)|(chi2?))\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\s{0,3},?\\s{0,3}N?\\s{0,3}\\=?\\s{0,3}\\d{0,10}\\s{0,3}\\)?\\s{0,3}\\=\\s{0,3})\\s{0,3}-?\\s{0,3}\\d*\\.?\\d*"
  ),
  "\\s"
))
chis[6] <- 22.31
chis[7] <-  23.31

dfs <- stringr::str_remove_all(stringr::str_extract(testChi,
                                                    "((chi square)|(χ2)|(<U\\+03C7>)|(chi squared)|(chisquared)|(chisquare)|(chi2?))\\s{0,3}\\(\\s{0,3}\\d*"),
                               "(chi square)|(χ2)|(<U\\+03C7>)|(chi squared)|(chisquared)|(chisquare)|(chi2?)\\s{0,3}\\(|\\(|\\s")
dfs[7] <- "12"

test_that("chi squared test extractor works", {
  extracted <- extractChiSquare(testChiString)
  expect_identical(extracted$reported, testChi)
  expect_identical(extracted$value,
                   as.numeric(chis))
  expect_true(all(is.na(extracted$df1)))
  expect_equal(extracted$df2,
                   as.double(dfs)
                   )
  
  
  expect_identical(extracted$p,
                     stringr::str_extract(
                       testChi,
                       "(p|P|, ns).*"
                     )
)
  
  expect_equal(extracted$n,
               as.numeric(
                     stringr::str_extract(
                       testChi,
                       "(?<=((N|n)\\s{0,5}=\\s{0,5}))\\d+"
                     )))
  
})


test_that("Chi squares that are misread are properly detected", {
  extracted <- extractChiSquare("χ(2)2=2.12,p < 0.001")
  expect_equal(extracted$value, 2.12)
  expect_equal(extracted$df2, 2)
  expect_equal(extracted$p, "p < 0.001")
})

test_that("Chi squares that are misread are properly detected", {
  extracted <- extractChiSquare("χ(2,  n = 123)2=2.12,p < 0.001")
  expect_equal(extracted$value, 2.12)
  expect_equal(extracted$df2, 2)
  expect_equal(extracted$p, "p < 0.001")
  expect_equal(extracted$n, 123)
})

