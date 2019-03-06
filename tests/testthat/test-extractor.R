context("test-extractor")
library(stringr)
library(base)
library(dplyr)
library(tibble)

# test sets
testF <- c("F(1, 12345) = 12.42345",
           "F(1,12345)=12.42345",
           "F(1, 12345) = 12.42345",
           "F (1, 12345) = 12.42345",
           "F ( 1, 12345 ) = 1",
           "F ( 1, 12345 ) = .42345",
           "F ( 1, 12345 ) = 12.",
           "F ( 1, 12345 ) = 12.,  p = .01",
           "F ( 1, 12345 ) = 12 p = 1.01",
           "F(1, 12345) = 12.42345, p < .01",
           "F1, 12345 = 12.42345, p < .01")

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
           "t(15) = - 12.42345",
           "t(145) = - 12.42345",
           "t(1345 ) = - 1",
           "t(1345 ) = - .42345",
           "t(1345 ) = - 12",
           "t(15) = 12.42345",
           "t (1)=12.42345",
           "t (15) = 12.42345",
           "t (145) = 12.42345",
           "t (1345 ) = 1",
           "t (1345 ) = .42345",
           "t (1345 ) = 12.",
           "t (1345 ) = 12.0, p = .99")

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
  expect_identical(extractTestStats(testTString)[[3]], testT)
  expect_identical(extractTestStats(testTString)[[2]],
                     str_remove_all(testT, "\\s"))
})
test_that("F test extractor works", {
  expect_identical(extractTestStats(testFString)[[3]], testF)
  testthat::expect_identical(extractTestStats(testFString)[[2]],
                     stringr::str_remove_all(testF, "\\s"))
})
test_that("correlational test extractor works", {
  expect_identical(extractTestStats(testRString)[[3]], testR[-9])
  expect_identical(extractTestStats(testRString)[[2]],
                     str_remove_all(testR, "\\s")[-9])
})

test_that("chi test extractor works", {
  expect_identical(extractTestStats(testChiString)[[3]], testChi)
  expect_identical(extractTestStats(testChiString)[[2]],
                     str_remove_all(testChi, "\\s"))
})


test_that("eta squared extractor works", {
  expect_identical(extractTestStats(testEtaString)[[3]], testEta)
  expect_identical(extractTestStats(testEtaString)[[2]],
                   str_remove_all(testEta, "\\s"))
})

test_that("cohen's d extractor works", {
  expect_identical(extractTestStats(testDString)[[3]], testD)
  expect_identical(extractTestStats(testDString)[[2]],
                   str_remove_all(testD, "\\s"))
}
)
