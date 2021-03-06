context("ftest_extractor")

test_that("ftest extractor works as expected", {
    extracted <- extractFTests("F(1, 2) = 12.3, p = 0.123")
    expect_equal(extracted$df1, 1)
    expect_equal(extracted$df2, 2)
    expect_equal(extracted$value, 12.3)
    expect_equal(extracted$p, "p = 0.123")
})

test_that("ftest extractor works as expected", {
    extracted <- extractFTests("F(1, 2) = 12.3, p = 0.123")
    expect_equal(extracted$df1, 1)
    expect_equal(extracted$df2, 2)
    expect_equal(extracted$value, 12.3)
    expect_equal(extracted$p, "p = 0.123")
})

testF <- c("F(1, 12345) = 12.42345",
           "F(2,1)=12.42345",
           "F(3, 12345) = 12.42345",
           "F (4, 99) = 12.42345",
           "F ( 5, 1235 ) = 1",
           "F ( 6, 12345 ) = .42345",
           "F ( 7, 1245 ) = 12",
           "F ( 8, 12345 ) = 12,  p = .01",
           "F ( 9, 1345 ) = 12 p = 0.01",
           "F(10, 12345) = 1, p < .01",
           "F(11, 12345) = 12.42345, p < .01",
           "F(11, 12345) = 12.42345",
           "F12, 12345 = 12.42345, p < .01",
           "F13, 12345 = 12.42345, P < .01",
           "F = 12.42345, p < .01",
           "F = 12.42345, P < .01")


testFString  <- stringr::str_flatten(testF, collapse = " ")

test_that("F test extractor works", {
  extracted <- extractFTests(testFString)
  
  testthat::expect_identical(extracted$reported, testF)

  
  expect_equal(extracted$value,
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
                     ifelse(.=="", NA, .))
  )
  
  expect_identical(extracted$df2,
                   as.numeric(
                   stringr::str_remove_all(stringr::str_extract(
                     testF,
                     "(?<=F\\s{0,3}\\(?\\s{0,3}\\d{0,10}\\s{0,3},)\\s*\\d*"
                   ),
                   "\\s*"
                   )
                   )
  )
  
  expect_identical(extracted$p, stringr::str_extract(testF, "(((p|P)\\s{0,5}\\=?\\s{0,5}))(<\\s*)?(>\\s*)?0?\\.\\d*"), "\\s")
})


test_that("Ideosyncratic F tests values are caught", { 
  expect_equal(extractFTests("F1 2 = 999, p = .99")$df1, 1)
  expect_equal(extractFTests("F(99999 2) = 1333, p = .01")$df2, 2)
})
