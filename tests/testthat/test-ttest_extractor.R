context("t test extractor")

test_that("t test works for simple cases" , {

  expect_true(
extractTTests("t(22) = 2.202")$df2 == 22  
  )
expect_true(
extractTTests("t(22) = 2.202")$value == 2.202
)

expect_true(
extractTTests("t(22) = 2.202, p < .05")$p == "p < .05"
)

})


test_that("t test works for slightly aborant cases" , {

expect_true(
extractTTests("t = 2.202, df = 22")$df2 == 22  
)
  
expect_true(
extractTTests("t = 2.202, df = 22")$value == 2.202
)

expect_true(
extractTTests("t = 2.202, df = 22, p < .05")$p == "p < .05"
)

})


testT <- c("t(1) = 12.42345",
           "t(2)=12.42345",
           "t(3) = 12.42345",
           "t(4) = 12.42345",
           "t(5 ) = 1",
           "t(6 ) = .42345",
           "t(7 ) = 12",
           "t(8) = -12.42345",
           "t(9)=-12.42345",
           "t(10) = -12.42345",
           "t(11) = -12.42345",
           "t(12 ) = -1",
           "t(13 ) = -.42345",
           "t(14 ) = -12",
           "t(15) = - 12.42345",
           "t(16)=- 12.42345",
           "t(17) = - 12.42345, p = .123",
           "t(18) = - 12.42345",
           "t(19 ) = - 1",
           "t(20 ) = - .42345, p = 0.00121",
           "t(21 ) = - 12",
           "t(22) = 12.42345",
           "t (23)=12.42345",
           "t (24) = 12.42345",
           "t (25) = 12.42345",
           "t (26 ) = 1",
           "t (27 ) = .42345",
           "t (28 ) = 12",
           "t (29 ) = 12.0, p = .99",
           "t (30 ) = 12.0, P = .99",
           "t = 12.0, df = 31, P < 1e-07",
           "t = 12.0, df = 32, P > .01",
           "t = 12.0, df = 33,  p > .05",
           "t = 12.0, p = .99",
           "t = 12.0, P = 0.99",
           "t = 12.0, P = 0.01",
           "t = 12.0, P = 0.001",
           "t = 12.0, P < 1e-07",
           "t = 12.0, P > .01"
           )

testTString  <- stringr::str_flatten(testT, collapse = " ")


test_that("t test extractor works", {
  extracted <- extractTTests(testTString)
  
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
  
  expect_equal(extracted$df2[1:33], 1:33
  )  
  
  
  expect_identical(extracted$p,
                     stringr::str_extract(
                       testT,
                       "(p|P).*"
                     )
                   )
  })

test_that("t test doesn't pick up tests without reported values", {
  
  expect_equal(
    dim(extractTTests("t(1), p = .8, and t(c) = .12")), c(0, 6))
  })

test_that("t test works with bad minus characters as long as text has been cleaned", {
  extractTTests(cleanText("[t(10) = –0.90, p = 0.39]"))
  })
