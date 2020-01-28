context("effectsize_extractor")

test_that("effectsize_extractor works as expected", {
  test_string <- "eta squared = 0, p < .01, d of 1, p = < .001 or HR = 2 p < .05, or OR = 3, or a hazard ratio of 4"
  output <- extractES(test_string)
  expect_true(all(0:4 %in% output$value))
  expect_true(sum(is.na(output$p)) == 2)
  expect_true(all(is.na(output$df1)))
  expect_true(all(is.na(output$df2)))
  expect_true(output$p[output$statistic == "d"] == "p = < .001") 
  expect_true(output$p[output$statistic == "eta"] == "p < .01") 
  expect_true(output$p[output$statistic == "OR" & stringr::str_detect(output$reported, "HR")] == "p < .05") 
  })

test_that("effectsize_extractor works with eta squared tests that are reported as \\eta2", {
  expect_equal(extractES("eta2 = .23,p = .13")$value, .23)
  })
