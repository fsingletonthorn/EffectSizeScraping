context("ftest_extractor")


test_that("ftest_extractor works as expected", {
    extracted <- extractFTests("F(1, 2) = 12.3, p = 0.123")
    expect_equal(extracted$df1, "1")
    expect_equal(extracted$df2, "2")
    expect_equal(extracted$value, "12.3")
    expect_equal(extracted$p, "p = 0.123")
})
