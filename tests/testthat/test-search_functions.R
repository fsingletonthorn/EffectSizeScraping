context("Test search_functions")

expect_true(
  checkCIs("95% CI [1,2]")[[3]] &
  checkCIs("95% CI 1.43–7.89")[[3]] &
  checkCIs("95% confidence Interval of [999,2]")[[3]] &
  checkCIs("95% confidence interval [1,2]")[[3]] &
  checkCIs("95% confidence Interval of 999,2")[[3]]
)

expect_false(
  checkCIs("I am not confident")[[3]] |
    checkCIs("95% con man")[[3]] |
    checkCIs("Confidence level of 95%]")[[3]]
)

### Test CI detector on real papers

test_that("sample size extraction works", {
expect_identical(findN("N=185")[[2]][[1]], "185")
expect_identical(findN("N = 185")[[2]][[1]], "185")
expect_identical(findN("N of 185")[[2]][[1]], "185")
expect_identical(findN("sample size of 185")[[2]][[1]], "185")
expect_identical(findN("participants were 543 individuals")[[2]][[1]], "543")
expect_identical(findN("A total of 543 participants responded to the drinking questions and returned a questionnaire at all three waves (51.2% women).")[[2]][[1]], "543")
expect_identical(findN("A total of 319 participants (Male = 121, Female = 193, Did Not Indicate = 5) were selected from an introductory psychology course.")[[2]][[1]], "319")
expect_identical(findN("We recruited 40 Princeton University undergraduate volunteers at the student campus center to complete the three-item CRT (Frederick, 2005).")[[2]][[1]], "40")
expect_identical(findN("We recruited a total of 40 Princeton University undergraduate volunteers at the student campus center to complete the three-item CRT (Frederick, 2005).")[[2]][[1]], "40")
expect_identical(findN("A separate sample of 42 participants")[[2]][[1]], "42")
expect_identical(findN("Twenty Princeton University undergraduate volunteers at the campus student")[[2]][[1]], "20")
expect_identical(findN("Forty Princeton University volunteers at the student campus center read a short review of a new MP3 player,")[[2]][[1]], "40")
expect_identical(findN("Fifty-one Hebrew University undergraduates participated in the experiment in exchange for course credit or pay.")[[2]][[1]], "51")
expect_identical(findN("Ninety-three Hebrew University undergraduates participated in exchange for course credit or pay.")[[2]][[1]], "93")
})

# Does not work: 
# expect_identical(findN("10 princeton undergraduate students reported")[[2]][[1]], "10")
