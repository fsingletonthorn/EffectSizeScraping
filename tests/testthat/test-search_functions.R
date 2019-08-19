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
expect_identical(findN("Twenty Princeton University undergraduate took part")[[2]][[1]], "20")
expect_identical(findN("40 Princeton University students took part.")[[2]][[1]], "40")
expect_identical(findN("Fifty-one Hebrew University undergraduates participated in the experiment in exchange for course credit or pay.")[[2]][[1]], "51")
expect_identical(findN("Ninety-three Hebrew University undergraduates participated in exchange for course credit or pay. 
                       In experiment 2, 23 Princeton University volunteers at the student campus center read a short review of a new MP3 player,
                       Experiment 3: 94 participants read the paper. ")$N, c("93", "23", "94"))
})

test_that("false positive sample sizes are avoided as much as is possible", {
  expect_true(is.na(findN("Looked at 40 Princeton University students' photos. None took part.")[[1]][1]))
}
)

# Does not work: 
# expect_identical(findN("10 princeton undergraduate students reported")[[2]][[1]], "10")

# Test on real papers
test_that("the correct number is extracted", {
  pdf <- extractPdf("https://osf.io/v7n6j/download")
  expect_true(
    max( as.numeric(
      extractNsFromProcessed( pdf )$n )) == 510
    )
  
  expect_equal(
    findN("participants in some conditions were, indeed, more familiar with the concept than those in other conditions, this could confound results. However, it is likely that greater familiarity would lead to greater ac- ceptance (Bryant & Barnett, 2018), and in this instance, the reverse was true: those claiming to be more familiar in the ‘lab grown meat’ and ‘cultured meat’ conditions actually also showed lower measures of ac- ceptance in subsequent analyses. Therefore, we are confident that this Appetite 137 (2019) 104–113 Items Eating [X] is likely to be healthy. [X] is likely to look, taste, smell, and feel the same as conventional meat. I think I could tell the difference between [X] and conventional meat. [X] is likely to contain chemicals or ingredients which should be avoided. [X] is likely to be safe for human consumption. I would trust [X]. [X] is unnatural. [X] is appealing to me. I feel positive about the development of [X]. The idea of [X] is disgusting. I feel comfortable about the idea of eating [X]. I would be anxious about eating [X]. Eating [X] would conflict with my values. I feel that I would have control over my decision to eat [X] or not. The production of [X] is a necessary scientific development. Others would disapprove of me eating [X]. [X] will have benefits for our society. Production of [X] is wise. Production of [X] is necessary. [X] is more environmentally friendly than conventional meat. Producing [X] poses a risk to society. I would be willing to try [X]. I would buy [X] regularly. I would eat [X] instead of conventional meat. I would rather eat [X] than soy-based meat substitutes or Quorn. I would pay more for [X] than for conventional meat. difference is a result of how familiar the names seem rather than how familiar the participants actually were. Familiarity was therefore not included as a covariate in subsequent analyses. 3.2. Word associations Before a description of IVM had been given, participants completed a word association task. They generated a total 721 words or phrases – where 338 of them were unique - an average of 3.90 per participant. They also rated the valence of each word or phrase they generated. Words were sorted into categories. Initial categories were identified, partly informed by themes observed in the literature on consumer ac- ceptance of IVM. After consultation, these categories were adjusted and some words were reclassified. Next, 3 independent raters allocated the words to categories with an initial agreement rate of 67%, which increased to 97% after further discussion with 1 rater. The remaining 3% of ambiguous words were categorised after further consultation between the co-authors. Words were ultimately placed into 24")$N,
    character(0)
    )
  
  pdf <- extractPdf("https://osf.io/nztsx/download")
  
  expect_true(
    max( as.numeric(
      extractNsFromProcessed( pdf )$n)) == 185
    )
})


test_that("CI extractor works I", {
   ciTexts <- list("Confidence interval", 
                   "as fsad f 95% CI 10, 20 fasd", 
                   "95% CI around the latest [1,10]", 
                   "95$ confidence range", 
                   "not very good at confidence ratings")
   testthat::expect_equivalent(checkCIs(ciTexts, context = T, contextSize = 100)$context[c(1,2)], as.character(ciTexts[1:2]))
   testthat::expect_equivalent(unlist(lapply(ciTexts, checkCIs, context = F)), c(T,T,T,F,F))
 })

test_that("CI extractor works II", {
   ciTexts <- list("CI = [1, 2]", 
                   "f 95% CI 10, 20 fa", 
                   "95% CI around the latest [1,10]", 
                   "23% confidence range of 1 to 5", 
                   "70% CI of 1, 2",
                   "CI = 1, 2")
   testthat::expect_equivalent(checkCIs(ciTexts, context = T, contextSize = 30)$context,
                               as.character(c(ciTexts[1:3], "", ciTexts[5], "")))
   testthat::expect_equivalent(unlist(lapply(ciTexts, checkCIs, context = F)), c(T,T,T,F,T,F))
 })



