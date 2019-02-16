# tests of function 
# library(tidyverse)

tests <- function() { 
testF <- c("F(1, 12345) = 12.42345",
           "F(1,12345)=12.42345",
           "F(1, 12345) = 12.42345",
           "F (1, 12345) = 12.42345",
           "F ( 1, 12345 ) = 1",
           "F ( 1, 12345 ) = .42345",
           "F ( 1, 12345 ) = 12.",
           "F ( 1, 12345 ) = 12.,  p = .01",
           "F ( 1, 12345 ) = 12 p = 1.01",
           "F(1, 12345) = 12.42345, p < .01")


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
           "R2 = .12",
           "correlation of 8, p = .009")

exampleString <- "table [45].Table 4Concurrent validity: t(15) = - 12.42345 Pearson’s correlations of MBI and Professional Fulfillment Index (PFI) scales with medical errors, sleep-related impairment, depression, anxiety, and World Health Organization Brief Quality of Life (QoL) survey domainsPFI measureMedical errorsSleep-related impairmentDepressionAnxietyPhysical QoLPsychological QoLSocial QoLEnvironmental QoLWork exhaustion0.150.580.580.57− 0.55− 0.60− 0.32− 0.42Interpers F ( 1, 12345 ) = 1 onal disengagement0.330.550.390.42F ( 1, 12345 ) = 1, 0.42− 0.44− 0.28− 0.37Overall bu  rnout scale0.280.610.530.53− 0.52− 0.55− 0.32− 0.43Professional fulfillment− 0.09*− 0.39− 0.490.460.430.570.320.40MBI emotional exhaustion0.230.590.560.59− 0.57− 0.63− 0.32− 0.47MBI depersonalization0.430.410.310.37− 0.30− 0.37− 0.25− 0.38MBI personal accomplishment− 0.06*− 0.27− 0.30− 0.330.330.410.200.38MBI Maslach Burnout Inventory*p ≥ 0.05. All other correlations are stat F( 1, 13123345 ) = 1 istically significant (p < 0.05)\nMBI emotional exhaustion, PFI work exhaustion and PFI overall burnout (average score across all PFI burnout items) all had small (> 0.1 < 0.3) but statistically significant correlations with self-reported medical errors. Neither MBI’s personal accomplishment nor PFI professional fulfillment correlated significantly with self-reported medical errors. All MBI and PFI scales correlated moderately or highly in expected directions with PROMIS sleep-related impairment, depression symptom, and anxiety symptom scales, with the exception of the correlation between MBI personal accomplishment and sleep-related impairment, which was − 0.27 (Table 4). Figure 1 demonstrates the dose-response effect on medical errors, sleep-related impairment, depression, and anxiety of PFI burnout scores by quartile. All correlations between MBI and PFI scales with WHOQOL-BREF physical, psychological, social, and environmental domain quality of life scores were moderate to high, with the exception of smaller but statistically significant correlations between social quality of life scores and PFI interpersonal disengagement, MBI depersonalization, and MBI personal accomplishment scores (Table 4).Fig. 1"

testTString  <- str_flatten(testT, collapse = " ")
testFString  <- str_flatten(testF, collapse = " ")
testRString  <- str_flatten(testR, collapse = " ")


return(list(
paste("T",identical(extractTestStats(testTString)[[3]], testT)),
paste("F",identical(extractTestStats(testFString)[[3]], testF)),
paste("R",identical(extractTestStats(testRString)[[3]], testR[-9])),

paste("T raw",identical(extractTestStats(testTString)[[2]], str_remove_all(testT, "\\s"))),
paste("F raw",identical(extractTestStats(testFString)[[2]], str_remove_all(testF, "\\s"))),
paste("R raw",identical(extractTestStats(testRString)[[2]], str_remove_all(testR[-9], "\\s")))
))
}
str_extract_all(testRstring, patternR)
str_extract_all(testTString, patternT)
