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
           "R2 = .12", # note this should not be included 
           "correlation of 8, p = .009")

testChi <- c("chi square = 12.32",
             "chi2 = 123.32",
             "χ2(123) = 1232.23, p < .05",
             "χ2 = 122.23,p = .13",
             "chi2(123) = 123.2, p < .001",
             "χ2(1, N = 320) = 22.31, p < 0.001")

testTString  <- str_flatten(testT, collapse = " ")
testFString  <- str_flatten(testF, collapse = " ")
testRString  <- str_flatten(testR, collapse = " ")
testChiString  <- str_flatten(testChi, collapse = " ")
return(list(
paste("T",identical(extractTestStats(testTString)[[3]], testT)),
paste("F",identical(extractTestStats(testFString)[[3]], testF)),
paste("R",identical(extractTestStats(testRString)[[3]], testR[-9])),
paste("chi",identical(extractTestStats(testChiString)[[3]], testChi)),

paste("T raw",identical(extractTestStats(testTString)[[2]], str_remove_all(testT, "\\s"))),
paste("F raw",identical(extractTestStats(testFString)[[2]], str_remove_all(testF, "\\s"))),
paste("R raw",identical(extractTestStats(testRString)[[2]], str_remove_all(testR[-9], "\\s"))),
paste("Chi raw",identical(extractTestStats(testChiString)[[2]], str_remove_all(testChi, "\\s"))),

paste("training1", identical(pullAndProcess( articles$oaiCall[ trainingSet ][1] ), read_rds("data/trainingSet/validatedOutput/test1.RDS"))),
paste("training2", identical(pullAndProcess( articles$oaiCall[ trainingSet ][2] ), read_rds("data/trainingSet/validatedOutput/test2.RDS"))),
paste("training3", identical(pullAndProcess( articles$oaiCall[ trainingSet ][3] ), read_rds("data/trainingSet/validatedOutput/test3.RDS")))

))
}

# write_rds( pullAndProcess( articles$oaiCall[ trainingSet ][1] ) , path = "data/trainingSet/validatedOutput/test1.RDS")
# write_rds( pullAndProcess( articles$oaiCall[ trainingSet ][2] ) , path = "data/trainingSet/validatedOutput/test2.RDS")
