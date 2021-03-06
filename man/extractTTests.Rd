% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ttest_extractor.R
\name{extractTTests}
\alias{extractTTests}
\title{Extract t tests from text}
\usage{
extractTTests(input)
}
\arguments{
\item{inputText}{input text}
}
\value{
A tibble containing all extracted values, with columns identifying the type of statistic extracted, the reported test as reported in the text, the degrees of freedom, the p value and the value reported.
}
\description{
Extract t tests from text.
}
\examples{
extractTTests("t(123) = .01, p = .001")


}
