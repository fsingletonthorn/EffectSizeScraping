
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![codecov.io](https://codecov.io/gh/fsingletonthorn/EffectSizeScraping/branch/master/graph/badge.svg)](https://codecov.io/gh/fsingletonthorn/EffectSizeScraping?branch=master)

# `ESEXtractor::`

This is a package for the automated extraction of test statistics,
effect sizes and reporting practices from scientific articles. It has
specialized functions for downloading and processing articles from the
PubMed Central open access subset, along with meta-data and bibliometric
information.

## Installation

You can install `ESExtractor::` from github with:

``` r
# install.packages("devtools")
devtools::install_github("fsingletonthorn/EffectSizeScraping")
```

### Usage

#### `extractTestStats()`

This package’s main function is `extractTestStats()`.
`extractTestStats()` is designed to extract test statistics, degrees of
freedom and p values for reported F tests, Chi Square Tests, t tests,
correlational tests, as well as standardised effect sizes reported as
Cohen’s d values, eta squared values, odds ratios, hazard ratios, and
correlations.

    extractTestStats("A cohen's d of .1, F(1, 123) = 12.2, p < .05, t(122) = 2.99, p = .049, χ2(1, N = 320) = 22.31, p < 0.001")

| statistic | reported                           | df1 | df2 | p          | value |   n |
| :-------- | :--------------------------------- | --: | --: | :--------- | ----: | --: |
| t         | t(122) = 2.99, p = .049            |  NA | 122 | p = .049   |  2.99 |  NA |
| chi       | X2(1, N = 320) = 22.31, p \< 0.001 |  NA |   1 | p \< 0.001 | 22.31 | 320 |
| F         | F(1, 123) = 12.2, p \< .05         |   1 | 123 | p \< .05   | 12.20 |  NA |
| d         | d of .1, p = .43                   |  NA |  NA | p = .43    |  0.10 |  NA |

If you want to additionally extract the context around each reported
result, you can specify “context = TRUE” and the number of characters of
context you wish to extract on either side (e.g., “contextSize = 100”,
the default is 300 characters).

###### Example

    extractTestStats("Across all 200 sessions, the hit rate was in the predicted direction but not significantly different from chance, 49.1%, t(199) = 1.31, p = .096, d = 0.09. (I now wish I had simply continued to use subliminal exposures.) Nevertheless, stimulus seeking was again positively correlated with psi performance (lower hit rates).", context = TRUE, contextSize = 100)

| statistic | reported                | df1 | df2 | p        | value |  n | context                                                                                                                                                                                                                         |
| :-------- | :---------------------- | --: | --: | :------- | ----: | -: | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| t         | t(199) = 1.31, p = .096 |  NA | 199 | p = .096 |  1.31 | NA | ns, the hit rate was in the predicted direction but not significantly different from chance, 49.1%, t(199) = 1.31, p = .096. (I now wish I had simply continued to use subliminal exposures.) Nevertheless, stimulus seeking wa |

#### Specific extractor functions

If you want to extract only one type of statistical test or effect
sizes, you can use the specific extractor functions
(`extractChiSquare()` for Chi Square tests, `extractTTests()` for *t*
tests, `extractFTests()` for F tests, `extractCorrelations()` for
correlations and correlational tests, and `extractES()` for odds ratios,
hazard ratios, Eta squared values and Cohen’s d values). The
functionality to extract context is not available when using the
specific extractor functions.

###### Example

    extractTTests("Across all 200 sessions, the hit rate was in the predicted direction but not significantly different from chance, 49.1%, t = 1.31, p = .096, d = 0.09, F(1,232)=12.3, p < .001 (I now wish I had simply continued to use subliminal exposures.) Nevertheless, stimulus seeking was again positively correlated with psi performance (lower hit rates).")

| statistic | reported           | df1 | df2 | p        | value |
| :-------- | :----------------- | :-- | --: | :------- | ----: |
| t         | t = 1.31, p = .096 | NA  |  NA | p = .096 |  1.31 |

#### pullPMC

This package also includes a function to extract text from articles on
the PubMed Central Open Access Subset. To use this function, pass a
PubMed Central Open Access Subset OAI-PMH service full XML text URL to
the pullPMC function (see
<https://www.ncbi.nlm.nih.gov/pmc/tools/openftlist/> for information on
this service and <https://www.ncbi.nlm.nih.gov/pmc/tools/oai/> for
information on how to request the full text in XML format). This
function returns a list containing “metadata”, any keywords associated
with the file, a list of authors, and the full text of the article
separated into the sections as labelled in the XML file (or, if any text
was not labelled, marked as “unlabeled”).

All files are presented in tidydata format, keyed using the PMCID, the
unique identifier used by the PubMed Central database. The metadata
returned includes the PMCID, DOI, the journal name, an abbreviated
journal name, the title of the record, the record’s issue number, the
volume of the journal the article was included in, the print publication
date, the electronic publication date, and the call used to request the
XML file.

###### Example

``` r

PMC_results <- pullPMC("https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3659440&metadataPrefix=pmc")
```

    PMC_results$metadata

| PMCID      | doi                 | journalID                            | journalIDAbrev    | title                                                                           | issue | volume | pPub       | ePub       | call                                                                                                                          |
| :--------- | :------------------ | :----------------------------------- | :---------------- | :------------------------------------------------------------------------------ | :---- | :----- | :--------- | :--------- | :---------------------------------------------------------------------------------------------------------------------------- |
| PMC3659440 | 10.1155/2013/649875 | Rehabilitation Research and Practice | Rehabil Res Pract | Healing Pathways: A Program for Women with Physical Disabilities and Depression | NA    | 2013   | 2013-01-01 | 2013-05-02 | <https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:3659440&metadataPrefix=pmc> |

#### Experimental features

There are several undocumented experimental features. If you want to use
these functions please be aware that they have not been extensively
tested.

##### Sample size extractor

Using the package
[`words_to_numbers`](https://github.com/fsingletonthorn/words_to_numbers)
`findN` converts all numbers written as words to numerics and looks for
phrases that report the sample sizes (e.g., “We included nine hundred
and twelve participants” or “12 student participants”, “N = 12”,
“participants were one hundred and 12 student volunteers”).

###### Example

    findN("We included nine hundred and twelve participants")

| string           | N   |
| :--------------- | :-- |
| 912 participants | 912 |

    findN("N = 12")

| string | N  |
| :----- | :- |
| N = 12 | 12 |

    findN("participants were one hundred and 12 student vulunteers")

| string                | N   |
| :-------------------- | :-- |
| participants were 112 | 112 |

##### CI extractor

`checkCIs()` checks input text for reported confidence intervals.
`checkCIs()` accepts either a fully written out “confidence interval” as
a hit, or “\[digits\]% CI”, or “CI” followed by just two numbers
separated by commas in parentheses “(digit, digit)” or square brackets
“\[digit, digit\]” (ignores white-space). Context is extracted if the
argument “`context`” is set to TRUE, and the number of characters
extracted is set using the argument “`contextSize`”.

    checkCIs("the world is round, p < .05, d = 1.1, 95% CI [.9, 1.2], although some people may not accept this fact", context = TRUE, contextSize = 25)

| CIs    | context                                                     | CIBinary |
| :----- | :---------------------------------------------------------- | :------- |
| 95% CI | round, p \< .05, d = 1.1, 95% CI \[.9, 1.2\], although some | TRUE     |

    checkCIs("The effect was 95% effective")

| CIs | context | CIBinary |
| :-- | :------ | :------- |
| NA  | NA      | FALSE    |
