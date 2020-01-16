context("Correlation coefficent extractor")

# Correlation designators
designatiors <- c(
"correlation",
"correlation coefficient",
"Correlation",
"Correlation coefficient",
"Correlation Coefficient",
"r",
"ρ",
"rho")

# Connectors
connectors <- c(
"=",
"of"
)

# dfs
# Includign some with commas in degrees of freedom
dfs <- c("1", "10", "12789", "12,789", "123,456,789,101", "(1)", "(10)", "(12789)", "(12,789)")

# Correlations with and without leading zeros
rsPos <- c(".5", "0.5", "0", "0.0", "1", "1.0", ".3141593", "0.3141593")
# Negatives too and negtives with spaces
rs <- c(rsPos, paste0("-", rsPos))


# Inclusion of p values
ps <- c("p = .05", "p < .001", "p = 1", "p > .05", 
        "p = 0.05", "p > 0.05", 
        ", p = .05", ", p < .001", ", p = 1", ", p > .05",
        "p = 1e-24", "p < 1e-24", ", p = 1e-24", ", p < 1e-24", 
        "NS", "p = ns", "ns"
        )

# Creating all combinations of the above 
# First without degrees of freedom 
df_cor_test <- expand.grid(designatiors, connectors, rs, stringsAsFactors = F)
cor_test_strings <-  as.character(tidyr::unite(df_cor_test, sep = " ", )[,1])

# with degrees of freedom 
df_cor_test_df <- expand.grid(designatiors[-c(1:5)], dfs,
                           connectors, rs, stringsAsFactors = F)
cor_test_strings_df <- as.character(tidyr::unite(df_cor_test_df, sep = " ", )[,1])
# Makign the df slightly more realistic
cor_test_strings_df <- stringr::str_remove_all(cor_test_strings_df, "\\s(?=\\()")

# Adding in p values
df_cor_test_df_ps <- expand.grid(designatiors[-c(1:5)], dfs,
                           connectors, rs,
                           ps,
                           stringsAsFactors = F)
cor_test_strings_df_ps <- as.character(tidyr::unite(df_cor_test_df_ps, sep = " ", )[,1])

# Makign the commas slightly more realistic
cor_test_strings_df_ps <- stringr::str_remove_all(cor_test_strings_df_ps, "\\s(?=,)")


### Test for all combinations
test_that("Raw text of correlation coefficients are extracted (with df)", {
  expect_equal(
    extractCorrelations(cor_test_strings_df)$raw, 
    cor_test_strings_df
  )
}
)

### Test for all combinations
test_that("Raw text of correlation coefficients are extracted (without df)", {
  expect_equal(
    extractCorrelations(cor_test_strings)$raw, 
    cor_test_strings
  )
  }
)

### Test for all combinations with df a p values 
test_that("Raw text of correlation coefficients are extracted (s with df a p values)", {
  expect_equal(
    extractCorrelations(cor_test_strings_df_ps)$raw, 
    cor_test_strings_df_ps
  )
  
  expect_equal(
    extractCorrelations(cor_test_strings_df_ps)$p,
    stringr::str_remove(df_cor_test_df_ps$Var5, stringr::fixed(", "))
  )  
  
  expect_equal(
    extractCorrelations(cor_test_strings_df_ps)$value,
    stringr::str_remove(df_cor_test_df_ps$Var4, stringr::fixed(", "))
  )  
  
  
  
  expect_equal(
    extractCorrelations(cor_test_strings_df_ps)$df1,
    rep(NA, length(df_cor_test_df_ps$Var1))
  )
}
)


## Assorted old tests
# 
# testR <- c("r(1) = .42345",
#            "r(2)= .1345",
#            "r(3) = 0.4234",
#            "r( 4 ) = 0.45",
#            "r(df = 5) = 1",
#            "r (n=6) = .45",
#            "r = .7",
#            "correlation of .8", 
#            # note the following should not be included in tests (it is removed)
#            "R2 = .12",
#            "correlation of .8, p = .009")
# 
# testRString  <- stringr::str_flatten(testR, collapse = " ")
# 
# test_that("correlation extractor works", {
#   extracted <- extractCorrelations(testRString)
#   expect_identical(extracted[[3]], testR[-9])
#   expect_identical(extracted[[2]],
#                    stringr::str_remove_all(testR, "\\s")[-9])
# })
