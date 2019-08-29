

Sys.setenv("R_TESTS" = "")
library(testthat)
library(Rcpp)
library(lastdose)
test_check("lastdose", reporter="summary")


