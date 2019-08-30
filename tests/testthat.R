
Sys.setenv("R_TESTS" = "")

if(require("testthat") & require("Rcpp")) {
  library(lastdose)
  test_check("lastdose", reporter="summary")
}

