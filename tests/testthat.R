

Sys.setenv("R_TESTS" = "")
library(lastdose)
library(testthat)
test_check("lastdose", reporter="summary")


