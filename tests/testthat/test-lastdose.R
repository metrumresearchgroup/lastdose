

context("basic functionality")
set_file <- system.file("csv", "setn.csv", package = "lastdose")
df <- read.csv(set_file)
set1 <- subset(df, set==1)
set2 <- subset(df, set==2 & ID==1 & time <= 12)


test_that("doses at time zero", {
  x <- lastdose(set1)
  expect_true(exists("TAD", x))
  expect_true(exists("LDOS", x))
  expect_identical(unique(x[["LDOS"]]), c(0,100))
  a <- c(0,0,4,8,12,4,8,12)
  expect_identical(x[["TAD"]],c(a,a))
  a <- c(0,rep(100,7))
  expect_identical(x[["LDOS"]],c(a,a))
})

test_that("time ties (q12h dosing)", {
  x <- lastdose(set1)
  z <- lastdose(set1, addl_ties = "dose_first")
  expect_false(identical(x,z))
  ax <- subset(x, time==12)
  az <- subset(z, time==12)
  expect_true(all(ax[["TAD"]] == 12))
  expect_true(all(az[["TAD"]] == 0))
})

test_that("don't fill back", {
  x <- lastdose(set1, back_calc = FALSE)
  a <- c(-99,0,4,8,12,4,8,12)
  expect_identical(x[["TAD"]],c(a,a))
})

test_that("customize fill", {
  x <- lastdose(set1, back_calc = FALSE, fill = NA_real_)
  a <- c(NA_real_,0,4,8,12,4,8,12)
  expect_identical(x[["TAD"]],c(a,a))
})

test_that("doses don't start at time zero", {
  x <- lastdose(set2)
  a <- c(seq(-6,0),seq(0,6))
  expect_identical(x[["TAD"]],as.double(a))
})

test_that("lastdose_df", {
  x <- lastdose_df(set1)
  y <- lastdose_list(set1)
  expect_identical(x[["tad"]], y[["tad"]])
})

test_that("required columns", {
  x <- set1
  x[["amt"]] <- NULL
  expect_error(lastdose(x))
  x <- set1
  x[["time"]] <- NULL
  expect_error(lastdose(x))
  x <- set1
  x[["ID"]] <- NULL
  expect_error(lastdose(x))
  x <- set1
  x[["evid"]] <- NULL
  expect_error(lastdose(x))
})


