library(testthat)
library(lastdose)

context("basic functionality")
set_file <- system.file("csv", "setn.csv", package = "lastdose")
df <- read.csv(set_file)
df$TIME <- df$time
df$time <- NULL
set1 <- subset(df, set==1)
set2 <- subset(df, set==2 & ID==1 & TIME <= 12)


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
  ax <- subset(x, TIME==12)
  az <- subset(z, TIME==12)
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

test_that("lastdose_list", {
  y <- lastdose_list(set1)
  expect_is(y,"list")
  expect_identical(names(y), c("tad", "ldos"))
})

test_that("required columns", {
  x <- set1
  x[["amt"]] <- NULL
  expect_error(lastdose(x))
  x <- set1
  x[["time"]] <- NULL
  x[["TIME"]] <- NULL
  expect_error(lastdose(x))
  x <- set1
  x[["ID"]] <- NULL
  x[["id"]] <- NULL
  expect_error(lastdose(x))
  x <- set1
  x[["evid"]] <- NULL
  expect_error(lastdose(x))
})

test_that("non-numeric data throws error", {
  for(col in c("ID","time", "addl", "ii", "evid", "amt")) {
    dd <- set1[seq(10), ]
    dd$ID <- NULL
    dd$TIME <- NULL
    dd <- dd[seq(10),]
    dd[[col]] <- "A"
    expect_error(lastdose(dd))
  }
})

test_that("records out of order throws error", {
  set1$TIME[12] <- 1E6
  expect_error(lastdose(set1))
})

test_that("tad and ldos are NA when time is NA", {
  set1$TIME[12] <- NA_real_
  ans <- lastdose(set1)[12,]
  expect_true(is.na(ans[["TAD"]]))
  expect_true(is.na(ans[["LDOS"]]))
  expect_true(is.na(ans[["TIME"]]))
})

test_that("error for missing values in ID,evid,ii,addl", {
  for(col in c("ID", "evid", "ii", "addl")) {
    dd <- set1[seq(10),]
    dd[[col]] <- NA_real_
    expect_error(lastdose(dd))
  }
})

test_that("NA amt is error for dosing record, ok otherwise", {
  dd <- set1
  dd$amt[5] <- NA_real_
  expect_is(lastdose(dd), "data.frame")
  dd <- set1
  dd$amt[10] <- NA_real_
  expect_error(lastdose(dd))
})

test_that("commented records", {
  com <- c(".", NA ,"C", "A","Comment")
  ans <- find_comments(com)
  expect_identical(ans, c(FALSE,FALSE,TRUE,TRUE,TRUE))
  df <- data.frame(C = com, DV=stats::rnorm(length(com)))
  ans2 <- find_comments(com)
  expect_identical(ans,ans2)
  df2 <- df
  df2[["C"]] <- sample(c(0,1),nrow(df),replace=TRUE)
  expect_warning(find_comments(df2))
  set1[["TIME"]] <- ifelse(set1[["ID"]]==2, set1[["TIME"]] + 25, set1[["TIME"]])
  set1[["C"]] <- NA_character_
  set1[["ID"]] <- 1
  set1[["ii"]] <- 0
  set1[["addl"]] <- 0
  set1[["C"]][10] <- "C"
  ans <- lastdose(set1)
  diff <- ans[["TIME"]] - ans[["TAD"]]
  expect_equal(sum(diff),0)
  expect_error(lastdose(set1, comments = c(FALSE, TRUE,FALSE)))
})

test_that("undefined behavior when checking ADDL and II issue-11", {
  no_addl <- subset(set1, ID==1)
  no_addl[["addl"]] <- NULL
  no_addl[["ii"]] <- NULL
  ans <- lapply(1:10, function(i) lastdose(data = no_addl))
  ans <- sapply(ans,inherits,what="data.frame")
  expect_true(all(ans))
})

test_that("user time and id columns", {
  d1 <- subset(set1, ID==1)
  d2 <- d1
  d2$TAFD <- d2$TIME
  d2$TIME <- NULL
  expect_identical(
    lastdose_df(d1),
    lastdose_df(d2, time_col = "TAFD")
  )
  expect_error(lastdose(d2), msg = "did not find time column")
  d2 <- d1
  d2$USUBJID <- "A"
  d2$ID <- NULL
  expect_identical(
    lastdose_df(d1),
    lastdose_df(d2, id_col = "USUBJID")
  )
  expect_error(lastdose(d2))
})

test_that("POSIXct datetime is converted to numeric time", {
  d1 <- subset(set1, ID <= 2)
  d2 <- d1
  base <- as.POSIXct(0, origin = "2020-01-01", tz = "UTC")
  d1$TIME <- d1$TIME * 60 * 60 + base
  expect_identical(
    lastdose_df(d2),
    lastdose_df(d1, time_units = "hours")
  )
  expect_error(lastdose(d1), msg="is required when time column is")
  expect_error(
    lastdose(d1, time_units = "seconds")
  )
})

