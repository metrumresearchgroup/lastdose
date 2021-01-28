library(testthat)
library(lastdose)

context("basic functionality")
set_file <- system.file("csv", "setn.csv", package = "lastdose")
df <- read.csv(set_file)
df$TIME <- df$time
df$time <- NULL
set1 <- subset(df, set==1)
set2 <- subset(df, set==2 & ID==1 & TIME <= 12)
set4 <- subset(df, set==4 & ID==1 & TIME <= 24)


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

test_that("time after first dose", {
  x <- lastdose(set4)
  expect_true(exists("TAFD", x))
  dose_rows <- which(set4$evid==1)
  time_of_first_dose <- set4$TIME[dose_rows[1]]
  expect_equal(x$TIME, x$TAFD + time_of_first_dose)
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
  expect_identical(names(y), c("tad", "tafd","ldos"))
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

test_that("user-named time and id columns", {
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

test_that("logical comment column is ok", {
  d <- subset(set1, ID ==1)
  d$C <- sample(c(FALSE, TRUE), nrow(d), replace = TRUE)
  expect_silent(lastdose(d))
  d$C <- sample(c(1, 2), nrow(d), replace = TRUE)
  expect_warning(lastdose(d), msg = "but it wasn't character or logical")
})

test_that("ii detection issue-21", {
  data <- data.frame(
    TIME = c(0,1,2,3,4,5,6,7,8),
    AMT  = c(0,1,0,0,0,0,0,0,0),
    EVID = c(0,1,0,0,0,0,0,0,0),
    II   = c(0,2,0,0,0,0,0,0,0),
    ADDL = c(0,2,0,0,0,0,0,0,0),
    ID = 1
  )
  out <- lastdose(data, addl_ties = "dose_first")
  expect_true(all(out$LDOS[-1]==1))
  expect_equal(out$TAD[1],-1)
  doses <- subset(out, TIME %in% c(1,3,5))
  expect_true(all(doses$TAD==0))
  ones <- subset(out, TIME %in% c(2,4,6))
  expect_true(all(ones$TAD==1))
  term <- subset(out, TIME >=5)
  expect_equal(term$TAD, c(0,1,2,3))
  data2 <- data
  II <- data2$II
  data2$II <- NULL
  data2$ii <- II
  out <- lastdose(data2, addl_ties = "dose_first")
  expect_true(all(out$LDOS[-1]==1))
  expect_equal(out$TAD[1],-1)
  doses <- subset(out, TIME %in% c(1,3,5))
  expect_true(all(doses$TAD==0))
  ones <- subset(out, TIME %in% c(2,4,6))
  expect_true(all(ones$TAD==1))
  term <- subset(out, TIME >=5)
  expect_equal(term$TAD, c(0,1,2,3))
})

test_that("error if ADDL requested by II le 0", {
  data <- data.frame(
    TIME = c(0,1,2,3),
    AMT  = c(0,1,0,0),
    EVID = c(0,1,0,0),
    ADDL = c(0,2,0,0),
    II   = 0,
    ID = 1
  )
  expect_error(
    lastdose(data),
    msg = "ADDL doses requested, but II not positive at row 2"
  )
})
