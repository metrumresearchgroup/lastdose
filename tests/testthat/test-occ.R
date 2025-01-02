library(testthat)
library(lastdose)


occdata <- function(file) {
  file <- system.file("test-data", "occ", file, package = "lastdose")
  read.csv(file, stringsAsFactors = FALSE)
}

test_that("OCC single dose", {
  data <- occdata("single-dose.csv")
  data <- lastdose(data)
  expect_equal(data$OCC[1], 0)
  expect_true(all(data$OCC[-1] ==1))
})

test_that("OCC single dose, recsort==3", {
  data <- occdata("single-dose-recsort3.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), 1L)
  expect_equal(data$EVID[1], 1L)
})

test_that("OCC multi-dose, explicit", {
  data <- occdata("multi-dose-explicit.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2))
  sp <- split(data, data$OCC)
  # Single observation prior to first dose
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_equal(sp[[2]]$OCC[1], 1)
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(range(sp[[2]]$TIME), c(0,12))
  # Second occasion
  expect_equal(sp[[3]]$OCC[1], 2)
  expect_equal(sp[[3]]$EVID[1], 1)
  expect_equal(range(sp[[3]]$TIME), c(12,24))
})

test_that("OCC multi-dose, addl", {
  data <- occdata("multi-dose-addl.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2))
  sp <- split(data, data$OCC)
  # Single observation prior to first dose
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_equal(sp[[2]]$OCC[1], 1)
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(range(sp[[2]]$TIME), c(0,10))
  # Second occasion
  expect_equal(sp[[3]]$OCC[1], 2)
  expect_equal(sp[[3]]$EVID[1], 0)
  expect_equal(range(sp[[3]]$TIME), c(12,24))
})

test_that("OCC multi-dose, addl dose_first", {
  data <- occdata("multi-dose-addl.csv")
  data1 <- lastdose(data)
  data2 <- lastdose(data, addl_ties = "dose_first")
  expect_identical(data1$OCC, data2$OCC)
  expect_false(all(data1$TAD==data2$TAD))
})

test_that("OCC multi-dose, addl", {
  data <- occdata("multi-dose-addl-no-obs.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1))
  sp <- split(data, data$OCC)
  # The first dose is not actually an OCC
  expect_equal(sp[[1]]$OCC, c(0,0))
  expect_equal(sp[[1]]$AMT, c(0,100))
  # First and only occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 0)
})

test_that("OCC multi-dose, explicit", {
  data <- occdata("multi-dose-explicit-no-obs.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1))
  sp <- split(data, data$OCC)
  # The first several doses are not actually an OCCs
  expect_equal(sp[[1]]$OCC, c(0,0,0,0))
  expect_equal(sp[[1]]$AMT, c(0,100,100,100))
  # First and only occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 1)
})

test_that("OCC handle EVID 2 and 3", {
  # Check EVID 2
  data <- occdata("evid-2-3-a.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2))
  sp <- split(data, data$OCC)
  # Pre-dose observation
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(sp[[2]]$EVID[2], 0)
  expect_true(all(sp[[2]]$EVID[c(3,4,5)] > 0))
  # Second occasion
  expect_true(all(sp[[3]]$OCC==2))
  expect_equal(sp[[3]]$EVID[1], 1)
  expect_equal(sp[[3]]$EVID[2], 0)

  # Check EVID 3
  data <- occdata("evid-2-3-b.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2))
  sp <- split(data, data$OCC)
  # Pre-dose observation
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(sp[[2]]$EVID[2], 0)
  expect_true(all(sp[[2]]$EVID[c(3,4,5)] > 0))
  # Second occasion
  expect_true(all(sp[[3]]$OCC==2))
  expect_equal(sp[[3]]$EVID[1], 1)
  expect_equal(sp[[3]]$EVID[2], 0)

  # Verify that EVID 0 triggers new OCC
  data <- occdata("evid-2-3-c.csv")
  data <- lastdose(data)
  expect_equal(unique(data$OCC), c(0,1,2,3))
  sp <- split(data, data$OCC)
  # Pre-dose observation
  expect_equal(sp[[1]]$OCC, 0)
  # First occasion
  expect_true(all(sp[[2]]$OCC==1))
  expect_equal(sp[[2]]$EVID[1], 1)
  expect_equal(sp[[2]]$EVID[2], 0)
  # Second occasion
  expect_true(all(sp[[3]]$OCC==2))
  expect_equal(sp[[3]]$EVID[1], 1)
  expect_equal(sp[[3]]$EVID[2], 3)
  expect_equal(sp[[3]]$EVID[3], 0)
  # Third occasion
  expect_true(all(sp[[4]]$OCC==3))
  expect_equal(sp[[4]]$EVID[1], 1)
  expect_equal(sp[[4]]$EVID[2], 0)
})

