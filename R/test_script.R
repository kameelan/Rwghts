# Author: Kameela Noah
# Date: November 1, 2024
# Description: Test script for quality assurance of the Rwghts package

# Load needed packages
install.packages("testthat")
library(testthat)
# install.packages("Rwghts") Had to manually install package from local machine, as it is not available on CRAN
library(Rwghts)

# tests/testthat/test-generate_rep_weights.R

test_that("generate_rep_weights works correctly with jackknife method", {
  data <- data.frame(
    id = 1:10,
    weight = runif(10, 1, 5),
    gender = rep(c("Male", "Female"), 5),
    age = sample(20:50, 10, replace = TRUE),
    cleaned_race_eth = sample(c("White", "Black", "Asian", "Other"), 10, replace = TRUE)
  )

  result <- generate_rep_weights(data, method = "jackknife")
  expect_length(result, 10)  # Expect 10 replicate weights
  expect_type(result, "list")
})

test_that("generate_rep_weights handles bootstrap method", {
  data <- data.frame(
    id = 1:10,
    weight = runif(10, 1, 5),
    gender = rep(c("Male", "Female"), 5),
    age = sample(20:50, 10, replace = TRUE),
    cleaned_race_eth = sample(c("White", "Black", "Asian", "Other"), 10, replace = TRUE)
  )

  result <- generate_rep_weights(data, method = "bootstrap", num_reps = 5)
  expect_length(result, 5)  # Expect 5 replicate weights
  expect_type(result, "list")
})

test_that("generate_rep_weights handles brr method", {
  data <- data.frame(
    id = 1:10,
    weight = runif(10, 1, 5),
    gender = rep(c("Male", "Female"), 5),
    age = sample(20:50, 10, replace = TRUE),
    cleaned_race_eth = sample(c("White", "Black", "Asian", "Other"), 10, replace = TRUE),
    strata = rep(c("A", "B"), 5)  # Example strata variable
  )

  result <- generate_rep_weights(data, method = "brr", strata_var = "strata")
  expect_length(result, 50)  # Assuming num_reps = 50
  expect_type(result, "list")
})
