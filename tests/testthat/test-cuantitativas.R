# tests/testthat/test-cuantitativas.R
# Test suite for Punto 3.2: Variables cuantitativas (insurance.csv)
library(testthat)

test_that("insurance dataset has required quantitative columns and rows", {
  test_path_data <- if (file.exists("insurance.csv")) "insurance.csv" else "../insurance.csv"
  skip_if_not(file.exists(test_path_data), "insurance.csv not found")
  
  df <- read.csv(test_path_data, stringsAsFactors = FALSE)
  expect_equal(nrow(df), 1338)
  expect_contains(names(df), c("age", "bmi", "children", "charges"))
  
  # Type checks
  expect_type(df$age, "integer") # or double
  expect_type(df$children, "integer")
  expect_type(df$bmi, "double")
  expect_type(df$charges, "double")
  
  # Completeness
  expect_false(anyNA(df$age))
  expect_false(anyNA(df$bmi))
  expect_false(anyNA(df$children))
  expect_false(anyNA(df$charges))
})

test_that("medidas de tendencia central y dispersión coinciden con valores teóricos", {
  test_path_data <- if (file.exists("insurance.csv")) "insurance.csv" else "../insurance.csv"
  skip_if_not(file.exists(test_path_data), "insurance.csv not found")
  
  df <- read.csv(test_path_data, stringsAsFactors = FALSE)
  
  # age
  expect_equal(round(mean(df$age), 2), 39.21)
  expect_equal(median(df$age), 39)
  expect_equal(round(sd(df$age), 2), 14.05)
  expect_equal(round(sd(df$age) / mean(df$age) * 100, 2), 35.84)
  expect_equal(min(df$age), 18)
  expect_equal(max(df$age), 64)
  
  # children
  expect_equal(round(mean(df$children), 2), 1.09)
  expect_equal(median(df$children), 1)
  expect_equal(round(sd(df$children), 2), 1.21)
  expect_equal(min(df$children), 0)
  expect_equal(max(df$children), 5)
  
  # bmi
  expect_equal(round(mean(df$bmi), 2), 30.66)
  expect_equal(round(median(df$bmi), 2), 30.40)
  expect_equal(round(sd(df$bmi), 2), 6.10)
  expect_equal(round(sd(df$bmi) / mean(df$bmi) * 100, 2), 19.89)
  expect_equal(min(df$bmi), 15.96)
  expect_equal(max(df$bmi), 53.13)
  
  # charges
  expect_equal(round(mean(df$charges), 2), 13270.42)
  expect_equal(round(median(df$charges), 2), 9382.03)
  expect_equal(round(sd(df$charges), 2), 12110.01)
  expect_equal(round(sd(df$charges) / mean(df$charges) * 100, 2), 91.26)
  expect_equal(round(min(df$charges), 2), 1121.87)
  expect_equal(round(max(df$charges), 2), 63770.43)
})

test_that("detección de atípicos según criterio de Tukey (IQR)", {
  test_path_data <- if (file.exists("insurance.csv")) "insurance.csv" else "../insurance.csv"
  skip_if_not(file.exists(test_path_data), "insurance.csv not found")
  
  df <- read.csv(test_path_data, stringsAsFactors = FALSE)
  
  calc_outliers <- function(x) {
    q1 <- quantile(x, 0.25)
    q3 <- quantile(x, 0.75)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    sum(x < lower | x > upper)
  }
  
  expect_equal(calc_outliers(df$age), 0)
  expect_equal(calc_outliers(df$children), 0)
  expect_equal(calc_outliers(df$bmi), 9)
  expect_equal(calc_outliers(df$charges), 139)
})
