library(testthat)

find_data_path <- function() {
  candidates <- c(
    "insurance.csv",
    "../../insurance.csv",
    "../insurance.csv",
    file.path(getwd(), "insurance.csv")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p))
  }
  return(NULL)
}

test_that("análisis cruzado 1: smoker vs charges", {
  path <- find_data_path()
  skip_if(is.null(path), "insurance.csv not found")
  df <- read.csv(path, stringsAsFactors = TRUE)
  
  # Fumadores
  fum <- df$charges[df$smoker == "yes"]
  no_fum <- df$charges[df$smoker == "no"]
  
  expect_equal(length(fum), 274)
  expect_equal(length(no_fum), 1064)
  
  expect_equal(round(mean(fum), 2), 32050.23)
  expect_equal(round(median(fum), 2), 34456.35)
  expect_equal(round(sd(fum), 2), 11541.55)
  
  expect_equal(round(mean(no_fum), 2), 8434.27)
  expect_equal(round(median(no_fum), 2), 7345.41)
  expect_equal(round(sd(no_fum), 2), 5993.78)
  
  # Mediana fumador es casi 5 veces la mediana no fumador
  expect_gt(median(fum), median(no_fum) * 4.5)
})

test_that("análisis cruzado 2: correlación age vs charges y bmi vs charges", {
  path <- find_data_path()
  skip_if(is.null(path), "insurance.csv not found")
  df <- read.csv(path, stringsAsFactors = TRUE)
  
  r_age_charges <- cor(df$age, df$charges)
  r_bmi_charges <- cor(df$bmi, df$charges)
  
  expect_equal(round(r_age_charges, 4), 0.2990)
  expect_equal(round(r_bmi_charges, 4), 0.1983)
})

test_that("análisis cruzado 3: sex vs smoker tabla de contingencia", {
  path <- find_data_path()
  skip_if(is.null(path), "insurance.csv not found")
  df <- read.csv(path, stringsAsFactors = TRUE)
  
  tab <- table(df$sex, df$smoker)
  expect_equal(as.numeric(tab["female", "no"]), 547)
  expect_equal(as.numeric(tab["female", "yes"]), 115)
  expect_equal(as.numeric(tab["male", "no"]), 517)
  expect_equal(as.numeric(tab["male", "yes"]), 159)
  
  test_chi <- chisq.test(tab, correct = FALSE)
  expect_equal(round(test_chi$statistic, 4), 7.7659)
})
