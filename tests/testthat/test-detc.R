context("Testing the detc function")
library(DET)

response = as.factor(c("+", "-"))
predictors = as.matrix(data.frame(predictor1 = c(1, 1),
                                  predictor2 = c(0, 1)))

test_that("detc errors for empty input", {
  expect_error(detc(c(), predictors),
               "'response' or 'predictors' are empty")
  expect_error(detc(response, as.matrix(data.frame())),
               "'response' or 'predictors' are empty")
})

test_that("detc errors for bad CI width", {
  expect_error(detc(response, predictors, conf = 1.1),
               "'conf' must be between 0 and 1")
})

test_that("detc errors for bad number of ncores", {
  expect_error(detc(response, predictors, ncores = 0),
               "'ncores' must be a integer number greater than 0.")
})

test_that("detc errors for bad types", {
  expect_error(detc(c("+", "-"), predictors),
               "'positive' argument must be one of the 'response' levels.")
  expect_error(detc(response, as.data.frame(predictors)),
               "Wrong type of argument: 'predictors' argument must be a 'matrix'.")
})

test_that("detc errors for responses with more than 2 levels", {
  response <- as.factor(c("positive", "negative", "neutral"))
  predictors <- as.matrix(data.frame(predictor1 = c(0.2, 0.4, 0.6)))
  expect_error(detc(response, predictors),
               "'response' argument must have two levels.")
})

test_that("detc errors for positive class not in response levels", {
  response <- as.factor(c("positive", "negative", "positive"))
  predictors <- as.matrix(data.frame(predictor1 = c(0.2, 0.4, 0.6)))
  expect_error(
    detc(response, predictors, positive = "other"),
    "'positive' argument must be one of the 'response' levels."
  )
})

test_that("detc calculates DET curve without CI", {
  response <-
    as.factor(c("negative", "positive", "positive", "negative"))
  predictors <- as.matrix(data.frame(
    predictor1 = c(0.2, 0.4, 0.6, 0.8),
    predictor2 = c(0.1, 0.8, 0.8, 0.3)
  ))
  expectedDetCurve = new("DETs", detCurves = list(
    predictor1 = new(
      "DET",
      fpr = c(1, 0.5, 0.5, 0.5, 0),
      fnr = c(0, 0, 0.5, 1, 1),
      thresholds = c(-Inf, 0.3, 0.5, 0.7, Inf),
      response = response,
      predictor = predictors[, 1],
      eer = 0.5,
      fnrLower = NaN,
      fnrMedian = NaN,
      fnrUpper = NaN,
      conf = NaN,
      eerMedian = NaN,
      eerLower = NaN,
      eerUpper = NaN
    ),
    predictor2 = new(
      "DET",
      fpr = c(1, 0.5, 0, 0),
      fnr = c(0, 0, 0, 1),
      thresholds = c(-Inf, 0.2, 0.55, Inf),
      eer = 0,
      response = response,
      predictor = predictors[, 2],
      fnrLower = NaN,
      fnrMedian = NaN,
      fnrUpper = NaN,
      conf = NaN,
      eerMedian = NaN,
      eerLower = NaN,
      eerUpper = NaN
    )
  ))
  actualDetCurve = detc(
    response,
    predictors,
    names = c("predictor1", "predictor2"),
    positive = "positive"
  )
  
  expect_equal(actualDetCurve, expectedDetCurve)
})

test_that("detc calculates DET curve with CI", {
  response <-
    as.factor(c("negative", "positive", "positive", "negative"))
  predictors <- as.matrix(data.frame(
    predictor1 = c(0.2, 0.4, 0.6, 0.8),
    predictor2 = c(0.1, 0.8, 0.8, 0.3)
  ))
  expectedDetCurve = new("DETs", detCurves = list(
    predictor1 = new(
      "DET",
      fpr = c(1, 0.5, 0.5, 0.5, 0),
      fnr = c(0, 0, 0.5, 1, 1),
      thresholds = c(-Inf, 0.3, 0.5, 0.7, Inf),
      response = response,
      predictor = predictors[, 1],
      eer = 0.5,
      fnrLower = c(0, 0, 0, 0, 0),
      fnrMedian = c(0, 0, 0, 0, 1),
      fnrUpper = c(0, 1, 1, 1, 1),
      conf = 0.95,
      eerMedian = 0.25,
      eerLower = 0,
      eerUpper = 0.75
    ),
    predictor2 = new(
      "DET",
      fpr = c(1, 0.5, 0, 0),
      fnr = c(0, 0, 0, 1),
      thresholds = c(-Inf, 0.2, 0.55, Inf),
      eer = 0.0,
      response = response,
      predictor = predictors[, 2],
      fnrLower = c(0, 0, 0, 0),
      fnrMedian = c(0, 0, 0, 0),
      fnrUpper = c(0, 0, 0, 0),
      conf =  0.95,
      eerMedian = 0,
      eerLower = 0,
      eerUpper = 0
    )
  ))
  actualDetCurve = detc(
    response,
    predictors,
    names = c("predictor1", "predictor2"),
    positive = "positive",
    conf = 0.95
  )
  
  expect_equal(actualDetCurve, expectedDetCurve)
})
