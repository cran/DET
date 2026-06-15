test_that("EER validates inputs", {
  expect_error(EER(c(0, 1), c(0)), "same length")
  expect_error(EER(c("0", "1"), c(0, 1)), "numeric vectors")
})

test_that("EER returns the average rate at the closest crossing", {
  fpr = c(1, 0.6, 0.4, 0)
  fnr = c(0, 0.3, 0.5, 1)

  expect_equal(EER(fpr, fnr), 0.45)
})

test_that("minDcf validates the DET input", {
  expect_error(minDcf(list()), "must be a 'DET' object")
})

test_that("minDcf returns expected fields for a DET object", {
  response = as.factor(c("negative", "positive", "positive", "negative"))
  predictors = as.matrix(data.frame(predictor1 = c(0.2, 0.4, 0.6, 0.8)))
  detCurves = suppressMessages(detc(response, predictors, positive = "positive"))

  result = minDcf(detCurves@detCurves[[1]])

  expect_named(
    result,
    c("minDcfValue", "minDcfIndex", "minDcf_threshold", "minDcf_fpr", "minDcf_fnr")
  )
  expect_type(result$minDcfValue, "double")
  expect_type(result$minDcfIndex, "double")
  expect_true(result$minDcfIndex >= 1)
})
