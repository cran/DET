test_that("show methods describe DETs objects", {
  response = as.factor(c("negative", "positive", "positive", "negative"))
  predictors = as.matrix(data.frame(predictor1 = c(0.2, 0.4, 0.6, 0.8)))
  detCurves = suppressMessages(detc(response, predictors, positive = "positive"))

  expect_output(show.DETs(detCurves), "Detection Error Tradeoff")
  expect_output(show(detCurves), "Detection Error Tradeoff")
  expect_output(show(detCurves), "Classifiers names")
})
