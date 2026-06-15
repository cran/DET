makeSimpleDetCurves = function() {
  response = as.factor(c("negative", "positive", "positive", "negative"))
  predictors = as.matrix(data.frame(
    predictor1 = c(0.2, 0.4, 0.6, 0.8),
    predictor2 = c(0.1, 0.8, 0.8, 0.3)
  ))
  suppressMessages(detc(
    response,
    predictors,
    names = c("predictor1", "predictor2"),
    positive = "positive"
  ))
}

test_that("DET and ROC plotting functions run without error", {
  detCurves = makeSimpleDetCurves()
  plotFile = tempfile(fileext = ".pdf")
  grDevices::pdf(plotFile)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(plot(detCurves), NA)
  expect_error(plot(detCurves@detCurves[[1]]), NA)
  expect_error(plotROCs(detCurves), NA)
  expect_error(plotROC(detCurves@detCurves[[1]]), NA)
  expect_error(pointsEER(detCurves), NA)
})
