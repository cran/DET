assertDetCurveParameters = function(response,
                                    predictors,
                                    ncores,
                                    conf,
                                    dets,
                                    names,
                                    nboot) {
  if (!is.null(dets) && class(dets) != "DETs") {
    stop("'dets' parameter needs to be an object of class 'DETs'.")
  }
  if (!is.null(dets) && !is.null(predictors)) {
    stop(
      "Wrong usage of the function. You can pass either a 'DETs' object or a pair of variables 'response' and 'predictors'."
    )
  }
  if (!is.null(predictors)) {
    if (length(response) == 0 || nrow(predictors) == 0) {
      stop("'response' or 'predictors' are empty.")
    }
    if (!is.matrix(predictors)) {
      stop("Wrong type of argument: 'predictors' argument must be a 'matrix'.")
    }
    if (!is.factor(response)) {
      stop("Wrong type of argument: 'response' argument must be a 'factor'.")
    }
    if (length(response) != nrow(predictors)) {
      stop("'response' and 'predictors' must have the same length.")
    }
    if (!is.null(names) && (length(names) != ncol(predictors))) {
      stop("'names' must have the same length that number of predictors.")
    }
  }
  if (!is.null(conf) && (conf <= 0 || conf >= 1)) {
    stop("'conf' must be between 0 and 1.")
  }
  if (ncores <= 0) {
    stop("'ncores' must be a integer number greater than 0.")
  }
  if (!is.null(conf) && (!is.null(nboot) && nboot < 1)) {
    stop("'nboot' must be greater than 1.")
  }
}

isDetCurveAlreadyComputedForCI = function(dets, conf) {
  if (!is.nan(dets@detCurves[[1]]@conf) &&
      conf == (dets@detCurves[[1]]@conf)) {
    cat("DETs object already has the confidence interval with level",
        dets@detCurves[[1]]@conf)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

extractLevelsFromResponse = function(response, positive) {
  levels = levels(response)
  if (length(levels) != 2) {
    stop("'response' argument must have two levels.")
  }
  if (!any(levels == positive)) {
    stop("'positive' argument must be one of the 'response' levels.")
  }
  if (positive != levels[2]) {
    levels = rev(levels)
  }
  return(levels)
}

buildPredictorList = function(dets, predictors, names) {
  predictorList = list()
  if (!is.null(dets)) {
    for (det in dets@detCurves) {
      predictorList[[length(predictorList) + 1]] = det@predictor
    }
    if (is.null(names) && !is.null(names(dets@detCurves))) {
      names = names(dets@detCurves)
    }
  } else {
    for (i in seq(ncol(predictors))) {
      predictorList[[length(predictorList) + 1]] = predictors[, i]
    }
    if (is.null(names) && !is.null(colnames(predictors))) {
      names = colnames(predictors)
    }
  }
  if (is.null(names)) {
    for (i in seq(length(predictorList))) {
      names[i] = paste("X", i, sep = "")
    }
  }
  names(predictorList) = names
  return(predictorList)
}

buildDetCurveInformationWithoutCI = function(rocCurve) {
  fpr = 1 - rocCurve$specificities
  fnr = 1 - rocCurve$sensitivities
  eer = EER(fpr, fnr)
  detCurveInformation = new(
    "DET",
    fpr = fpr,
    fnr = fnr,
    thresholds = rocCurve$thresholds,
    response = rocCurve$response,
    predictor = rocCurve$predictor,
    eer = eer,
    conf = NaN,
    fnrLower = NaN,
    fnrMedian = NaN,
    fnrUpper = NaN,
    eerMedian = NaN,
    eerLower = NaN,
    eerUpper = NaN
  )
  return(detCurveInformation)
}

buildDetCurveInformationWithCI = function(rocCurve,
                                          conf,
                                          sensitivityConfidenceInterval) {
  fpr = 1 - rocCurve$specificities
  fnr = 1 - rocCurve$sensitivities
  fnrLower = 1 - as.numeric(sensitivityConfidenceInterval[, 3])
  fnrMedian = 1 - as.numeric(sensitivityConfidenceInterval[, 2])
  fnrUpper = 1 - as.numeric(sensitivityConfidenceInterval[, 1])
  eer = EER(fpr, fnr)
  eerLower = EER(fpr, fnrLower)
  eerMedian = EER(fpr, fnrMedian)
  eerUpper = EER(fpr, fnrUpper)
  detCurveInformation = new(
    "DET",
    fpr = fpr,
    fnr = fnr,
    thresholds = rocCurve$thresholds,
    response = rocCurve$response,
    predictor = rocCurve$predictor,
    eer = eer,
    conf = conf,
    fnrLower = fnrLower,
    fnrMedian = fnrMedian,
    fnrUpper = fnrUpper,
    eerLower = eerLower,
    eerMedian = eerMedian,
    eerUpper = eerUpper
  )
  return(detCurveInformation)
}
