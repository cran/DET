#' DET Curve calculation
#'
#' From a response and one or more predictors, the function calculates a DET curve for each response-predictor pair. Optionally, it can compute each curve with a confidence interval (CI).
#' Instead of a response and predictors, it can also receive a 'DETs' object to extract existing DET curve results and compute CIs.
#' @param response A factor, typically encoded with 0 (healthy, genuine user, signal, normal) and 1 (diseased, impostor user, noise, abnormal). It can also be a dichotomous variable that will be treated as a factor. By default, the second factor level is used as the positive class.
#' @param predictors A matrix whose columns represent the values of each predictor.
#' @param dets A 'DETs' object which will be used to compute the DET curves.
#' @param names A character vector that will be used to set the names of the DET curves. They will also appear in the plot legend.
#' @param conf If present, the confidence level for the DET curve CI, within [0,1]. Default: the CI is not computed.
#' @param positive A string with the name of the 'positive' level used as the reference level of 'response'. If left as the default empty string, the second factor level of 'response' is used.
#' @param parallel If TRUE, the bootstrap method used to calculate the CI is processed in parallel, using the backend provided by \code{plyr} (foreach).
#' @param ncores The number of nodes to be forked for the parallel computation of the CI. Default: the maximum available. None used if \code{parallel = FALSE}.
#' @param nboot The number of bootstrap replicates to be used for the computation of the CI. Default: 2000.
#' @param plot If TRUE, the DET curves are plotted. Default: FALSE.
#' @param ... Further attributes that will be passed to the \code{plot} function.
#' @return A 'DETs' object. The object's 'detCurves' attribute contains one DET curve per classifier. Each DET curve is an object of class "DET", containing the DET curve parameters (false positive ratio, false negative ratio, and thresholds), along with the Equal Error Rate (EER). If the CI was calculated, the object also includes the median,
#' upper, and lower CI limits for the false negative ratio and EER. Use the \code{show} method for more details about the results saved in a 'DETs' object.
#' @examples
#' \donttest{
#' library(DET)
#' n = 500
#' # Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative1 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(5321)
#' scoreNegative2 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(11452)
#' scorePositive1 = rnorm(n, mean = 0.55, sd = 0.125)
#' set.seed(54321)
#' scorePositive2 = rnorm(n, mean = 0.65, sd = 0.125)
#' response = as.factor(c(rep(c("diseased"), times = n), rep(c("healthy"), times = n)))
#' predictor1 = c(scoreNegative1, scorePositive1)
#' predictor2 = c(scoreNegative2, scorePositive2)
#' predictors = matrix(c(predictor1, predictor2), ncol = 2)
#' colnames(predictors) = c("DET1", "DET2")
#' detCurves = detc(
#'   response,
#'   predictors,
#'   positive = "diseased",
#'   names = colnames(predictors)
#' )
#'
#' # Run in parallel for faster execution by activating the logical argument
#' # 'parallel' and setting the number of cores of your computer
#' detCurvesWithConfidenceInterval = detc(
#'   response,
#'   predictors,
#'   positive = "diseased",
#'   names = colnames(predictors),
#'   conf = 0.95,
#'   parallel = TRUE,
#'   ncores = 2
#' )
#' }
#' @export
#' @importFrom pROC roc ci.se
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom methods new
detc = function(response = NULL,
                predictors = NULL,
                dets = NULL,
                names = NULL,
                conf = NULL,
                positive = "",
                parallel = FALSE,
                ncores = detectCores(),
                nboot = NULL,
                plot = FALSE,
                ...) {
  if (is.null(dets)) {
    response = as.factor(response)
  }
  
  if (!is.matrix(predictors)) {
    if(is.vector(predictors)) {
      ## if it's a vector, just change it to matrix with one column
      predictors = matrix(predictors, ncol = 1)
    }
  }
  
  assertDetCurveParameters(response, predictors, ncores, conf, dets, names, nboot)
  if (!is.null(dets)) {
    if (isDetCurveAlreadyComputedForCI(dets, conf)) {
      if (plot) {
        plot.DETs(dets, ...)
      }
      return(dets)
    } else {
      response = dets@detCurves[[1]]@response
    }
  }
  predictorList = buildPredictorList(dets, predictors, names)
  nCurves = length(predictorList)
  detCurvesInformation = list()
  levels = extractLevelsFromResponse(response, positive)
  defaultW <- getOption("warn")
  options(warn = -1)
  for (i in seq(nCurves)) {
    cat("Calculating DET Curve for:", names(predictorList)[i], "\n")
    if (parallel) {
      cluster = makeCluster(ncores)
      registerDoParallel(cluster)
    }
    rocCurve = roc(response, predictorList[[i]], levels = levels)
    if (!is.null(conf)) {
      if (is.null(nboot)) {
        nboot = 2000
      }
      sensitivityConfidenceInterval = ci.se(
        rocCurve,
        specificities = rocCurve$specificities,
        conf.level = conf,
        boot.n = as.integer(nboot),
        method = "bootstrap",
        parallel = parallel
      )
      detCurveInformation = buildDetCurveInformationWithCI(rocCurve,
                                                           conf,
                                                           sensitivityConfidenceInterval)
    } else {
      detCurveInformation = buildDetCurveInformationWithoutCI(rocCurve)
    }
    detCurvesInformation[[length(detCurvesInformation) + 1]] = detCurveInformation
    if (parallel) {
      stopCluster(cluster)
    }
  }
  options(warn = defaultW)
  names(detCurvesInformation) = names(predictorList)
  detCurves = (new("DETs", detCurves = detCurvesInformation))
  if (plot) {
    plot.DETs(detCurves, ...)
  }
  return(detCurves)
}

#' DET Curve calculation with CI
#'
#' From a 'DETs' object, the function extracts or computes the confidence interval (CI) of each DET curve in the object.
#' @param dets A 'DETs' object which will be used to extract or compute the CIs of the DET curves.
#' @param conf A single numeric value in the (0,1) interval representing the confidence level of the DET curve CI. Default: \code{conf = 0.95}.
#' @param positive A string with the name of the 'positive' level used as the reference level of 'response'. If left as the default empty string, the second factor level of 'response' is used.
#' @param parallel Boolean. By default \code{parallel = TRUE}. If TRUE, the bootstrap method used to calculate the CI is processed in parallel, using the backend provided by \code{plyr} (foreach).
#' @param ncores The number of nodes to be forked for the parallel computation of the CI. Default: the maximum available. None used if \code{parallel = FALSE}.
#' @param nboot The number of bootstrap replicates to be used for the computation of the CI. Default: \code{nboot = 2000}.
#' @param plot If TRUE, the CIs will be plotted for the DET curves. Default: \code{plot = FALSE}.
#' @param ... Further attributes that will be passed to the \code{plot} function.
#' @return A 'DETs' object containing the list of DET curves with their CIs, one per classifier.
#' @examples
#' \donttest{
#' library(DET)
#' n = 500
#' # Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative1 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(5321)
#' scoreNegative2 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(11452)
#' scorePositive1 = rnorm(n, mean = 0.55, sd = 0.125)
#' set.seed(54321)
#' scorePositive2 = rnorm(n, mean = 0.65, sd = 0.125)
#' response = as.factor(c(rep(c("diseased"), times = n), rep(c("healthy"), times = n)))
#' predictor1 = c(scoreNegative1, scorePositive1)
#' predictor2 = c(scoreNegative2, scorePositive2)
#' predictors = matrix(c(predictor1, predictor2), ncol = 2)
#' colnames(predictors) = c("DET1", "DET2")
#' detCurves = detc(
#'   response,
#'   predictors,
#'   positive = "diseased",
#'   names = colnames(predictors)
#' )
#'
#' # Run in parallel for faster execution by activating the logical argument
#' # 'parallel' and setting the number of cores of your computer
#' detCurvesWithConfidenceInterval = detc.ci(
#'   dets = detCurves,
#'   positive = "diseased",
#'   names = colnames(predictors),
#'   conf = 0.95,
#'   parallel = TRUE,
#'   ncores = 2
#' )
#' }
#' @export
#' @importFrom pROC roc ci.se
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom methods new
detc.ci = function(dets = NULL,
                   conf = 0.95,
                   positive = "",
                   parallel = TRUE,
                   ncores = detectCores(),
                   nboot = 2000,
                   plot = FALSE,
                   ...) {
  return(
    detc(
      dets = dets,
      conf = conf,
      positive = positive,
      parallel = parallel,
      ncores = ncores,
      nboot = nboot,
      plot = plot,
      ...
    )
  )
}
