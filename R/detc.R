




#' DET Curve calculation
#'
#' From a response and predictors, the function calculates the DET curve for each pair (response, predictor). Optionally, it can compute this curve with a Confidence Interval (CI).
#' Instead of a response and predictors, it can also receive a 'DETs' object to extract the results of the DET curves and compute the CIs.
#' @param response A factor, typically encoded with 0 (non-target) and 1 (target). Also it can be a dicothomous variable which will be treated as a factor. By default, the level of response is taken as target.
#' @param predictors A matrix which columns represent the values of each predictor.
#' @param dets A 'DETs' object which will be used to compute the DET curves.
#' @param names A character vector that will be used to set the names of the DET Curves. It will also appear on the graphic legend when is plotted.
#' @param conf If present, it represents the confidence level of the CI of the DET Curve, within [0,1]. Default: The CI will not be computed.
#' @param positive A string with the name of the 'positive' level which is setting as reference level of 'response'.
#' @param parallel If TRUE, the bootstrap method to calculated the CI is processed in parallel, using the backend provided by \code{plyr} (foreach).
#' @param ncores The number of nodes to be forked for the parallel computation of the CI. Default: the maximum available. None used if \code{parallel = FALSE}.
#' @param nboot The number of bootstrap replicates to be used for the computation of the CI. Default: 2000.
#' @param plot If TRUE, the DETs curves will be plotted. Default: FALSE.
#' @param ... Further attributes that will be passed to the \code{plot} function.
#' @return A 'DETs' object. This object contains in the attribute 'detCurves' the list of DET curves, one per classifier. Each DET curve is an object of class "DET". This object contains the parameters of the DET curve (false positive ratio, false negative ratio, and the thresholds used), along with the Equal Error Rate (EER). If the CI was calculated, it includes the median,
#' upper and lower extremes of the CI of the false negative ratio and EER. Check the \code{'show'} function to know more details on the outcomes saved in a 'DETs' object.
#' @examples
#' \donttest{
#' library(DET)
#' n = 500
#' #Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative1 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(5321)
#' scoreNegative2 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(11452)
#' scorePositive1 = rnorm(n, mean = 0.55, sd = 0.125)
#' set.seed(54321)
#' scorePositive2 = rnorm(n, mean = 0.65, sd = 0.125)
#' response = as.factor(c(rep(c("target"), times = n), rep(c("nontarget"), times = n)))
#' predictor1 = c(scoreNegative1, scorePositive1)
#' predictor2 = c(scoreNegative2, scorePositive2)
#' predictors = matrix(c(predictor1, predictor2), ncol = 2)
#' colnames(predictors) = c("DET1", "DET2")
#' detCurves = detc(
#'   response,
#'   predictors,
#'   positive = "target",
#'   names = colnames(predictors)
#' )
#'
#' #Run in parallel for a faster execution activating logical argument 'parallel'
#' #and setting the number of cores of your computer
#' #logical argument 'parallel'
#' detCurvesWithConfidenceInterval = detc(
#'   response,
#'   predictors,
#'   positive = "target",
#'   names = colnames(predictors),
#'   conf = 0.95,
#'   parallel = TRUE,
#'   ncores = 2
#' )
#' }
#' @export
#' @import pROC
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
#' From a 'DETs' object, the function extracts either computes the confidence interval (CI) of each DET curve of the object.
#' @param dets A 'DETs' object which will be used to extract or compute the CIs of the DET curves.
#' @param conf A single numeric value into the (0,1) interval,  which represents the confidence level of the CI of the DET Curve. Default: \code{conf = 0.95}.
#' @param positive A string with the name of the 'positive' level which is setting as reference level of 'response'.
#' @param parallel Boolean. By default \code{parallel = TRUE}. If TRUE, the bootstrap method to calculated the CI is processed in parallel, using the backend provided by \code{plyr} (foreach).
#' @param ncores The number of nodes to be forked for the parallel computation of the CI. Default: the maximum available. None used if \code{parallel = FALSE}.
#' @param nboot The number of bootstrap replicates to be used for the computation of the CI. Default: \code{nboot = 2000}.
#' @param plot If TRUE, the CIs will be plotted for the DET curves. Default: \code{plot = FALSE}.
#' @param ... Further attributes that will be passed to the \code{plot} function.
#' @return A 'DETs' object containing the list of DET curves with their CIs, one per classifier.
#' @examples
#' \donttest{
#' library(DET)
#' n = 500
#' #Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative1 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(5321)
#' scoreNegative2 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(11452)
#' scorePositive1 = rnorm(n, mean = 0.55, sd = 0.125)
#' set.seed(54321)
#' scorePositive2 = rnorm(n, mean = 0.65, sd = 0.125)
#' response = as.factor(c(rep(c("target"), times = n), rep(c("nontarget"), times = n)))
#' predictor1 = c(scoreNegative1, scorePositive1)
#' predictor2 = c(scoreNegative2, scorePositive2)
#' predictors = matrix(c(predictor1, predictor2), ncol = 2)
#' colnames(predictors) = c("DET1", "DET2")
#' detCurves = detc(
#'   response,
#'   predictors,
#'   positive = "target",
#'   names = colnames(predictors)
#' )
#'
#' #Run in parallel for a faster execution activating logical argument 'parallel'
#' #and setting the number of cores of your computer
#' #logical argument 'parallel'
#' detCurvesWithConfidenceInterval = detc.ci(
#'   dets = detCurves,
#'   positive = "target",
#'   names = colnames(predictors),
#'   conf = 0.95,
#'   parallel = TRUE,
#'   ncores = 2
#' )
#' }
#' @export
#' @import pROC
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
