
#' Show the structure of a DET object
#'
#' From a 'DETs' object (generated with the \code{detc} function), the function shows the different attributes of each curve with a brief description, which have to be used to get the results for each curve.
#' @param dets An object of class "DETs".
#' @export
show.DETs <- function(dets) {
  if (class(dets) == "DETs") {
    cat("**Parameters of the Detection Error Tradeoff (DET) Curves**\n\n")
    cat("Classifiers names:", names(dets@detCurves), "\n\n")
    cat("*To access the list of DET Curves, take the attribute @detCurves\n")
    cat("*For each DET Curve, you can get the following attributes:\n\n")
    cat("@fpr            false positive rates\n")
    cat("@fnr            false negative rates\n")
    cat("@thresholds     thresholds corresponding to points of the DET curve\n")
    cat("@eer            Equal Error Rate\n")
    det = dets@detCurves[[1]]
    if (is.numeric(det@conf) && !is.nan(det@conf)) {
      cat("The",
          det@conf * 100,
          "% Confidence Interval has been calculated\n\n")
      cat("@conf           the confidence level of the confidence intervals\n")
      cat("@fnrLower       lower extremes for false negative rates CI\n")
      cat("@fnrMedian      median for false negative rates CI\n")
      cat("@fnrUpper       upper extremes for false negative rates CI\n")
      cat("@eerLower       lower extreme of the EER CI\n")
      cat("@eerMedian      median for the EER CI\n")
      cat("@eerUpper       upper extreme of the EER CI\n")
    }
  }
}
