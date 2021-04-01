#' Equal Error Rate computation
#'
#' From two vectors of false positive and false negative rates which define the points of the curve, the function computes the Equal Error Rate (EER).
#' @param fpr A numeric vector representing the False Positive Rates.
#' @param fnr A numeric vector representing the False Negative Rates.
#' @return The Equal Error Rate (EER). 
#' @export
EER = function(fpr, fnr) {
  if ((!is.numeric(fpr) || !is.numeric(fnr)) || length(fpr) != length(fnr)) {
    stop("'fpr' and 'fnr' must be numeric vectors with the same length.")
  }
  return(0.5 * fpr[which.min(abs(fpr - fnr))] + 0.5 * fnr[which.min(abs(fpr - fnr))])
}


#' Minimum of the Detection Cost Function computation
#'
#' From a 'DET' object, the function computes the minimum value of the Detection Cost Function (minDCF).
#' @param det An object of class "DET".
#' @param p A single numeric value into the (0, 1) intervalrepresenting the prior probability of positive class.
#' @param cFp A single numeric value representing the cost of False Positives.
#' @param cFn A single numeric value representing the cost of False Negatives.
#' @return A 'data.frame' with two attributes:
#'
#' - 'minDcfValue': the computed minDCF.
#'
#' - 'minDcfIndex': the index of the fpr and fnr in which the minimum is reached.
#' @export
minDcf = function(det,
                  p = 0.01,
                  cFp = 1,
                  cFn = 10) {
  if (class(det) != "DET") {
    stop("'det' parameter must be a 'DET' object.")
  }
  if (p<0 || p>1) {
    stop("'p' parameter must be into the (0, 1) interval.")
  }
  if (!is.numeric(c(cFp, cFn)) && (length(cFp) !=1 || length(cFn) != 1)) {
    stop("Wrong type of the costs of false rates, 'cFp' and 'cFn' parameters must to be single numeric values.")
  }
  fpr = det@fpr
  fnr = det@fnr
  minDcfValue = min(p * cFn * fnr + (1 - p) * cFp * fpr)
  minDcfIndex = which.min(p * cFn * fnr + (1 - p) * cFp * fpr)
  return(data.frame(minDcfValue = minDcfValue, minDcfIndex = minDcfIndex))
}
