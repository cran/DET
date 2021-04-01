setClass(
  "DET",
  slots = list(
    fpr = "numeric",
    fnr = "numeric",
    thresholds = "numeric",
    response = "factor",
    predictor = "numeric",
    eer = "numeric",
    conf = "numeric",
    eerLower = "numeric",
    eerMedian = "numeric",
    eerUpper = "numeric",
    fnrLower = "numeric",
    fnrMedian = "numeric",
    fnrUpper = "numeric"
  )
)
setClass("DETs", slots = list(detCurves = "list"))