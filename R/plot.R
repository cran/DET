plotDetCurveWithCI = function(detCurve, color, lwd, type) {
  big_integer = 2147483647
  x = qnorm(detCurve@fpr)
  y0 = qnorm(detCurve@fnr)
  y1 = qnorm(detCurve@fnrLower)
  y2 = qnorm(detCurve@fnrUpper)
  x[x == Inf] = big_integer
  x[x == -Inf] = -big_integer
  y0[y0 == -Inf] = -big_integer
  y1[y1 == -Inf] = -big_integer
  y2[y2 == -Inf] = -big_integer
  y0[y0 == Inf] = big_integer
  y1[y1 == Inf] = big_integer
  y2[y2 == Inf] = big_integer
  lines(x,
        y0,
        col = color,
        lwd = lwd,
        type = type)
  lines(x, y1, col = color)
  lines(x, y2, col = color)
  bandColor = rgb(col2rgb(color)[1] / 255,
                  col2rgb(color)[2] / 255,
                  col2rgb(color)[3] / 255,
                  0.1)
  polygon(c(x, rev(x)),
          c(y2, rev(y1)),
          col = bandColor,
          border = NA)
}

plotDetCurve = function(detCurve, color, lwd) {
  big_integer = 2147483647
  x = qnorm(detCurve@fpr)
  y = qnorm(detCurve@fnr)
  x[x == Inf] = big_integer
  x[x == -Inf] = -big_integer
  y[y == -Inf] = -big_integer
  y[y == Inf] = big_integer
  lines(x, y, col = color, lwd = lwd)
}


plotDETs = function(dets,
                    xlim = c(0.05, 50),
                    ylim = c(0.05, 50),
                    col = c("black", "blue", "red", "green", "yellow"),
                    labels_x = c(0.1, 0.2, 0.5, 1, 2, 5, seq(10, 100, 10)),
                    labels_y = c(0.1, 0.2, 0.5, 1, 2, 5, seq(10, 100, 10)),
                    xlab = "FPR(%)",
                    ylab = "FNR(%)",
                    panel.first = grid(nx = 20, ny = 20),
                    lwd = 2,
                    type = "l",
                    lty = 1,
                    ...) {
  if (missing(xlim) && !missing(ylim)) {
    xlim = ylim
  }
  if (!missing(xlim) && missing(ylim)) {
    ylim = xlim
  }
  lims_x = qnorm(xlim / 100)
  lims_y = qnorm(ylim / 100)
  interval = c(1, length(labels_x))
  while (labels_x[interval[1]] < xlim[1] ||
         labels_x[interval[2]] > xlim[2]) {
    if (labels_x[interval[1]] < xlim[1])
      interval[1] = interval[1] + 1
    if (labels_x[interval[2]] > xlim[2])
      interval[2] = interval[2] - 1
  }
  labels_x = c(xlim[1], labels_x[interval[1]:interval[2]], xlim[2])
  axises_x = labels_x / 100
  interval = c(1, length(labels_y))
  while (labels_y[interval[1]] < ylim[1] ||
         labels_y[interval[2]] > ylim[2]) {
    if (labels_y[interval[1]] < ylim[1])
      interval[1] = interval[1] + 1
    if (labels_y[interval[2]] > ylim[2])
      interval[2] = interval[2] - 1
  }
  labels_y = c(ylim[1], labels_y[interval[1]:interval[2]], ylim[2])
  axises_y = labels_y / 100
  plot(
    x = NaN,
    y = NaN,
    type = "n",
    xlab = xlab,
    xlim = lims_x,
    ylab = ylab,
    ylim = lims_y,
    xaxt = "n",
    yaxt = "n",
    panel.first = panel.first,
    ...
  )
  axis(1, at = qnorm(axises_x), labels = labels_x)
  axis(2, at = qnorm(axises_y), labels = labels_y)
  lines(seq(-100, 100, 0.1),
        seq(-100, 100, 0.1),
        col = "gray",
        lty = 6)
  ncurves = length(dets@detCurves)
  if (ncurves > length(col)) {
    col = rep(col, ceiling(ncurves / length(col)))
  }
  for (i in seq(ncurves)) {
    if (is.nan(dets@detCurves[[i]]@conf)) {
      plotDetCurve(dets@detCurves[[i]], col[i], lwd)
    } else {
      plotDetCurveWithCI(dets@detCurves[[i]], col[i], lwd, type)
    }
  }
  if (ncurves != 1 && !is.null(names(dets@detCurves))) {
    legend(
      "topright",
      legend = names(dets@detCurves),
      col = col[1:ncurves],
      lty = lty,
      cex = 0.7
    )
  }
}


plotROCCurves = function(dets,
                         xlim = c(0, 1),
                         ylim = c(0, 1),
                         col = c("black", "blue", "red", "green", "yellow"),
                         labels_x = seq(0, 1, 0.1),
                         labels_y = seq(0, 1, 0.1),
                         xlab = "1 - Specificity",
                         ylab = "Sensitivity",
                         panel.first = grid(nx = 20, ny = 20),
                         type = "l",
                         lwd = 2,
                         lty = 1,
                         ...) {
  if (missing(xlim) && !missing(ylim)) {
    xlim = sort(1-ylim)
  }
  if (!missing(xlim) && missing(ylim)) {
    ylim = sort(1-xlim)
  }
  lims_x = xlim
  lims_y = ylim
  interval = c(1, length(labels_x))
  while (labels_x[interval[1]] < xlim[1] ||
         labels_x[interval[2]] > xlim[2]) {
    if (labels_x[interval[1]] < xlim[1])
      interval[1] = interval[1] + 1
    if (labels_x[interval[2]] > xlim[2])
      interval[2] = interval[2] - 1
  }
  labels_x = c(xlim[1], labels_x[interval[1]:interval[2]], xlim[2])
  interval = c(1, length(labels_y))
  while (labels_y[interval[1]] < ylim[1] ||
         labels_y[interval[2]] > ylim[2]) {
    if (labels_y[interval[1]] < ylim[1])
      interval[1] = interval[1] + 1
    if (labels_y[interval[2]] > ylim[2])
      interval[2] = interval[2] - 1
  }
  labels_y = c(ylim[1], labels_y[interval[1]:interval[2]], ylim[2])
  plot(
    x = NaN,
    y = NaN,
    type = "n",
    xlab = xlab,
    xlim = lims_x,
    ylab = ylab,
    ylim = lims_y,
    xaxt = "n",
    yaxt = "n",
    panel.first = panel.first,
    ...
  )
  axis(1, at = labels_x, labels = labels_x)
  axis(2, at = labels_y, labels = labels_y)
  lines(seq(0, 1, 0.1), seq(0, 1, 0.1), col = "gray", lty = 6)
  detCurves = dets@detCurves
  ncurves = length(detCurves)
  if (ncurves > length(col)) {
    col = rep(col, ceiling(ncurves / length(col)))
  }
  for (i in seq(ncurves)) {
    x = detCurves[[i]]@fpr
    y = 1 - detCurves[[i]]@fnr
    x = c(1, x, 0)
    y = c(1, y, 0)
    lines(x, y, col = col[i], lwd = lwd)
  }
  if (ncurves != 1 && !is.null(names(detCurves))) {
    legend(
      "bottomright",
      legend = names(detCurves),
      col = col[1:ncurves],
      lty = lty,
      cex = 0.7
    )
  }
}


plotROCCurvesWithCI = function(dets,
                               xlim = c(0, 1),
                               ylim = c(0, 1),
                               col = c("black", "blue", "red", "green", "yellow"),
                               labels_x = seq(0, 1, 0.1),
                               labels_y = seq(0, 1, 0.1),
                               xlab = "1 - Specificity",
                               ylab = "Sensitivity",
                               panel.first = grid(nx = 20, ny = 20),
                               type = "l",
                               lwd = 2,
                               lty = 1,
                               ...) {
  if (missing(xlim) && !missing(ylim)) {
    xlim = sort(1-ylim)
  }
  if (!missing(xlim) && missing(ylim)) {
    ylim = sort(1-xlim)
  }
  lims_x = xlim
  lims_y = ylim
  interval = c(1, length(labels_x))
  while (labels_x[interval[1]] < xlim[1] ||
         labels_x[interval[2]] > xlim[2]) {
    if (labels_x[interval[1]] < xlim[1])
      interval[1] = interval[1] + 1
    if (labels_x[interval[2]] > xlim[2])
      interval[2] = interval[2] - 1
  }
  labels_x = c(xlim[1], labels_x[interval[1]:interval[2]], xlim[2])
  interval = c(1, length(labels_y))
  while (labels_y[interval[1]] < ylim[1] ||
         labels_y[interval[2]] > ylim[2]) {
    if (labels_y[interval[1]] < ylim[1])
      interval[1] = interval[1] + 1
    if (labels_y[interval[2]] > ylim[2])
      interval[2] = interval[2] - 1
  }
  labels_y = c(ylim[1], labels_y[interval[1]:interval[2]], ylim[2])
  plot(
    x = NaN,
    y = NaN,
    type = "n",
    xlab = xlab,
    xlim = lims_x,
    ylab = ylab,
    ylim = lims_y,
    xaxt = "n",
    yaxt = "n",
    panel.first = panel.first,
    ...
  )
  axis(1, at = labels_x, labels = labels_x)
  axis(2, at = labels_y, labels = labels_y)
  lines(seq(0, 1, 0.1), seq(0, 1, 0.1), col = "gray", lty = 6)
  detCurves = dets@detCurves
  ncurves = length(detCurves)
  if (ncurves > length(col)) {
    col = rep(col, ceiling(ncurves / length(col)))
  }
  for (i in seq(ncurves)) {
    x = detCurves[[i]]@fpr
    y0 = 1 - detCurves[[i]]@fnr
    y1 = 1 - detCurves[[i]]@fnrLower
    y2 = 1 - detCurves[[i]]@fnrUpper
    x = c(1, x, 0)
    y0 = c(1, y0, 0)
    y1 = c(1, y1, 0)
    y2 = c(1, y2, 0)
    lines(x, y0, col = col[i], lwd = lwd)
    lines(x, y1, col = col[i])
    lines(x, y2, col = col[i])
    bandColor = rgb(col2rgb(col[i])[1] / 255,
                    col2rgb(col[i])[2] / 255,
                    col2rgb(col[i])[3] / 255,
                    0.1)
    polygon(c(x, rev(x)),
            c(y2, rev(y1)),
            col = bandColor,
            border = NA)
  }
  if (ncurves != 1 && !is.null(names(detCurves))) {
    legend(
      "bottomright",
      legend = names(detCurves),
      col = col[1:ncurves],
      lty = lty,
      cex = 0.7
    )
  }
}


#' DET Curves plot
#'
#' From a 'DETs' object generated with the \code{detc} function, this function plots the DET curves included in the object. It includes
#' a confidence band when the DET curves were calculated with a confidence interval.
#'
#' It accepts plot customisation with graphical parameters (see \code{\link{plot}}, for more details):
#'
#' -  'xlim': a numeric vector of length 2, giving the x coordinate range of the plot.
#'
#' -  'ylim': a numeric vector of length 2, giving the y coordinate range of the plot.
#'
#' -  'col': a vector of colors, specifying the color for each DET curve.
#'
#' -  'labels_x': a numeric vector indicating the labels of the x-axis.
#'
#' -  'labels_y': a numeric vector indicating the labels of the y-axis.
#'
#' -  'xlab': a main label for the x-axis.
#'
#' -  'ylab': a main label for the y-axis.
#'
#' -  'panel.first': a background grid is plotted. It can be used for modifying the background style of the plot.
#'
#' @param x An object of class "DETs".
#' @param ... Further graphical arguments passed to the \code{plot} function.
#' @export
#' @importFrom graphics axis grid legend lines plot points polygon par
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats qnorm
#' @examples
#' library(DET)
#' n = 5000
#' # Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative1 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(5321)
#' scoreNegative2 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(6987)
#' scoreNegative3 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(11452)
#' scorePositive1 = rnorm(n, mean = 0.55, sd = 0.125)
#' set.seed(54321)
#' scorePositive2 = rnorm(n, mean = 0.65, sd = 0.125)
#' set.seed(65987)
#' scorePositive3 = rnorm(n, mean = 0.75, sd = 0.125)
#' response = as.factor(c(rep(c("target"), times = n), rep(c("nontarget"), times = n)))
#' predictor1 = c(scoreNegative1, scorePositive1)
#' predictor2 = c(scoreNegative2, scorePositive2)
#' predictor3 = c(scoreNegative3, scorePositive3)
#' predictors = matrix(c(predictor1, predictor2, predictor3), ncol = 3)
#' colnames(predictors) = c("DET1", "DET2", "DET3")
#' detCurves = detc(
#'   response,
#'   predictors,
#'   positive = "target",
#'   names = colnames(predictors)
#' )
#' plot(detCurves,
#'      main = "Example",
#'      col = c("black", "blue", "red"))
plot.DETs = function(x,
                     ...) {
  plotDETs(x, ...)
}


#' DET Curve plot
#'
#' From a 'DET' object, this function plots the DET curve included in the object.
#' @param x An object of class "DET".
#' @param ... Further graphical arguments passed to the \code{plot} function.
#' @export
#' @importFrom graphics axis grid legend lines plot points polygon par
#' @importFrom stats qnorm
#' @importFrom methods new
#' @examples
#' library(DET)
#' n = 5000
#' # Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(11452)
#' scorePositive = rnorm(n, mean = 0.55, sd = 0.125)
#' response = as.factor(c(rep(c("target"), times = n), rep(c("nontarget"), times = n)))
#' predictor = matrix(c(scoreNegative, scorePositive), ncol = 1)
#' colnames(predictor) = c("DET")
#' detCurve = detc(response,
#'                 predictor,
#'                 names = colnames(predictor),
#'                 positive = "target")
#' plot(detCurve@detCurves$DET,
#'      main = "Example")
plot.DET = function (x, ...) {
  plot.DETs(new("DETs", detCurves = list(detCurve = x)), ...)
}


#' ROC Curves plot
#'
#' From a 'DETs' object, this function plots the ROC curves associated with the DET curves in the object. It includes a
#' confidence band when CIs were computed for the DET curves.
#'
#' It accepts plot customisation with graphical parameters (see \code{\link{plot}}, for more details):
#'
#' -  'xlim': a numeric vector of length 2, giving the x and y coordinate ranges of the plot.
#'
#' -  'col': a vector of colors, specifying the color for each DET curve.
#'
#' -  'labels_x': a numeric vector indicating the labels of the x- and y-axes.
#'
#' -  'xlab': a main label for the x-axis.
#'
#' -  'ylab': a main label for the y-axis.
#'
#' -  'panel.first': a background grid is plotted. It can be used for modifying the background style of the plot.
#'
#' @param dets An object of class "DETs".
#' @param ... Further graphical arguments passed to the \code{plot} function.
#' @export
#' @importFrom graphics axis grid legend lines plot points polygon par
#' @importFrom stats qnorm
#' @importFrom methods new
#' @examples
#' library(DET)
#' n = 5000
#' # Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative1 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(5321)
#' scoreNegative2 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(6987)
#' scoreNegative3 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(11452)
#' scorePositive1 = rnorm(n, mean = 0.55, sd = 0.125)
#' set.seed(54321)
#' scorePositive2 = rnorm(n, mean = 0.65, sd = 0.125)
#' set.seed(65987)
#' scorePositive3 = rnorm(n, mean = 0.75, sd = 0.125)
#' response = as.factor(c(rep(c("target"), times = n), rep(c("nontarget"), times = n)))
#' predictor1 = c(scoreNegative1, scorePositive1)
#' predictor2 = c(scoreNegative2, scorePositive2)
#' predictor3 = c(scoreNegative3, scorePositive3)
#' predictors = matrix(c(predictor1, predictor2, predictor3), ncol = 3)
#' colnames(predictors) = c("DET1", "DET2", "DET3")
#' detCurves = detc(
#'   response,
#'   predictors,
#'   positive = "target",
#'   names = colnames(predictors)
#' )
#' plotROCs(detCurves,
#'           main = "Example",
#'           col = c("black", "blue", "red"))
plotROCs = function (dets, ...) {
  if (is.nan(dets@detCurves[[1]]@conf)) {
    plotROCCurves(dets, ...)
  } else {
    plotROCCurvesWithCI(dets, ...)
  }
}


#' ROC Curve plot
#'
#' From a 'DET' object, this function plots the ROC curve associated with the DET curve of the object. It also draws the confidence band when it is available in the object.
#' @param dets A 'DET' object from the list of a 'DETs' object computed by the \code{detc} function.
#' @param ... Further graphical arguments passed to the \code{plot} function.
#' @export
#' @importFrom graphics axis grid legend lines plot points polygon par
#' @importFrom stats qnorm
#' @importFrom methods new
#' @examples
#' library(DET)
#' n = 5000
#' # Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(11452)
#' scorePositive = rnorm(n, mean = 0.55, sd = 0.125)
#' response = as.factor(c(rep(c("target"), times = n), rep(c("nontarget"), times = n)))
#' predictor = matrix(c(scoreNegative, scorePositive), ncol = 1)
#' colnames(predictor) = c("DET")
#' detCurve = detc(response,
#'                 predictor,
#'                 names = colnames(predictor),
#'                 positive = "target")
#' plotROC(detCurve@detCurves$DET,
#'          main = "Example")
plotROC = function (dets, ...) {
  plotROCs(new("DETs", detCurves = list(detCurve = dets)), ...)
}


#' EER Plot
#'
#' From a 'DETs' object, this function plots the EER points of each classifier on the same graph as the DET curves.
#' @param dets An object of class "DETs".
#' @param pch Symbol used for plotting the EER points, by default \code{pch = 19}.
#' @param col A vector of colors, specifying the color of the points for each DET curve.
#' @param lwd Line width used for drawing symbols, by default \code{lwd = 3}.
#' @param ... Further graphical arguments passed to the \code{plot} function.
#' @param ... Further graphical arguments passed to the \code{points} function. For example, by default \code{pch = 19} and \code{lwd = 3} (see \code{\link{points}}, for more details).
#' @export
#' @importFrom graphics points
#' @importFrom stats qnorm
#' @examples
#' library(DET)
#' n = 5000
#' # Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative1 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(5321)
#' scoreNegative2 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(6987)
#' scoreNegative3 = rnorm(n, mean = 0.25, sd = 0.125)
#' set.seed(11452)
#' scorePositive1 = rnorm(n, mean = 0.55, sd = 0.125)
#' set.seed(54321)
#' scorePositive2 = rnorm(n, mean = 0.65, sd = 0.125)
#' set.seed(65987)
#' scorePositive3 = rnorm(n, mean = 0.75, sd = 0.125)
#' response = as.factor(c(rep(c("target"), times = n), rep(c("nontarget"), times = n)))
#' predictor1 = c(scoreNegative1, scorePositive1)
#' predictor2 = c(scoreNegative2, scorePositive2)
#' predictor3 = c(scoreNegative3, scorePositive3)
#' predictors = matrix(c(predictor1, predictor2, predictor3), ncol = 3)
#' colnames(predictors) = c("DET1", "DET2", "DET3")
#' detCurves = detc(
#'   response,
#'   predictors,
#'   positive = "target",
#'   names = colnames(predictors),
#'   plot = TRUE,
#'   main = "Example",
#'   col = c("black", "blue", "red"))
#'
#' pointsEER(detCurves)
pointsEER = function (dets,
                      pch = 19,
                      col = c("black", "blue", "red", "green", "yellow"),
                      lwd = 3,
                      ...) {
  detCurves = dets@detCurves
  ncurves = length(detCurves)
  if (ncurves > length(col)) {
    col = rep(col, ceiling(ncurves / length(col)))
  }
  for (i in seq(ncurves)) {
    points(
      qnorm(detCurves[[i]]@eer),
      qnorm(detCurves[[i]]@eer),
      pch = 19,
      col = col[i],
      lwd = 3,
      ...
    )
  }
}
