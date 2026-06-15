# DET

DET builds Detection Error Tradeoff (DET) curves and Receiver Operating
Characteristic (ROC) curves for binary classification systems. It can be used to
summarise and compare classifier performance from one or more numeric predictor
scores.

## Maintainer note

This package was created by Alvaro Garcia-Rodenas, Manuel Franco,
Juana-Maria Vivo, Jesualdo T. Fernandez-Breis, and Roberto Font. James Curran is
the current maintainer to keep the package available on CRAN; credit belongs to
the original authors.

## Installation

Install the released version from CRAN:

```r
install.packages("DET")
```

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("jmcurran/DET")
```

## Basic use

`detc()` computes DET curves from a binary response and one or more numeric
predictors. The `positive` argument identifies the response level that should be
treated as the positive class.

```r
library(DET)

data(speaker)

predictors = matrix(
  c(
    as.numeric(speaker$scoresLDA),
    as.numeric(speaker$scoresPLDA),
    as.numeric(speaker$scoresLDAPLDA)
  ),
  ncol = 3
)
colnames(predictors) = c("LDA", "PLDA", "LDAandPLDA")

response = as.factor(speaker$keys$V3)

detCurves = detc(
  response = response,
  predictors = predictors,
  names = colnames(predictors),
  positive = "target"
)

show(detCurves)
plot(detCurves, main = "VoxCeleb verification test")
```

## ROC curves

The corresponding ROC curves can be plotted from the same `DETs` object.

```r
plotROCs(detCurves, main = "ROC curves")
```

## Performance measures

`EER()` computes the equal error rate from false positive and false negative
rates. `minDcf()` computes the minimum detection cost function from a single
`DET` object.

```r
firstCurve = detCurves@detCurves[[1]]

firstCurve@eer
minDcf(firstCurve)
```

## Confidence intervals

Confidence intervals can be requested with the `conf` argument. The bootstrap
calculation may take some time for larger data sets.

```r
detCurvesWithCi = detc(
  response = response,
  predictors = predictors,
  names = colnames(predictors),
  positive = "target",
  conf = 0.95,
  nboot = 2000
)

plot(detCurvesWithCi, main = "DET curves with confidence intervals")
```

## Main functions

- `detc()` computes one or more DET curves.
- `detc.CI()` computes confidence intervals for existing DET curves.
- `plot()` displays `DET` and `DETs` objects.
- `plotROC()` and `plotROCs()` display ROC curves.
- `EER()` computes the equal error rate.
- `minDcf()` computes the minimum detection cost function.
