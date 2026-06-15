# DET 3.0.3

* Added and updated maintenance files:
  - `show.R`: corrected display behaviour for DET objects.
  - `measures.R`: added objects to the output of `minDcf()`.
  - `plot.R`: corrected DET and ROC curve plots with confidence intervals, axis labels, and zoom options.
  - `detc.R`: corrected the names of the positive and negative labels.

* Small code modernisation and polish.

# DET 3.0.2

* Minor changes to restore DET to CRAN.

# DET 3.0.0

* Added class objects to model DET curves.

* Overrode the standard `plot()` function when called with a `DET` or `DETs` object.

* Renamed `printDET()` to `show()`.

* Added new functions to compute performance measures: `EER()` and `minDcf()`.

* Added new functions to plot the corresponding ROC curve from a DET curve: `plotROC()` and `plotROCs()`.

# DET 2.0.1

* `printDET()` shows the names of the classifiers used to access the DET object.

* `detc()` and `detc.CI()` do not plot the EER. The examples describe how to do this.

* Added the `speaker` and `ovarianCancer` data sets to provide real examples of DET curves.

# DET 2.0.0

* Added `printDET()` for describing the DET returned parameters.

* Renamed `det()` and `det.CI()` as `detc()` and `detc.CI()` to avoid overriding existing functions.

* New `detc()` and `detc.CI()` functions now provide an estimate of the Equal Error Rate in the returned data frame.

# DET 1.0.1

* Updated the appearance of ROC curve plots.

* `det()` and `det.CI()` now include `xlim` and `ylim` arguments to select the axis limits of the plot.
