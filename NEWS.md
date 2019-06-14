# DET 2.0.1

* `printDET()` shows the names of the classifiers to access de DET object.

* `detc()` and `detc.CI()` do not plot the EER. How to to that is described at the example

* `speaker` and `ovarianCancer` databases are added for complementing real examples of using DET curves.



# DET 2.0.0

* New function `printDET()` for describing the DET returned parameters.

* `det()` and `det.CI()` are renamed as `detc()` and `detc.CI()` to avoid overriding.

* New `detc()` and `detc.CI()` functions now provide an estimation for the Equal Error Rate (new field at the returned dataframe)


# DET 1.0.1

* New aspect of ROC Curve plot.

* `det()` and `det.CI()` now inlcude *xlim* and *ylim* arguments to select the axes limit of the plot.


