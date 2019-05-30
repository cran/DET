
#' DET Curve information
#' 
#' From a DET curve list (generated with detc() or detc.CI()), the function shows the different atributtes of each curve with a little description.
#' @param dets list of DET curves originated by detc() or detc.CI() functions
#' @export
printDET <- function (dets){
  cat("**Parameters of the Detection Error Tradeoff (DET) Curves**\n\n")
  if (!is.null(dets[[1]]$conf)){
  cat("The",dets[[1]]$conf*100,"% Confidence Interval has been calculated\n\n")
  }
  cat("*The results are available in the following objects:\n\n")
  cat("name                description\n\n")
  cat("$DET                DET curve coordinates\n")
  cat("$DET$fpr            false positive rates\n")
  if (!is.null(dets[[1]]$conf)){
  cat("$DET$fnr_lower      lower extremes for false negative rates CI\n")
  cat("$DET$fnr_median     median for false negative rates CI\n")
  cat("$DET$fnr_upper      upper extremes for false negative rates CI\n")
  }else{
  cat("$DET$fnr            false negative rates\n")
  }
  cat("$DET$Thresholds     threasholds for each DET point\n")
  if (!is.null(dets[[1]]$conf)){
  cat("$conf               the width of the confidence interval as [0,1]\n")
  cat("$EER_median         median for EER CI\n")
  cat("$EER_lower          lower extreme of the EER CI\n")
  cat("$EER_upper          upper extreme of the EER CI\n")
  }else{
  cat("$EER                Equal Error Rate\n")
    
  }
  

}


