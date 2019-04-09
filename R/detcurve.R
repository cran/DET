




#' DET Curve calculation with Confidence Interval
#' 
#' From a dataframe of responses and predictors, the function calculates and plots DET curves for each pair (response,predictor), with a Confidence Interval. In addition, ROC curves can be displayed by activating a boolean argument.
#' @param responses A dataframe of factor, numeric or character vector of responses, typically encoded with 0 (non-target) and 1 (target).  By default, the first two values of levels(as.factor(response)) are taken. If only one response is passed, 
#' it will be used for calculating all the curves.
#' @param predictors A dataframe of numeric vector of the same length than response, containing the predicted value of each observation. An ordered factor is coerced to a numeric.
#' 
#' @param conf The width of the confidence interval as [0,1]. Default: 0.95 (95\% CI).
#' 
#' @param names Array of strings, containing the name of each pair (response,predictor) that will appear in the legend of the graph. 
#' @param positive string with the name of the 'positive' class. Default: negative class will be the first string of response levels and positive class the second string.
#' @param title Main tile for the graph
#' @param legend the location of the leyend in the graph. Could be a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center" and NULL. Default: "topright".
#' @param parallel if TRUE, the bootstrap is processed in parallel, using parallel backend provided by plyr (foreach).
#' @param ncores The number of nodes to be forked for the parallel computation. Default: 2.
#' @param file The name of the file where the plot will be saved . If Empty, the DET and ROC curves are plotted by the graphic output.
#' @param plotROC Boolean specifying ploting or not the ROC Curve
#' @return A list of dataframe, one per classifier. Each dataframe contains the name of the pair, and the 
#' parameters of the DET curve (FPR, the median of FNR and the upper and lower extremes for the CI, and the thresholds used)
#' @examples
#' \donttest{
#' n <- 500
#' #Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative <- rnorm(n, mean = 0.25,sd = 0.125) 
#' set.seed(11452)
#' scorePositive1 <- rnorm(n, mean = 0.55,sd = 0.125)
#' set.seed(54321)
#' scorePositive2 <- rnorm(n, mean = 0.65,sd = 0.125)
#' response = c(rep(c("target"), times = n),rep(c("nontarget"), times = n))
#' predictor1 = c(scoreNegative,scorePositive1)
#' predictor2 = c(scoreNegative,scorePositive2)
#' responses <- data.frame(
#'   response = response
#' )
#' predictors <- data.frame(
#'   DET1 = predictor1,
#'   DET2 = predictor2
#' )
#' #Run in parallel for a faster execution (takes about 3-5 min in a 2018 laptop) activating 
#' #logical argument 'parallel'
#' detcurve <- det.CI(responses,predictors, 
#'                    names = names(predictors),
#'                    title = "Example with CI",
#'                    positive="target",
#'                    parallel = TRUE)
#'}
#' @export
#' @import pROC
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom grDevices col2rgb dev.off png rgb
#' @importFrom graphics axis grid legend lines plot points polygon par
#' @importFrom stats qnorm
det.CI <-function(responses,predictors,conf=0.95,names=c(""),positive = "",title = "",legend = "topright",parallel = FALSE,ncores = 2,file, plotROC = FALSE){
  if (length(responses) == 0 || length(predictors) == 0 ) {
    stop("'responses' or 'predictors' are empty dataframes")
  }
  if (length(responses) > 1 &&  length(responses) != length(predictors)  ) {
    stop("Different number of 'responses' and 'predictors'")
  }
  colores = c("black","blue","red","green","yellow")
  coloreslight = c("lightgray","lightblue","lightpink","lightgreen","lightyellow")
  big_integer = 2147483647 #Constant for ploting DET Curve
  if(class(responses) != "data.frame" || class(predictors) != "data.frame"){
    stop("Bad type in arguments. Responses and predictors must be of class 'data.frame'")
  }
  response = list()
  for (i in colnames(responses)){
    response[[length(response)+1]] <- responses[[i]] 
  }
  predictor = list()
  for (i in colnames(predictors)){
    predictor[[length(predictor)+1]] <- predictors[[i]] 
  }
  ncurves = length(predictor)
  ROCs = list()
  ICsens = list()
  Thresholds = list()
  ListInfo = list()
  if(parallel){
      cat("Using",ncores,"cores \n")
      for (i in seq(ncurves)) {
        cat("Calculating DET Curves with CI for:",names[i],"\n")
        if(length(response) == 1){
          levels=levels(as.factor(response[[1]]))
        }else{
          levels=levels(as.factor(response[[i]]))
        }
        if (length(levels) != 2) {
          stop("'responses' must have two levels")
        }
        if (positive != ""){
          if (positive != levels[1] && positive != levels[2]) {
            stop("positive class not in predictor levels")
          }
          if(positive != levels[2]){
            levels=rev(levels)
          }
        }
        registerDoParallel(cl <- makeCluster(ncores))
        if(length(response) == 1){
          curvaROC<-roc(response[[1]],predictor[[i]],levels=levels)}
        else{
          curvaROC<-roc(response[[i]],predictor[[i]],levels=levels)
        }
        ROCs[[length(ROCs)+1]] <- curvaROC
        Thresholds[[length(Thresholds)+1]] <- curvaROC$thresholds
        ICsen<-ci.se(curvaROC,specificities = curvaROC$specificities,conf.level=conf,method="bootstrap",parallel=TRUE)
        ICsens[[length(ICsens)+1]] <- ICsen
        stopCluster(cl)
      }
  }else{
    for (i in seq(ncurves)) {
      if(length(response) == 1){
        levels=levels(as.factor(response[[1]]))
      }else{
        levels=levels(as.factor(response[[i]]))
      }
      if (length(levels) != 2) {
        stop("'responses' must have two levels")
      }
      if (positive != ""){
        if (positive != levels[1] && positive != levels[2]) {
          stop("positive class not in predictor levels")
        }
        if(positive != levels[2]){
          levels=rev(levels)
        }
      }
      if(length(response) == 1){
        curvaROC<-roc(response[[1]],predictor[[i]],levels=levels)}
      else{
        curvaROC<-roc(response[[i]],predictor[[i]],levels=levels)
      }
      ROCs[[length(ROCs)+1]] <- curvaROC
      Thresholds[[length(Thresholds)+1]] <- curvaROC$thresholds
      ICsen<-ci.se(curvaROC,specificities = curvaROC$specificities,conf.level=conf,method="bootstrap")
      ICsens[[length(ICsens)+1]] <- ICsen
    }

  }
  if(!missing(file) && plotROC) {png(paste(file,"ROC.png",sep=""), width = 540, height = 540)}
  #Draw ROC Curves with IC
  if(plotROC){
    for (i in seq(ncurves)) {
      if(i==1){
        plot(ROCs[[i]],col=colores[i],main = paste("ROC Curves",title))
        plot(ICsens[[i]],type="shape",col=rgb(col2rgb(coloreslight[i])[1]/255, col2rgb(coloreslight[i])[2]/255, col2rgb(coloreslight[i])[3]/255,0.2))
      }else{
        plot(ROCs[[i]],add=TRUE, col=colores[i])
        plot(ICsens[[i]],type="shape",col=rgb(col2rgb(coloreslight[i])[1]/255, col2rgb(coloreslight[i])[2]/255, col2rgb(coloreslight[i])[3]/255,0.2))
      }
    }
    if(!is.null(legend)){
      legend("bottomright", legend=names,
             col=colores[1:ncurves], lty=1, cex=0.7)
    }
  }

  if(!missing(file) && plotROC) {dev.off()}

  if(!missing(file)) {png(paste(file,"DET.png",sep=""), width = 540, height = 540)}
  #Draw DET Curves with IC
  lims = qnorm(c(0.0005,0.5))
  axises = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50)
  labels = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30, 40, 50)
  plot(x = NaN,y = NaN,type = 'n',
       xlab = 'FPR(%)',xlim = lims, ylab = 'FNR(%)'
       ,ylim = lims,xaxt='n',yaxt='n',panel.first = grid(nx = 20, ny = 20),main = paste("DET Curves",title))
  axis(1, at=qnorm(axises),labels=labels)
  axis(2, at=qnorm(axises),labels=labels)
  for (i in seq(ncurves)) {
    fpr=1-ROCs[[i]]$specificities
    fnr1 = 1-as.numeric(ICsens[[i]][,1])
    fnr0 = 1-as.numeric(ICsens[[i]][,2])
    fnr2 = 1-as.numeric(ICsens[[i]][,3])
    x = qnorm(fpr)
    y0 = qnorm(fnr0)
    y1 = qnorm(fnr1)
    y2 = qnorm(fnr2)
    x[x==Inf] = big_integer
    x[x==-Inf]= -big_integer
    y1[y1==-Inf]= -big_integer
    y2[y2==-Inf]= -big_integer
    lines(x, y0, col = colores[i], lwd=2 )
    points(x,y1,type="l",col=colores[i])
    points(x,y2,type="l",col=colores[i])
    polygon(c(x,rev(x)),c(y2,rev(y1)),
            col=rgb(col2rgb(coloreslight[i])[1]/255, col2rgb(coloreslight[i])[2]/255, col2rgb(coloreslight[i])[3]/255,0.2),
            border = NA)
    DET <- data.frame(
      fpr = fpr,
      fnr_lower = fnr1,
      fnr_median = fnr0,
      fnr_upper = fnr2
    )
    Info <- data.frame(
      DET = DET,
      name = names[i],
      Thresholds = Thresholds[[i]]
    )
    ListInfo[[length(ListInfo)+1]] <- Info
  }
  if(!is.null(legend)){
    legend(legend,legend=names,
           col=colores[1:ncurves], lty=1, cex=0.7)
  }
  if(!missing(file)) {dev.off()}

  return(ListInfo)
}


#' DET Curve calculation 
#' 
#' From a dataframe of responses and predictors, the function calculates and plots the DET curves for each pair (response,predictor). In addition, ROC curves can be displayed by activating a boolean argument.
#' @param responses A dataframe of factor, numeric or character vector of responses, typically encoded with 0 (non-target) and 1 (target).  By default, the first two values of levels(as.factor(response)) are taken. If only one response is passed, 
#' it will be used for calculating all the curves.
#' @param predictors A dataframe of numeric vector of the same length than response, containing the predicted value of each observation. An ordered factor is coerced to a numeric.
#' @param names Array of strings, containing the name of each pair (response,predictor) that will appear in the legend of the graph. 
#' @param positive string with the name of the 'positive' class. Default: negative class will be the first string of response levels and positive class the second string.
#' @param title Main tile for the graph
#' @param legend the location of the leyend in the graph. Could be a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center" and NULL. Default: "topright".
#' @param file The name of the file where the plot will be saved. If Empty, the DET and ROC curves are plotted by the graphic output.
#' @param plotROC Boolean specifying ploting or not the ROC Curve
#' @return A list of dataframe, one per classifier. Each dataframe contains the name of the classifier, and the 
#' parameters of the DET curve (FPR, FNR and the thresholds used)
#' @examples
#' n <- 5000
#' set.seed(12345)
#' #Predictors with normal distribution
#' set.seed(1235)
#' scoreNegative <- rnorm(n, mean = 0.25,sd = 0.125) 
#' set.seed(11452)
#' scorePositive1 <- rnorm(n, mean = 0.55,sd = 0.125)
#' set.seed(54321)
#' scorePositive2 <- rnorm(n, mean = 0.65,sd = 0.125)
#' set.seed(65987)
#' scorePositive3 <- rnorm(n, mean = 0.75,sd = 0.125) 
#' response = c(rep(c("target"), times = n),rep(c("nontarget"), times = n))
#' predictor1 = c(scoreNegative,scorePositive1)
#' predictor2 = c(scoreNegative,scorePositive2)
#' predictor3 = c(scoreNegative,scorePositive3)
#' responses <- data.frame(
#'   response = response
#' )
#' predictors <- data.frame(
#'   DET1 = predictor1,
#'   DET2 = predictor2,
#'   DET3 = predictor3
#' )
#' #We can also plot the ROC curves activating logical attribute 'plotROC'
#' detcurve <- det(responses,predictors,
#'                 names = names(predictors),
#'                 positive="target",
#'                 title="Example",
#'                 plotROC = TRUE)
#'
#' @export
#' @import pROC
#' @importFrom grDevices col2rgb dev.off png rgb
#' @importFrom graphics axis grid legend lines plot points polygon par
#' @importFrom stats qnorm
det <-function(responses,predictors,names=c(""),positive="",title = "",legend = "topright",file, plotROC = FALSE){
  if (length(responses) == 0 || length(predictors) == 0 ) {
    stop("'responses' or 'predictors' are empty dataframes")
  }
  if (length(responses) > 1 &&  length(responses) != length(predictors)  ) {
    stop("Different number of 'responses' and 'predictors'")
  }
  colores = c("black","blue","red","green","yellow")
  big_integer = 2147483647 #Constant for ploting DET Curve
  if(class(responses) != "data.frame" || class(predictors) != "data.frame"){
    stop("Bad type in arguments. Responses and predictors must be of class 'data.frame'")
  }
  response = list()
  for (i in colnames(responses)){
    response[[length(response)+1]] <- responses[[i]] 
  }
  predictor = list()
  for (i in colnames(predictors)){
    predictor[[length(predictor)+1]] <- predictors[[i]] 
  }
  ncurves = length(predictor)
  ROCs = list()
  ICsens = list()
  Thresholds = list()
  ListInfo = list()
  for (i in seq(ncurves)) {
    if(length(response) == 1){
      levels=levels(as.factor(response[[1]]))
    }else{
      levels=levels(as.factor(response[[i]]))
    }
    if (length(levels) != 2) {
      stop("'responses' must have two levels")
    }
    if (positive != ""){
      if (positive != levels[1] && positive != levels[2]) {
        stop("positive class not in predictor levels")
      }
      if(positive != levels[2]){
        levels=rev(levels)
      }
    }
    if(length(response) == 1){
      curvaROC<-roc(response[[1]],predictor[[i]],levels=levels)}
    else{
      curvaROC<-roc(response[[i]],predictor[[i]],levels=levels)
    }
    ROCs[[length(ROCs)+1]] <- curvaROC
    Thresholds[[length(Thresholds)+1]] <- curvaROC$thresholds
  }
  if(!missing(file) && plotROC) {png(paste(file,"ROC.png",sep=""), width = 540, height = 540)}
  #Draw ROC Curves 
  if(plotROC){
    for (i in seq(ncurves)) {
      if(i==1){
        plot(ROCs[[i]],col=colores[i], main = paste("ROC Curves",title))
      }else{
        plot(ROCs[[i]],add=TRUE, col=colores[i])
      }
    }
    if(!is.null(legend)){
    legend("bottomright", legend=names,
           col=colores[1:ncurves], lty=1, cex=0.7)
    }
  }

  if(!missing(file) && plotROC) {dev.off()}
  if(!missing(file)) {png(paste(file,"DET.png",sep=""), width = 540, height = 540)}
  #Draw DET Curves
  lims = qnorm(c(0.0005,0.5))
  axises = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50)
  labels = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 30, 40, 50)
  
  plot(x = NaN,y = NaN,type = 'n',
       xlab = 'FPR(%)',xlim = lims, ylab = 'FNR(%)'
       ,ylim = lims,xaxt='n',yaxt='n',panel.first = grid(nx = 20, ny = 20), main = paste("DET Curves",title))
  axis(1, at=qnorm(axises),labels=labels)
  axis(2, at=qnorm(axises),labels=labels)
  for (i in seq(ncurves)) {
    fpr=1-ROCs[[i]]$specificities
    fnr = 1-ROCs[[i]]$sensitivities
    x = qnorm(fpr)
    y = qnorm(fnr)
    x[x==Inf] = big_integer
    x[x==-Inf]= -big_integer
    y[y==-Inf]= -big_integer
    y[y==Inf]= big_integer
    lines(x, y, col = colores[i],lwd=2)
    DET <- data.frame(
      fpr = fpr,
      fnr = fnr
    )
    Info <- data.frame(
      DET = DET,
      name = names[i],
      Thresholds = Thresholds[[i]]
    )
    ListInfo[[length(ListInfo)+1]] <- Info
  }
  if(!is.null(legend)){
    legend(legend,legend=names,
           col=colores[1:ncurves], lty=1, cex=0.7)
  }
  if(!missing(file)) {dev.off()}
  
  return(ListInfo)
}







