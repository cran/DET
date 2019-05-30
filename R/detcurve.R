




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
#' @param xlim numeric vector of length 2, giving the x coordinates range.
#' @param ylim numeric vector of length 2, giving the y coordinates range.
#' @return A list of dataframe, one per classifier. Each dataframe contains the name of the pair, and the 
#' parameters of the DET curve (FPR, the median of FNR and the upper and lower extremes for the CI, and the thresholds used), along with the Equal Error Rate (%).
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
#' detcurve <- detc.CI(responses,predictors, 
#'                    names = names(predictors),
#'                    title = "Example with CI",
#'                    positive="target",
#'                    parallel = TRUE)
#'}
#' @export
#' @import pROC
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom grDevices col2rgb dev.off pdf rgb
#' @importFrom graphics axis grid legend lines plot points polygon par
#' @importFrom stats qnorm
detc.CI <-function(responses,predictors,conf=0.95,names=c(""),positive = "",title = "",legend = "topright",parallel = FALSE,ncores = 2,file, plotROC = FALSE,xlim = c(0.05,50),ylim = c(0.05,50)){
  if (length(responses) == 0 || length(predictors) == 0 ) {
    stop("'responses' or 'predictors' are empty dataframes")
  }
  if (length(responses) > 1 &&  length(responses) != length(predictors)  ) {
    stop("Different number of 'responses' and 'predictors'")
  }
  if (xlim[1] >= xlim[2] || ylim[1] >= ylim[2] ) {
    stop("Bad X or Y ranges")
  }
  if (xlim[1] < 0 || ylim[1] < 0 ) {
    stop("xlim and ylim have to be between 0 and 100")
  }
  if (xlim[1] == 0) xlim[1] =  0.05
  if (xlim[2] == 100) xlim[2] =  99.95
  if (ylim[1] == 0) ylim[1] =  0.05
  if (ylim[2] == 100) ylim[2] =  99.95
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
  if(!missing(file) && plotROC) {pdf(paste(file,"ROC.pdf",sep=""), width = 10, height = 10)}
  #Draw ROC Curves with IC
  if(plotROC){
    plot(x = NaN,y = NaN,type = 'n',
         xlab = '1 - Specificity',xlim = c(-0.1,1.1), ylim  = c(-0.1,1.1),ylab = 'Sensitivity',
         panel.first = grid(nx = 20, ny = 20),main = paste(title,"ROC Curves"))
    for (i in seq(ncurves)) {
        fpr=1-ROCs[[i]]$specificities
        tpr1 = as.numeric(ICsens[[i]][,1])
        tpr0 = as.numeric(ICsens[[i]][,2])
        tpr2 = as.numeric(ICsens[[i]][,3])
        x = fpr
        y0 = tpr0
        y1 = tpr1
        y2 = tpr2
        x[x==Inf] = 1
        x[x==-Inf]= 0
        y1[y1==-Inf]= 0
        y2[y2==-Inf]= 0
        x = c(1,x,0)
        y0 = c(1,y0,0)
        y1 = c(1,y1,0)
        y2 = c(1,y2,0)
        lines(x, y0, col = colores[i], lwd=2 )
        points(x,y1,type="l",col=colores[i])
        points(x,y2,type="l",col=colores[i])
        polygon(c(x,rev(x)),c(y2,rev(y1)),
                col=rgb(col2rgb(coloreslight[i])[1]/255, col2rgb(coloreslight[i])[2]/255, col2rgb(coloreslight[i])[3]/255,0.2),
                border = NA)
    }
    lines(seq(0,1,0.1), seq(0,1,0.1), col = "gray",lty=6)
    if(!is.null(legend)){
      legend("bottomright", legend=names,
             col=colores[1:ncurves], lty=1, cex=0.7)
    }
  }

  if(!missing(file) && plotROC) {dev.off()}

  if(!missing(file)) {pdf(paste(file,"DET.pdf",sep=""), width = 10, height = 10)}
  #Draw DET Curves with IC
  lims_x = qnorm(xlim/100)
  lims_y = qnorm(ylim/100)
  axises_x = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, seq(0.10,1,0.10))
  labels_x = c(0.1, 0.2, 0.5, 1, 2, 5, seq(10,100,10))
  axises_y = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, seq(0.10,1,0.10))
  labels_y = c(0.1, 0.2, 0.5, 1, 2, 5, seq(10,100,10))
  interval = c(1,length(labels_x))
  while(labels_x[interval[1]] < xlim[1] || labels_x[interval[2]] > xlim[2] ){
    if(labels_x[interval[1]] < xlim[1]) interval[1] = interval[1]+1
    if(labels_x[interval[2]] > xlim[2]) interval[2] = interval[2]-1
  }
  axises_x = c(xlim[1]/100,axises_x[interval[1]:interval[2]],xlim[2]/100)
  labels_x = c(xlim[1],labels_x[interval[1]:interval[2]],xlim[2])
  interval = c(1,length(labels_y))
  while(labels_y[interval[1]] < ylim[1] || labels_y[interval[2]] > ylim[2] ){
    if(labels_y[interval[1]] < ylim[1]) interval[1] = interval[1]+1
    if(labels_y[interval[2]] > ylim[2]) interval[2] = interval[2]-1
  }
  axises_y = c(ylim[1]/100,axises_y[interval[1]:interval[2]],ylim[2]/100)
  labels_y = c(ylim[1],labels_y[interval[1]:interval[2]],ylim[2])
  plot(x = NaN,y = NaN,type = 'n',
       xlab = 'FPR(%)',xlim = lims_x, ylab = 'FNR(%)'
       ,ylim = lims_y,xaxt='n',yaxt='n',panel.first = grid(nx = 20, ny = 20),main = paste(title,"DET Curves"))
  axis(1, at=qnorm(axises_x),labels=labels_x)
  axis(2, at=qnorm(axises_y),labels=labels_y)
  lines(seq(-100,100,0.1), seq(-100,100,0.1), col = "gray",lty=6)
  lines(seq(-4,4,0.01), -seq(-4,4,0.01), col = "gray",lty=6)
  for (i in seq(ncurves)) {
    fpr=1-ROCs[[i]]$specificities
    fnr1 = 1-as.numeric(ICsens[[i]][,1])
    fnr0 = 1-as.numeric(ICsens[[i]][,2])
    fnr2 = 1-as.numeric(ICsens[[i]][,3])
    EER0 = 0.5*fpr[which.min(abs(fpr-fnr0))]+0.5*fnr0[which.min(abs(fpr-fnr0))]
    EER1 = 0.5*fpr[which.min(abs(fpr-fnr1))]+0.5*fnr1[which.min(abs(fpr-fnr1))]
    EER2 = 0.5*fpr[which.min(abs(fpr-fnr2))]+0.5*fnr2[which.min(abs(fpr-fnr2))]
    points(qnorm(EER0),qnorm(EER0),pch=19,col = colores[i],lwd=2.5)
    points(qnorm(EER1),qnorm(EER1),pch=20,col = colores[i],lwd=2.5)
    points(qnorm(EER2),qnorm(EER2),pch=20,col = colores[i],lwd=2.5)
    
    x = qnorm(fpr)
    y0 = qnorm(fnr0)
    y1 = qnorm(fnr1)
    y2 = qnorm(fnr2)
    x[x==Inf] = big_integer
    x[x==-Inf]= -big_integer
    y0[y0==-Inf]= -big_integer
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
      fnr_upper = fnr2,
      Thresholds = Thresholds[[i]]
    )
    Info <- list(
      DET = DET,
      conf = conf,
      EER_median = EER0,
      EER_lower = EER1,
      EER_upper = EER2
    )
    names(Info) = c("DET","conf","EER_median","EER_lower","EER_upper")
    ListInfo[[length(ListInfo)+1]] <- Info
  }
  if(!is.null(legend)){
    legend(legend,legend=names,
           col=colores[1:ncurves], lty=1, cex=0.7)
  }
  if(!missing(file)) {dev.off()}
  names(ListInfo) = names
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
#' @param xlim numeric vector of length 2, giving the x coordinates range.
#' @param ylim numeric vector of length 2, giving the y coordinates range.
#' @return A list of dataframe, one per classifier. Each dataframe contains the name of the classifier, and the 
#' parameters of the DET curve (FPR, FNR and the thresholds used), along with the Equal Error Rate (%).
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
#' detcurve <- detc(responses,predictors,
#'                 names = names(predictors),
#'                 positive="target",
#'                 title="Example",
#'                 plotROC = TRUE)
#'  
#'
#' @export
#' @import pROC
#' @importFrom grDevices col2rgb dev.off pdf rgb
#' @importFrom graphics axis grid legend lines plot points polygon par
#' @importFrom stats qnorm
detc <-function(responses,predictors,names=c(""),positive="",title = "",legend = "topright",file, plotROC = FALSE,xlim = c(0.05,50),ylim = c(0.05,50)){
  if (length(responses) == 0 || length(predictors) == 0 ) {
    stop("'responses' or 'predictors' are empty dataframes")
  }
  if (xlim[1] >= xlim[2] || ylim[1] >= ylim[2] ) {
    stop("Bad X or Y ranges")
  }
  if (xlim[1] < 0 || ylim[1] < 0 ) {
    stop("xlim and ylim have to be between 0 and 100")
  }
  if (xlim[1] == 0) xlim[1] =  0.05
  if (xlim[2] == 100) xlim[2] =  99.95
  if (ylim[1] == 0) ylim[1] =  0.05
  if (ylim[2] == 100) ylim[2] =  99.95
  
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
  if(!missing(file) && plotROC) {pdf(paste(file,"ROC.pdf",sep=""), width = 10, height = 10)}
  #Draw ROC Curves 
  if(plotROC){
    for (i in seq(ncurves)) {
      if(i==1){
        plot(1-ROCs[[i]]$specificities,ROCs[[i]]$sensitivities,type="l",
             panel.first = grid(nx = 20, ny = 20),col=colores[i], 
             main = paste("ROC Curves.",title),xlim = c(-0.1,1.1),
             xlab = '1 - Specificity', ylab = 'Sensitivity',lwd=2)
      }else{
        lines(1-ROCs[[i]]$specificities,ROCs[[i]]$sensitivities,lty=1, col=colores[i],lwd=2)
      }
    }
    lines(seq(0,1,0.1), seq(0,1,0.1), col = "gray",lty=6)
    
    if(!is.null(legend)){
    legend("bottomright", legend=names,
           col=colores[1:ncurves], lty=1, cex=0.7)
    }
  }

  if(!missing(file) && plotROC) {dev.off()}
  if(!missing(file)) {pdf(paste(file,"DET.pdf",sep=""), width = 10, height = 10)}
  #Draw DET Curves
  
  lims_x = qnorm(xlim/100)
  lims_y = qnorm(ylim/100)
  axises_x = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, seq(0.10,1,0.10))
  labels_x = c(0.1, 0.2, 0.5, 1, 2, 5, seq(10,100,10))
  axises_y = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, seq(0.10,1,0.10))
  labels_y = c(0.1, 0.2, 0.5, 1, 2, 5, seq(10,100,10))
  interval = c(1,length(labels_x))
  while(labels_x[interval[1]] < xlim[1] || labels_x[interval[2]] > xlim[2] ){
    if(labels_x[interval[1]] < xlim[1]) interval[1] = interval[1]+1
    if(labels_x[interval[2]] > xlim[2]) interval[2] = interval[2]-1
  }
  axises_x = c(xlim[1]/100,axises_x[interval[1]:interval[2]],xlim[2]/100)
  labels_x = c(xlim[1],labels_x[interval[1]:interval[2]],xlim[2])
  interval = c(1,length(labels_y))
  while(labels_y[interval[1]] < ylim[1] || labels_y[interval[2]] > ylim[2] ){
    if(labels_y[interval[1]] < ylim[1]) interval[1] = interval[1]+1
    if(labels_y[interval[2]] > ylim[2]) interval[2] = interval[2]-1
  }
  axises_y = c(ylim[1]/100,axises_y[interval[1]:interval[2]],ylim[2]/100)
  labels_y = c(ylim[1],labels_y[interval[1]:interval[2]],ylim[2])
  plot(x = NaN,y = NaN,type = 'n',
       xlab = 'FPR(%)',xlim = lims_x, ylab = 'FNR(%)'
       ,ylim = lims_y,xaxt='n',yaxt='n',panel.first = grid(nx = 20, ny = 20),
       main = paste("DET Curves.",title))
  axis(1, at=qnorm(axises_x),labels=labels_x)
  axis(2, at=qnorm(axises_y),labels=labels_y)
  lines(seq(-100,100,0.1), seq(-100,100,0.1), col = "gray",lty=6)
  lines(seq(-4,4,0.01), -seq(-4,4,0.01), col = "gray",lty=6)
  for (i in seq(ncurves)) {
    fpr=1-ROCs[[i]]$specificities
    fnr = 1-ROCs[[i]]$sensitivities
    EER = 0.5*fpr[which.min(abs(fpr-fnr))]+0.5*fnr[which.min(abs(fpr-fnr))]
    x = qnorm(fpr)
    y = qnorm(fnr)
    x[x==Inf] = big_integer
    x[x==-Inf]= -big_integer
    y[y==-Inf]= -big_integer
    y[y==Inf]= big_integer
    lines(x, y, col = colores[i],lwd=2)
    points(qnorm(EER),qnorm(EER),pch=19,col = colores[i],lwd=3)
    DET <- data.frame(
      fpr = fpr,
      fnr = fnr,
      Thresholds = Thresholds[[i]]
    )
    Info <- list(DET,EER)
    names(Info) = c("DET","EER")
    ListInfo[[length(ListInfo)+1]] <- Info
  }
  if(!is.null(legend)){
    legend(legend,legend=names,
           col=colores[1:ncurves], lty=1, cex=0.7)
  }
  if(!missing(file)) {dev.off()}
  names(ListInfo) = names
  return(ListInfo)
}








