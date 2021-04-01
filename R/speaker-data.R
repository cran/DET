#' Data on a Speaker Recognition System (Voxceleb verification test)
#'
#' For our experiments, we have used the Voxceleb database, which contains more than one hundred thousand utterances
#' extracted from Youtube interview videos. The database includes training and test sets that can be used for speaker
#' recognition system development and performance evaluation respectively. The testing protocol consists of a list of
#' utterance pairs, with the corresponding target or nontarget, and the task is to detect whether the two utterances 
#' belong to the same speaker or to different ones. 
#'
#' @docType data
#'
#' @usage data(speaker)
#'
#' @format An object of class "data.frame".
#'
#' @keywords datasets speaker recognition system 
#'
#' @references Nagraniy A, Chungy JS, Zisserman A (2017). Proceedings of the Annual Conference of the International Speech Communication Association, 950:2616–2620
#' (\href{https://www.robots.ox.ac.uk/~vgg/publications/2017/Nagrani17/nagrani17.pdf}{Publication})
#'
#' @source \href{https://www.robots.ox.ac.uk/~vgg/data/voxceleb/vox1.html}{Web Archive}
#'
#' @examples
#' library(DET)
#' data(speaker)
#' scoresLDA = speaker$scoresLDA
#' scoresPLDA = speaker$scoresPLDA
#' scoresLDAPLDA = speaker$scoresLDAPLDA
#' predictors = matrix(c(as.numeric(scoresLDA),
#'                       as.numeric(scoresPLDA),
#'                       as.numeric(scoresLDAPLDA)), ncol = 3)
#' colnames(predictors) = c("LDA", "PLDA", "LDAandPLDA")
#' response = as.factor(speaker$keys$V3)
#' detCurves =
#'   detc(
#'     response,
#'     predictors,
#'     names = colnames(predictors),
#'     positive = "target"
#'   )
#' plot(detCurves, main = "Voxceleb verification test")
#' 
"speaker"