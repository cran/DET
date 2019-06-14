#' Speaker Recognition System (Voxceleb verification test)
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
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets ovarian cancer
#'
#' @references Nagraniy A, Chungy JS, Zisserman A (2017). Proceedings of the Annual Conference of the International Speech Communication Association, 950:2616–2620
#' (\href{http://www.robots.ox.ac.uk/~vgg/publications/2017/Nagrani17/nagrani17.pdf}{Publication})
#'
#' @source \href{http://www.robots.ox.ac.uk/~vgg/data/voxceleb/vox1.html}{Web Archive}
#'
#' @examples
#' data(speaker)
#' scoresLDA <-speaker$scoresLDA
#' scoresPLDA <-speaker$scoresPLDA
#' scoresLDAPLDA <- speaker$scoresLDAPLDA
#' responses <- data.frame(speaker$keys$V3)
#' predictors <- data.frame(
#'   n1 = as.numeric(scoresLDA),
#'   n2 = as.numeric(scoresPLDA),
#'   n3 = as.numeric(scoresLDAPLDA)
#' )
#' detcurve <- detc(responses,predictors, 
#'                  names = c("LDA+DC","PLDA","LDA+PLDA"),
#'                  positive = "target",
#'                  title = "Voxceleb verification test",
#'                  plotROC = TRUE)
#' 
"speaker"