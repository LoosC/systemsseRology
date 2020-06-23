#' Accuracy
#'
#' @param y1
#' @param y2
#'
#' @return
#' @export
#'
#' @examples
score_accuracy <- function(y1, y2) {
  num1 <- as.numeric(y1)
  num2 <- as.numeric(y2)
  correct <- which(num1 == num2)
  return(length(correct) / length(y1))
}
