#' Accuracy
#'
#' @param y true labels
#' @param y_pred predicted labels
#'
#' @return classification accuracy
#' @export
score_accuracy <- function(y, y_pred) {
  num <- as.numeric(y)
  num_pred <- as.numeric(y_pred)
  correct <- which(num == num_pred)
  accuracy <- length(correct) / length(y)
  return(accuracy)
}


#' Mean squared error
#'
#' @param y true labels
#' @param y_pred predicted labels
#'
#' @return mean squared error
#' @export
score_mse <- function(y, y_pred) {
  num <- as.numeric(y)
  num_pred <- as.numeric(y_pred)
  mse <- mean((num - num_pred) ^ 2)
  return(mse)
}


#' R^2 (coefficient of determination)
#'
#' @param y true labels
#' @param y_pred predicted labels
#'
#' @return R2 value
#' @export
score_r2 <- function(y, y_pred) {
  num <- as.numeric(y)
  num_pred <- as.numeric(y_pred)
  y_bar <- mean(num)
  total_var <- sum((num - y_bar) ^ 2)
  residual_var <- sum((num - num_pred) ^ 2)
  r2 <- 1 - residual_var / total_var
  return(r2)
}
