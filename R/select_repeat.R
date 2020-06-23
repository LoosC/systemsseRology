#' Title
#'
#' @param X
#' @param y
#' @param selector
#' @param trials
#' @param threshold
#'
#' @return
#' @export select_repeat
#'
#' @examples
select_repeat <- function(X, y, selector, trials = 100, threshold = 0.8) {

  feature_count <- matrix(0L, nrow = 1, ncol = dim(X)[2])
  colnames(feature_count) <- colnames(X)

  for (trial in 1:trials) {
    features <- selector(X, y)
    feature_count[1, features] <- 1 + feature_count[1, features]
  }

  above_threshold <- feature_count[1, which(feature_count[1,] > threshold * trials)]
  return(colnames(above_threshold))
}
