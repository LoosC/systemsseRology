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

  feature_count <- rep(0, dim(X)[2])
  names(feature_count) <- colnames(X)

  for (trial in 1:trials) {
    features <- selector(X, y)
    feature_count[features] <- 1 + feature_count[features]
  }

  selected <- feature_count[-which(feature_count <= threshold * trials)]

  if (length(selected) == 0) {
    return(NULL)
  } else {
    return(names(selected))
  }
}
