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
  # vector counting how often each feature is selected
  feature_count <- rep(0, dim(X)[2])
  names(feature_count) <- colnames(X)

  # run the feature selector trials times and increment the counters
  # for the features that are selected
  for (trial in 1:trials) {
    features <- selector(X, y)
    feature_count[features] <- 1 + feature_count[features]
  }

  # keep those features that were selected in more than threshold
  # percent of the trials
  selected <- feature_count[-which(feature_count <= threshold * trials)]

  # if no features satisfy this requirement, return NULL instead
  if (length(selected) == 0) {
    return(NULL)
  } else {
    return(names(selected))
  }
}
