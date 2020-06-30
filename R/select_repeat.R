#' Repeat the feature selection and return features that are selected more
#' often than a provided threshold
#'
#' @param X n_samples x n_features matrix
#' @param y vector of labels
#' @param selector feature selection method
#' @param options options list

#' @return
#' @export
select_repeat <- function(X, y, selector, options = list()) {

  if (!("n_trials" %in% names(options))) {
    options$n_trials <- 100
  }
  if (!("threshold" %in% names(options))) {
    options$threshold <- 0.8
  }
  if (!("return_count" %in% names(options))) {
    options$return_count <- FALSE
  }


  # vector counting how often each feature is selected
  feature_count <- rep(0, ncol(X))
  names(feature_count) <- colnames(X)

  # run the feature selector trials times and increment the counters
  # for the features that are selected
  for (trial in 1:options$n_trials) {
    features <- selector(X, y)
    feature_count[features] <- 1 + feature_count[features]
  }

  # keep those features that were selected in more than threshold
  # percent of the trials
  selected <- feature_count[-which(feature_count <= options$threshold * options$n_trials)]

  # if no features satisfy this requirement, return NULL instead
  if (options$return_count) {
    return(list(feature_count = feature_count, sel_features = names(selected)))
  } else {
    if (length(selected) == 0) {
      return(NULL)
    } else {
      return(names(selected))
    }
  }
}
