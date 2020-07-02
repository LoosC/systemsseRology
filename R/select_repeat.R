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

  # ----------------- BEGIN OPTIONS ----------------- #
  # How often it should be repeated
  if (!("n_trials" %in% names(options))) {
    options$n_trials <- 100
  }
  if (!("threshold" %in% names(options))) {
    options$threshold <- 0.8
  }
  # returns the whole data frame of how often a feature was selected
  if (!("return_count" %in% names(options))) {
    options$return_count <- FALSE
  }
  if (!("force_select" %in% names(options))) {
    options$force_select <- TRUE
  }
  # ----------------- END OPTIONS ----------------- #

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

  # if a selection is forced, return the features which are selected the most
  if (length(selected) == 0 & options$force_select) {
    selected <- feature_count[which(feature_count == max(feature_count))]
  }

  if (options$return_count) {
    return(list(feature_count = feature_count, sel_features = names(selected)))
  } else {
    return(names(selected))
  }
}
