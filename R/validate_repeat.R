#' Title
#'
#' @param X n_samples x n_features matrix
#' @param y vector of labels
#' @param method method list with train, predict, score, and optionally select
#' @param options options list
#' @param n_trials number of trials (repetitions) for validate()
#'
#' @return list of lists with cv scores, rf scores, pt scores from validate()
#' @export
#'
#' @examples
validate_repeat <- function(X, y, method, options, n_trials = 100) {
  # we run validate() n_trials times, returning the vector of real scores
  # and the matrices of random feature scores/permutation test scores

  return_vals <- list()

  for (trial in 1:n_trials) {
    message(paste("validate_repeat: trial", trial, "/", n_trials))
    val_scores <- validate(X, y, method, options)
    # remove the actual prediction from the validation
    val_scores["cv_y"] <- NULL
    return_vals[[trial]] <- val_scores
  }

  return(return_vals)
}
