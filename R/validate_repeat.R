#' Title
#'
#' @param X
#' @param y
#' @param method
#' @param options
#' @param n_trials
#'
#' @return
#' @export
#'
#' @examples
validate_repeat <- function(X, y, method, options, n_trials = 100) {
  # we run validate() n_trials times, returning the vector of real scores
  # and the matrices of random feature scores/permutation test scores

  return_vals <- list()

  for (trial in 1:n_trials) {
    print(paste("validate_repeat: trial", trial, "/", n_trials))
    val_scores <- validate(X, y, method, options)
    # remove the actual prediction from the validation
    val_scores["cv_y"] <- NULL
    return_vals[[trial]] <- val_scores
  }

  return(return_vals)
}
