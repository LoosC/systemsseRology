#' Multilevel Denoising
#'
#' @param X raw data matrix
#' @param X_labels vector/factor indicating subject id
#'
#' @return denoised and z-scored data matrix
#' @export
#'
#' @examples
multilevel_denoising <- function(X, X_labels) {
  # check if X is already z-scored and warn if this is the case
  nz_col_means <- abs(colMeans(X)) > 1e-10
  nu_col_vars <- abs(matrixStats::colVars(X) - 1) > 1e-10
  if (sum(nz_col_means) + sum(nu_col_vars) == 0) {
    message("Warning in multilevel_denoising():")
    message("    X was already z-scored")
  }

  # throw an error if X_labels is neither numeric nor factor
  if (!(is.numeric(X_labels) || is.factor(X_labels))) {
    stop("X_labels must be either a factor or a numeric vector")
  }

  # if X_labels is a numeric vector, cast it into a factor
  if (is.numeric(X_labels)) {
    X_labels <- as.factor(X_labels)
  }

  # for each subject, i.e., group of samples with the same label, subtract
  # the mean over the samples belonging to the subject
  for (lab in levels(X_labels)) {
    subject_inds <- X_labels == lab
    subject_means <- colMeans(X[subject_inds, ])
    X[subject_inds, ] <- sweep(X[subject_inds, ], MARGIN = 2,
                               STATS = subject_means, FUN = "-")
  }

  # finally z-score the entire data matrix X
  X <- scale(X, center = TRUE, scale = TRUE)

  return(X)
}
