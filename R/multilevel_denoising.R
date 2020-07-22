#' Multilevel Denoising
#'
#' @param X raw data matrix
#' @param X_labels vector/factor indicating
#'
#' @return
#' @export
#'
#' @examples
multilevel_denoising <- function(X, X_labels) {
  # check if X is already z-scored and warn if this is the case
  nz_col_means <- abs(colMeans(X)) > 1e-10
  nu_col_vars <- abs(matrixStats::colVars(X) - 1) > 1e-10
  if (sum(nz_col_means) + sum(nu_col_vars) == 0) {
    message("Warning in multilevel_denoising():")
    message("    X was z-scored before multilevel_denoising()")
  }

  # throw an error if X_labels is neither numeric nor factor
  if (!(is.numeric(X_labels) || is.factor(X_labels))) {
    stop("X_labels must be either a factor or a numeric vector")
  }

  # if X_labels is a numeric vector, cast it into a factor
  if (is.numeric(X_labels)){
    X_labels <- as.factor(X_labels)
  }

  # for each cluster of samples with the same label, subtract
  # the mean over the cluster from the individual samples
  for (lab in levels(X_labels)) {
    cluster_inds <- X_labels == lab
    cluster_means <- colMeans(X[cluster_inds, ])
    X[cluster_inds, ] <- sweep(X[cluster_inds, ], MARGIN = 2,
                               STATS = cluster_means, FUN = "-")
  }

  # finally z-score the entire data matrix X
  X <- scale(X, center = TRUE, scale = TRUE)

  return(X)
}
