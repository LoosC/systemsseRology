#' Feature selection using LASSO
#'
#' @param X n_samples x n_features matrix
#' @param y vector of labels
#' @param options with 'alpha' the elastic-net mixing parameter (see glmnet)
#'
#' @return names of selected features
#' @export
select_lasso <- function(X, y, options = list()) {
  # decide on type of GLM depending on type of y
  # 2-level factor -> binomial, n-level factor -> multinomial
  # vector -> gaussian, anything else gives an error
  if (is.factor(y)) {
    if (nlevels(y) == 1) {
      stop("y is a factor with only one level")
    } else if (nlevels(y) == 2) {
      fam <- "binomial"
    } else {
      fam <- "multinomial"
    }
  } else if (is.numeric(y)) {
    fam <- "gaussian"
  } else {
    stop("y must be of type factor or numeric vector")
  }

  # default alpha to 1 if it is not set
  if (!("alpha" %in% names(options))) {
    options$alpha <- 1
  }

  # cv.glmnet needs at least 3 folds, so we need at least three features
  n_samples <- nrow(X)
  if (n_samples < 3) {
    stop("select_lasso() requires more than three samples for internal cross-validation")
  }

  # default subfolds if its is not set
  # if there are 5 or more samples, default to 5
  # otherwise default to 3
  # while were at it, ensure that subfolds is in [3, n_samples]
  # and print a warning if it wasn't
  if (!("subfolds" %in% names(options))) {
    if (n_samples >= 5) {
      options$subfolds <- 5
    } else {
      options$subfolds <- 3
    }
  } else {
    if (options$subfolds > n_samples) {
      message("Warning in select_lasso():")
      message("    options$subfolds greater than number of samples")
      message("    setting options$subfolds = number of samples")
      options$subfolds <- n_samples
    }
    if (options$subfolds < 3) {
      message("Warning in select_lasso():")
      message("    options$subfolds was less than 3")
      message("    setting options$subfolds = 3")
      options$subfolds <- 3
    }
  }

  # fit an appropriate lasso model with a number of trials corresponding to
  # different values of lambda
  lasso <- glmnet::cv.glmnet(X, y, type.measure = "mse", alpha = options$alpha,
                             family = fam, type.multinomial = "grouped",
                             nfolds = options$subfolds)

  # lasso$lambda[k] is the value of lambda in the k-th trial
  # lasso$nzero[k] is the number of non-zero coefficients in the fitted model
  # (= number of features not including the intercept) in the k-th trial
  # things are arranged such that the first entry of nzero is 0
  # and the final entry of nzero equals the number of total features
  # lasso$cvm is the cross-validated score of the k-th trial

  # find the model with the smallest error that has at least one non-zero
  # coefficient other than the intercept
  indices <- which(lasso$nzero > 0)
  lambdas <- lasso$lambda[indices]
  scores <- lasso$cvm[indices]
  # if there is more than one index attaining the minimum which.min picks
  # the smallest one - this corresponds to choosing best score with the
  # minimal number of features selected
  best <- which.min(scores)
  lasso_coeffs <- coef(lasso, s = lambdas[best])

  if (fam == "multinomial") {
    # if the data has multiple responses, the coefficients are a matrix
    # that is returned as a list of columns. type.multinomial = "grouped"
    # forced features to be selected for all responses or for none, so we
    # can get the selected features also by only considering the first
    # column. we just replace lasso_coeffs by this column and proceed as usual
    lasso_coeffs <- lasso_coeffs[[1]]
  }

  # remove the intercept and the entries that are zero
  lasso_coeffs <- lasso_coeffs[-1,]
  lasso_coeffs <- lasso_coeffs[which(lasso_coeffs != 0)]

  # return the names of the selected features. previous code turned
  # coefficients into a vector, so use names() rather than rownames()
  return(names(lasso_coeffs))
}
