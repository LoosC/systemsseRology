#' Title
#'
#' @param X
#' @param y
#' @param options
#'
#' @return
#' @export select_lasso
#'
#' @examples
select_lasso <- function(X, y, options = list()) {
  # check zscore?

  # decide on type of GLM depending on type of y
  # 2-level factor -> binomial, n-level factor -> multinomial
  # vector -> gaussian, anything else gives an error
  if (class(y) == "factor") {
    if (nlevels(y) == 1) {
      stop("y is a factor with only one level")
    } else if (nlevels(y) == 2) {
      fam <- "binomial"
    } else {
      fam <- "multinomial"
    }
  } else if (class(y) == "numeric") {
    fam <- "gaussian"
  } else {
    stop("y must be of type factor or numeric vector")
  }

  # default alpha to 1 if it is not set
  if (!("alpha" %in% names(options))) {
    options$alpha <- 1
  }

  # cv.glmnet needs at least 3 folds, so we need at least three features
  n_samples <- length(X[, 1])
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
      print("Warning: options$subfolds greater than number of samples")
      print("         setting options$subfolds = number of samples")
      options$subfolds <- n_samples
    }
    if (options$subfolds < 3) {
      print("Warning: options$subfolds was less than 3")
      print("         setting options$subfolds = 3")
      options$subfolds <- 3
    }
  }

  # fit an appropriate lasso model with a number of trials corresponding to
  # different values of lambda
  lasso <- glmnet::cv.glmnet(X, y, type.measure = "mse", alpha = options$alpha,
                                    family = fam, nfolds = options$subfolds)

  # lasso$lambda[k] is the value of lambda in the k-th trial
  # lasso$nzero[k] is the number of non-zero coefficients in the fitted model
  # (= number of features not including the intercept) in the k-th trial
  # things are arranged such that the first entry of nzero is 0
  # and the final entry of nzero equals the number of total features
  # lasso$cvm is the cross-validated MSE score of the k-th trial

  # find the model with the smallest error that has at least one nonz-ero
  # coefficient other than the intercept
  indices <- which(lasso$nzero > 0)
  lambdas <- lasso$lambda[indices]
  mses <- lasso$cvm[indices]
  # if there is more than one index attaining the minimum which.min picks
  # the smallest one - this corresponds to choosing best mse scofe with the
  # minimal number of features selected
  best <- which.min(mses)
  coefficients <- coef(lasso, s = lambdas[best])

  # remove the intercept and the entries that are zero
  coefficients <- coefficients[-1,]
  coefficients <- coefficients[which(coefficients != 0)]

  # return the names of the selected features. previous code turned
  # coefficients into a vector, so use names() rather than rownames()
  return(names(coefficients))
}
