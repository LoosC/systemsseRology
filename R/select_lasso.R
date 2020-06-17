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

  # MAYBE INCLUDE POSSIBILIY TO RUN ON ONE SAMPLE?
  # check we have at least two features for internal cross-validation
  n_samples <- length(X[, 1])
  if(n_samples == 1) {
    stop("select_lasso() requires more than one sample for internal cross-validation")
  }

  # default subfolds if its is not set
  # if there are 5 or more samples, default to 5
  # otherwise default to 2
  # while were at it, see if options$subfolds > n_samples, in which case
  # we print a warning and set options$subfolds = n_samples
  if (!("subfolds" %in% names(options))) {
    if (n_samples >= 5) {
      options$subfolds <- 5
    } else {
      options$subfolds <- 2
    }
  } else {
    if (options$subfolds > n_samples) {
      print("Warning: options$subfolds greater than number of samples")
      print("         setting options$subfolds = number of samples")
      options$subfolds <- n_samples
    }
  }

  result_lasso <- glmnet::cv.glmnet(X, y, type.measure = "mse", alpha = options$alpha,
                                    family = fam, nfolds = 5)
  coefs <- coef(result_lasso, s = "lambda.min")

  # it tries it for a sequence of lambdas, choose the one with minimal error
  # that still has features other than intrcept

  # first entry of coefs is the intercept, exclude it from the search
  sel_features <- rownames(coefs)[which(!(coefs[-1,1] == 0)) + 1]
  return(sel_features)
}
