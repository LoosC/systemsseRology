#' Title
#'
#' @param X
#' @param y
#' @param options
#'
#' @return
#' @export
#'
#' @examples
model_train <- function(X, y, options) {

  model <- ropls::opls(X, y, permI = 0, crossValI = 5,
                       info.txtC = "none", fig.pdfC = "none")
  return(model)
}

#' Title
#'
#' @param model
#' @param options
#'
#' @return
#' @export
#'
#' @examples
model_predict <- function(model, X, options) {

  y_pred <- ropls::predict(model, newdata = X)
  return(y_pred)
}

#' Title
#'
#' @param X
#' @param y
#' @param options
#'
#' @return
#' @export
#'
#' @examples
select_features <- function(X, y, options = FALSE) {

  result_lasso <- glmnet::cv.glmnet(X, y, type.measure = "mse", alpha = 1,
                                    family = "binomial", nfolds = 5)
  coefs <- coef(result_lasso, s = "lambda.min")
  # first entry of coefs is the intercept, exclude it from the search
  sel_features <- rownames(coefs)[which(!(coefs[-1,1] == 0)) + 1]
  return(sel_features)
}
