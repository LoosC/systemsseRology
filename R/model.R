#' Title
#'
#' @param X
#' @param y
#'
#' @return
#' @export
#'
#' @examples
model_train <- function(X, y) {

  model <- ropls::opls(X, y, predI = 2, permI = 0, crossValI = 5,
                       info.txtC = "none", fig.pdfC = "none")
  return(model)
}

#' Title
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
model_predict <- function(model, X) {

  y_pred <- ropls::predict(model, newdata = X)
  return(y_pred)
}

#' Title
#'
#' @param X
#' @param y
#'
#' @return
#' @export
#'
#' @examples
select_features <- function(X, y) {

  result_lasso <- glmnet::cv.glmnet(X, y, type.measure = "mse", alpha = 1,
                                    family = "binomial", nfolds = 5)
  coefs <- glmnet::coef(result_lasso, s = "lambda.min")
  # first entry of coefs is the intercept, exclude it from the search
  sel_features <- rownames(coefs)[which(!(coefs[-1,1] == 0)) + 1]
  return(sel_features)
}
