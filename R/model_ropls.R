#' Training the ropls model
#'
#' @param X
#' @param y
#'
#' @return
#' @export
#'
#' @examples
train_ropls <- function(X, y) {
  try_out <- try(
    model <- ropls::opls(X, y,
                         crossValI = 5, # TODO make this an option
                         permI = 0, # no permutation and other output to save computation time
                         info.txtC = "none",
                         fig.pdfC = "none")
  )
  if (is(try_out, "try-error")) {
    # No model could be build, provide a model with one latent variable (even if this is not significant)
    model <- ropls::opls(X, y, predI = 1,
                         crossValI = 5,
                         permI = 0,
                         info.txtC = "none",
                         fig.pdfC = "none")
  }
  return(model)
}

#' Prediction using a ropls object
#'
#' @param model
#' @param X
#'
#' @return
#' @export
#'
#' @examples
predict_ropls <- function(model, X) {

  y_pred <- ropls::predict(model, newdata = X)
  return(y_pred)
}

#' PCA using ropls
#'
#' @param X
#' @param y
#'
#' @return
#' @export
#'
#' @examples
pca_ropls <- function(X) {
  try_out <- try(
    model <- ropls::opls(X, predI = NA,
                         crossValI = 5, # TODO make this an option
                         permI = 0, # no permutation and other output to save computation time
                         info.txtC = "none",
                         fig.pdfC = "none")
  )
  if (is(try_out, "try-error") | ropls::getSummaryDF(model)$pre < 2) {
    # to ensure that the model has at least two prinicipal components
    model <- ropls::opls(X, predI = 2,
                         crossValI = 5,
                         permI = 0,
                         info.txtC = "none",
                         fig.pdfC = "none")
  }

  return(model)
}
