#' Title
#'
#' @param X
#' @param y
#' @param feature_selector
#' @param trainer
#' @param predicter
#' @param options
#'
#' @return
#' @export validate
#'
#' @examples
#'
validate <- function(X, y, feature_selector, trainer, predicter, options) {

  # split into folds
  nfolds <- options$n_folds
  folds <- caret::createFolds(y, nfolds)

    # vector of cross-validation predictions
  y_cv <- vector(mode = "numeric", length = length(y))

  # train model on data outside the folds
  # the predict data inside fold
  for (excl in folds) {
    X_train <- X[-excl, ]
    y_train <- y[-excl]
    X_pred <- X[excl, ]

    features <- feature_selector(X_train, y_train)
    model <- trainer(X_train[, features], y_train)

    y_cv[excl] <- predicter(model, X_pred[, features])

    # do permutation stuff
  }

  return(y_cv)
}
