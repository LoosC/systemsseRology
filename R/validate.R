#' Title
#'
#' @param X
#' @param y
#' @param options
#' @param method
#'
#' @return
#' @export validate
#'
#' @examples
#'
validate <- function(X, y, method, options) {

  # ----------------- INITIAL PROCESSING ----------------- #
  # give these guys some shorter names
  select = method$select
  train = method$train
  predict = method$predict
  score = method$score

  # add return values to this list as we go along
  return_values <- list()

  # stores the number of features selected for each fold
  # during cross-validation. we need this later for the
  # random features test
  n_features <- list()

  # ----------------- END INITIAL PROCESSING ----------------- #



  # ----------------- BEGIN CROSS-VALIDATION ----------------- #

  # split data into folds
  fold_indices <- caret::createFolds(y, options$n_folds)
  fold_names <- names(fold_indices)

  # vector of cross-validation predictions
  y_cv <- vector(mode = "numeric", length = length(y))

  for(fname in fold_names) {
    excl <- fold_indices[[fname]]
    X_train <- X[-excl, ]
    y_train <- y[-excl]
    X_pred <- X[excl, ]

    features <- select(X_train, y_train)

    # store number of features in fold for later
    n_features[[fname]] <- length(features)

    model <- train(X_train[, features], y_train)

    y_cv[excl] <- predict(model, X_pred[, features])
  }

  return_values$y_cv = y_cv
  return_values$score_cv = score(y_cv, y)

  # ----------------- END CROSS-VALIDATION ----------------- #



  # ----------------- BEGIN RANDOM FEATURES ----------------- #
  # n_trials = options$n_random_trials
  # if(n_trials > 0) {
  #   random_feature_score <- vector(mode = "numeric", length = n_trials)
  #   while(n_trials > 0) {
  #
  #     for(fname in fold_names) {
  #       excl <- fold_indices[[fname]]
  #       X_train <- X[-excl, ]
  #       y_train <- y[-excl]
  #       X_pred <- X[excl, ]
  #
  #       features <- feature_selector(X_train, y_train)
  #       n_features[[fname]] <- length(features)
  #       model <- trainer(X_train[, features], y_train)
  #
  #       y_cv[excl] <- predicter(model, X_pred[, features])
  #     }
  #
  #     n_trials <- n_trials - 1
  #   }
  # }
  # ----------------- END RANDOM FEATURES ----------------- #


  # ----------------- BEGIN PERMUTATION TESTING ----------------- #

  # # do permutation stuff
  # # random features
  # if random features
  # take same nr of features as returned by select()
  # generate Nrandomfearutes samples of N random features
  # train model on those features and store in a new y_pred
  # compute the score here or store for later?
  #
  # if permutation
  # generate N random permutations of y
  # # permute labels



  # ----------------- END PERMUTATION TESTING ----------------- #

  return(return_values)
}
