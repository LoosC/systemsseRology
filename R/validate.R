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
  select <- method$select
  train <- method$train
  predict <- method$predict
  score <- method$score

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
  y_pred <- vector(mode = "numeric", length = length(y))

  for(fname in fold_names) {
    excl <- fold_indices[[fname]]
    X_train <- X[-excl, ]
    y_train <- y[-excl]
    X_pred <- X[excl, ]

    features <- select(X_train, y_train)

    # store number of features in fold for later
    n_features[[fname]] <- length(features)

    model <- train(X_train[, features], y_train)

    y_pred[excl] <- predict(model, X_pred[, features])
  }

  return_values$cv_y <- y_pred
  return_values$cv_score <- score(y_pred, y)

  # ----------------- END CROSS-VALIDATION ----------------- #



  # ----------------- BEGIN RANDOM FEATURES ----------------- #
  n_trials <- options$n_random_trials
  if(n_trials > 0) {
    rf_scores <- vector(mode = "numeric", length = n_trials)

    for(trial in 1:n_trials) {

      for(fname in fold_names) {
        excl <- fold_indices[[fname]]
        X_train <- X[-excl, ]
        y_train <- y[-excl]
        X_pred <- X[excl, ]

        # careful with sample() pathology here...
        # select random features
        total_features <- length(X_train[1, ])
        features <- sample(1:total_features, n_features[[fname]])

        model <- train(X_train[, features], y_train)

        y_pred[excl] <- predict(model, X_pred[, features])
      }

      rf_scores[trial] <- score(y_pred, y)
    }

    return_values$rf_scores <- rf_scores
  }
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
