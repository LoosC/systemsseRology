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
  # TODO all kinds of debugging, check for data types
  # data type fuckups, bad indexing, weird cases, emtpy sets etc.
  # naming... comments/cleanup
  # check if factors/vectors etc

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

  for (fname in fold_names) {
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
  n_trials <- options$rf_trials
  if (n_trials > 0) {
    rf_scores <- vector(mode = "numeric", length = n_trials)

    for (trial in 1:n_trials) {

      for (fname in fold_names) {
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
  n_trials <- options$pt_trials
  if (n_trials > 0) {
    pt_scores <- vector(mode = "numeric", length = n_trials)

    for (trial in 1:n_trials) {
      # create permuted y, but only permute inside each fold
      y_perm <- factor(levels = levels(y))
      for (fname in fold_names) {
        excl <- fold_indices[[fname]]
        perm <- sample(1:length(excl))
        y_perm[excl] <- y[excl[perm]]
      }

      for (fname in fold_names) {
        excl <- fold_indices[[fname]]
        X_train <- X[-excl, ]
        y_train <- y_perm[-excl]
        X_pred <- X[excl, ]

        features <- select(X_train, y_train)
        model <- train(X_train[, features], y_train)

        y_pred[excl] <- predict(model, X_pred[, features])
      }

      pt_scores[trial] <- score(y_pred, y_perm)
    }

    return_values$pt_scores <- pt_scores
  }
  # ----------------- END PERMUTATION TESTING ----------------- #

  return(return_values)
}
