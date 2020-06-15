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
  # bad indexing, weird cases, emtpy sets
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
  feats_per_fold <- list()
  # ----------------- END INITIAL PROCESSING ----------------- #



  # ----------------- BEGIN CROSS-VALIDATION ----------------- #
  # split data into folds
  folds <- caret::createFolds(y, options$n_folds)
  fold_names <- names(folds)

  # vector of cross-validation predictions
  y_pred <- y

  for (fname in fold_names) {
    indices <- folds[[fname]]
    X_train <- X[-indices, ]
    y_train <- y[-indices]
    X_pred <- X[indices, ]

    features <- select(X_train, y_train)
    # store number of features selected in fold for later
    feats_per_fold[[fname]] <- length(features)

    model <- train(X_train[, features], y_train)

    y_pred[indices] <- predict(model, X_pred[, features])
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
        indices <- folds[[fname]]
        X_train <- X[-indices, ]
        y_train <- y[-indices]
        X_pred <- X[indices, ]

        # careful with sample() pathology here...
        # select random features
        total_features <- length(X_train[1, ])
        features <- sample(1:total_features, feats_per_fold[[fname]])

        model <- train(X_train[, features], y_train)

        y_pred[indices] <- predict(model, X_pred[, features])
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
    y_perm <- y

    for (trial in 1:n_trials) {
      # create permuted y, but only permute inside each fold
      for (fname in fold_names) {
        indices <- folds[[fname]]
        perm <- sample(1:length(indices))
        y_perm[indices] <- y[indices[perm]]
      }

      for (fname in fold_names) {
        indices <- folds[[fname]]
        X_train <- X[-indices, ]
        y_train <- y_perm[-indices]
        X_pred <- X[indices, ]

        features <- select(X_train, y_train)
        model <- train(X_train[, features], y_train)

        y_pred[indices] <- predict(model, X_pred[, features])
      }

      pt_scores[trial] <- score(y_pred, y_perm)
    }

    return_values$pt_scores <- pt_scores
  }
  # ----------------- END PERMUTATION TESTING ----------------- #

  return(return_values)
}
