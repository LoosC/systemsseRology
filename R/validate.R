#' Model validation performing cross-validation and permutation tests.
#'
#' @param X n_samples x n_features matrix
#' @param y vector of labels
#' @param options options list
#' @param method method list with train, predict, score, and optionally select
#'
#' @return cv prediction, cv scores, rf scores, pt scores
#' @export
#'
validate <- function(X, y, method, options) {
  # ----------------- INITIAL PROCESSING ----------------- #

  # see if a feature selector is passed, otherwise default
  # to selecting all features. in the latter case, also make
  # sure rf_trials = 0 since random features don't make sense
  if ("select" %in% names(method) ) {
    select <- method$select
  } else {
    select <- function(X,y) {return(colnames(X))}
    if (!("rf_trials" %in% names(options))) {
      options$rf_trials <- 0
    } else if (options$rf_trials != 0) {
      message("Warning in validate():")
      message("    no feature selector given but rf_trials != 0")
      message("    forcing rf_trials = 0")
      options$rf_trials <- 0
    }
  }

  # default to five-fold cross-validation
  if (!("n_folds" %in% names(options))) {
    options$n_folds <- 5
  }

  # also give these guys some shorter names
  train <- method$train
  predict <- method$predict
  score <- method$score

  # add return values to this list as we go along
  return_values <- list()

  # stores the number of features selected for each fold
  # during cross-validation. we need this later for the
  # random features test
  feats_per_fold <- list()

  # if score is a single function instead of a list of
  # functions, wrap it in a list
  if (class(score) != "list") {
    score <- list(score)
  }
  # ----------------- END INITIAL PROCESSING ----------------- #



  # ----------------- BEGIN CROSS-VALIDATION ----------------- #
  # split data into folds
  folds <- caret::createFolds(y, options$n_folds)
  fold_names <- names(folds)

  # vector of cross-validation predictions
  y_pred <- y

  for (fname in fold_names) {
    indices <- folds[[fname]]
    X_train <- X[-indices, , drop = FALSE]
    y_train <- y[-indices]
    X_pred <- X[indices, , drop = FALSE]

    features <- select(X_train, y_train)

    # actually, check more for valid indices...
    if (length(features) == 0) {
      stop("method$select() did not return any features")
    }

    # store number of features selected in fold for later
    feats_per_fold[[fname]] <- length(features)

    model <- train(as.matrix(X_train[, features, drop = FALSE]), y_train)

    y_pred[indices] <- predict(model, as.matrix(X_pred[, features, drop = FALSE]))
  }

  return_values$cv_y <- y_pred

  # apply the list of functions in score to y_pred, y
  f_star <- function(f) {f(y, y_pred)}
  return_values$cv_score <- lapply(score, f_star)
  # ----------------- END CROSS-VALIDATION ----------------- #



  # ----------------- BEGIN RANDOM FEATURES ----------------- #
  n_trials <- options$rf_trials
  if (n_trials > 0) {
    n_scores <- length(score)
    rf_scores <- list(vector(mode = "numeric", length = n_trials))
    rf_scores <- rep(rf_scores, n_scores)

    for (trial in 1:n_trials) {

      for (fname in fold_names) {
        indices <- folds[[fname]]
        X_train <- X[-indices, , drop = FALSE]
        y_train <- y[-indices]
        X_pred <- X[indices, , drop = FALSE]

        # careful with sample() pathology here...
        # select random features
        total_features <- ncol(X_train)
        features <- sample(1:total_features, feats_per_fold[[fname]])

        model <- train(as.matrix(X_train[, features, drop = FALSE]), y_train)

        y_pred[indices] <- predict(model, as.matrix(X_pred[, features, drop = FALSE]))
      }

      # compute list of scores
      score_list <- lapply(score, f_star)

      # assign them to vectors in the list
      rf_scores <- lv_assign(rf_scores, score_list, trial)
      #rf_scores[trial] <- score(y_pred, y)
    }

    return_values$rf_scores <- rf_scores
  }
  # ----------------- END RANDOM FEATURES ----------------- #



  # ----------------- BEGIN PERMUTATION TESTING ----------------- #
  n_trials <- options$pt_trials

  if (n_trials > 0) {
    n_scores <- length(score)
    pt_scores <- list(vector(mode = "numeric", length = n_trials))
    pt_scores <- rep(pt_scores, n_scores)

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
        X_train <- X[-indices, , drop = FALSE]
        y_train <- y_perm[-indices]
        X_pred <- X[indices, , drop = FALSE]

        features <- select(X_train, y_train)
        model <- train(as.matrix(X_train[, features, drop = FALSE]), y_train)

        y_pred[indices] <- predict(model, as.matrix(X_pred[, features, drop = FALSE]))
      }

      score_list <- lapply(score, f_star)

      pt_scores <- lv_assign(pt_scores, score_list, trial)
    }

    return_values$pt_scores <- pt_scores
  }
  # ----------------- END PERMUTATION TESTING ----------------- #



  # ----------------- BEGIN FINAL PROCESSING ----------------- #
  # unpack the list if its length is 1 (score was just a function)
  if (length(score) == 1) {
    return_values$cv_score <- return_values$cv_score[[1]]

    if ("rf_scores" %in% names(return_values)) {
      return_values$rf_scores <- return_values$rf_scores[[1]]
    }

    if ("pt_scores" %in% names(return_values)) {
      return_values$pt_scores <- return_values$pt_scores[[1]]
    }
  }
  # ----------------- END FINAL PROCESSING ----------------- #

  return(return_values)
}


# for each index in vec_list, it sets
# vec_list[[ind]][v_index] = val_list[[ind]]
lv_assign <- function(vec_list, val_list, v_index) {
  list_indices <- c(1:length(vec_list))
  for (ind in list_indices) {
    vec <- vec_list[[ind]]
    vec[v_index] <- val_list[[ind]]
    vec_list[[ind]] <- vec
  }
  return(vec_list)
}
