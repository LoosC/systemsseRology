#' Performs feature selection using different methods.
#'
#' @export
#' @param X data matrix, the column names are the featues
#' @param y label/values
#' @param method for feature selection:
#' \itemize{
#'  \item "none": all features are used
#'  \item "lasso": performs \code{nFeatRep} repetitions of penalized regression (linear for type = "regression", logistic for type = "classification")
#'  are performed and the features which are chosen at least \code{thresh} times are selected. Requires the package `glmnet`
#'  \item "lasso_min_mse": performs \code{nFeatRep} repetitions of penalized regression (linear for type = "regression", logistic for type = "classification")
#'  and choses the feature set with the overall lowest mean squared error (MSE).
#'  \item"randomForest_RFE": random forest recursive feature elimination (RFE). In each iteration the feature with lowest improtance is eliminated,
#'  finally the feature set is chosen which has the overall lowest out of bag error. Requires the package `randomForest`
#'  }
#' @param nFeatRep only used for method = "lasso" or "lasso_min_mse"
#' @param nLassoFolds Performs \code{nLassoFolds}-fold crossvalidation to determine penalization parameter. only used for method = "lasso"
#' @param thresh only used for method = "lasso"
#' @param chooseS determines whether the regularization parameter is chosen for each repetition of LASSO which
#' provides the minimal MSE ("min"), or the most regularized model within one standard error of the minimal MSE is chosen.
#' Only used for method = "lasso" or "lasso_min_mse"
#' @param alpha Elastic-net mixing parameter (see documentatino of glmnet). alpha = 1 corresponds to standard LASSO.
#' Only used for method = "lasso" or "lasso_min_mse"
#' @return Names of the selected features

featureSelection <- function(X, y, method = "lasso", type = "classification",
                             nFeatRep = 100, chooseS = "min", nLassoFolds = 5, thresh = 1,
                             alpha = 1) {
    nClasses <- length(unique(y))
    if (method == "none") {
        selFeatures <- colnames(X)
    } else if (method == "caret_rf_rfe") {
      control <- rfeControl(functions = rfFuncs, method = "cv", rerank = TRUE, number = 5)
      # run the RFE algorithm
      results <- rfe(X, y, rfeControl = control)
      selFeatures <- results$optVariables
    } else if (method == "randomForest_importance_top") {
      rf <- randomForest(as.matrix(X), as.factor(y))
      selFeatures <- rownames(rf$importance)[rev(order(rf$importance))[1:thresh]]
    } else if (method == "randomForest_importance") {
      rf <- randomForest(as.matrix(X), as.factor(y))
      selFeatures <- rownames(rf$importance)[which(rf$importance > thresh*sum(rf$importance))]
    } else if (method == "randomForest_RFE") {
        X_red <- X # matrix with reduced featuers
        featSel <- data.frame(matrix(1, ncol = dim(X)[2], nrow = dim(X)[2] + 1))
        rownames(featSel) <- c(colnames(X), "oob") # add column for the out of bag error ("oob")
        colnames(featSel) <- 1:dim(X)[2]
        for (ind in 1:(dim(X)[2])) {
            # Perform random forest training (with default parameters) and calculate the
            # out of bag error for this iteration
            if (type == "classification") {
                rf <- randomForest(as.matrix(X_red), as.factor(y))
                featSel[dim(X)[2] + 1, ind] <- mean(predict(rf) != y)
            } else {
                rf <- randomForest(as.matrix(X_red), as.vector(y))
                featSel[dim(X)[2] + 1, ind] <- mean(sqrt(mean((y - predict(rf))^2)))
            }
            # Remove the feature which has the lowest importance
            if (ind < dim(X)[2]) {
                remFeature <- rownames(rf$importance)[which(rf$importance == min(rf$importance))[1]]
                X_red <- X_red[, -which(colnames(X_red) == remFeature)]
                featSel[remFeature, ind:dim(X)[2]] <- 0
            }
        }
        oob_min <- which(featSel["oob", ] == min(featSel["oob", ]))
        selFeatures <- colnames(X)[which(featSel[1:(dim(featSel)[1] - 1), oob_min[length(oob_min)]] == 1)]
    } else if (method == "lasso" || method == "lasso_min_mse") {
        tmpFeat <- data.frame(features = c("(Intercept)", colnames(X)))
        lastZero <- data.frame(features = c("(Intercept)", colnames(X)))
        mses <- rep(NA, nFeatRep)
        if (type == "classification") {
            if (nClasses > 2) {
                family = "multinomial"
            }else {
                family = "binomial"
            }
        } else {
            family <- "gaussian"
        }
        for (iRep in 1:nFeatRep) {
            resLasso <- cv.glmnet(X, y, type.measure = "mse", alpha = alpha,
                                  family = family, nfolds = nLassoFolds)
            mses[iRep] <- resLasso$cvm[which(resLasso$lambda == resLasso$lambda.min)]

            if (type == "classification" & nClasses > 2) {
                if (chooseS == "min") {
                    coefs <- coef(resLasso, s = "lambda.min")
                } else {
                    coefs <- coef(resLasso, s = "lambda.1se")  # check again
                }
                tmpInds <- 1
                for (indY in 1:length(coefs)) {
                    tmpCoefs <- coefs[[indY]]
                    for (ind in 2:length(tmpCoefs)) {
                        if (tmpCoefs[ind] != 0) {
                            tmpInds <- c(tmpInds, ind)
                        }
                    }
                }
                selec <- rep(0, dim(X)[2] + 1)
                selec[unique(tmpInds)] <- 1
                tmpFeat <- cbind(tmpFeat, selec)
                indLastZero <- NA
            } else {
                if (chooseS == "min") {
                    coefs <- coef(resLasso, s = "lambda.min")
                } else {
                    coefs <- coef(resLasso, s = "lambda.1se")  # check again
                }
                tmpInds <- 1
                for (ind in 2:length(coefs)) {
                    if (coefs[ind] != 0) {
                      tmpInds <- c(tmpInds, ind)
                    }
                }
                selec <- rep(0, dim(X)[2] + 1)
                selec[tmpInds] <- 1
                tmpFeat <- cbind(tmpFeat, selec)

                # last coefficient to set to 0
                indLastZero <- which(resLasso$glmnet.fit$df > 1)[1]
                if (!is.na(indLastZero)) {
                    coefs <- coef(resLasso, s = resLasso$lambda[indLastZero])
                    tmpInds <- 1
                    for (ind in 2:length(coefs)) {
                      if (coefs[ind] != 0) {
                        tmpInds <- c(tmpInds, ind)
                      }
                    }
                    selec <- rep(0, dim(X)[2] + 1)
                    selec[tmpInds] <- 1
                    lastZero <- cbind(lastZero, selec)
                }
            }
        }
        if (nFeatRep == 1) {
            indSel <- which(tmpFeat[, -1] >= thresh)
        } else {
            indSel <- which(rowSums(tmpFeat[, -1]) >= thresh)
        }
        thresh2 <- thresh - 1
        while (length(indSel) < 3 && thresh2 > 0) {
            if (nFeatRep == 1) {
                indSel <- which(tmpFeat[, -1] >= thresh2)
            } else {
                indSel <- which(rowSums(tmpFeat[, -1]) >= thresh2)
            }
            thresh2 <- thresh2 - 1
        }
        if (length(indSel) < 3 & !is.na(indLastZero)) {
            if (nFeatRep == 1) {
                indSel <- which(lastZero[, -1] >= thresh)
            } else {
                indSel <- which(rowSums(lastZero[, -1]) >= thresh)
            }
            indSel <- which(rowSums(lastZero[, -1]) >= thresh)
            thresh2 <- thresh - 1
            while (length(indSel) < 3 && thresh2 > 0) {
                if (nFeatRep == 1) {
                    indSel <- which(lastZero[, -1] >= thresh2)
                } else {
                    indSel <- which(rowSums(lastZero[, -1]) >= thresh2)
                }
                thresh2 <- thresh2 - 1
            }
        }
        if (method == "lasso") {
            selFeatures <- tmpFeat$features[indSel]
            selFeatures <- selFeatures[2:length(selFeatures)]  # remove intercept
            if (is.na(selFeatures)) {
              error("no feature chosen")
            }
        } else if (method == "lasso_min_mse") {
            indSel2 <- which(tmpFeat[, which(mses == min(mses)) + 1] == 1)
            if (length(indSel2) < 3) {
                selFeatures <- tmpFeat$features[indSel]
                selFeatures <- selFeatures[2:length(selFeatures)]  # remove intercept
                if (is.na(selFeatures)) {
                  error("no feature chosen")
                }
            } else {
                selFeatures <- tmpFeat$features[indSel2]
                selFeatures <- selFeatures[2:length(selFeatures)]  # remove intercep
            }
        }

    } else {
        error("method not defined")
    }

    return(selFeatures)
}
