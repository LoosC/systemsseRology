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
#'  \item "randomForest_RFE": random forest recursive feature elimination (RFE). In each iteration the feature with lowest improtance is eliminated,
#'  finally the feature set is chosen which has the overall lowest out of bag error. Requires the package `randomForest`
#'  \item "allComb": Comparison of all feature combination of sizes encoded in \{indComb} using the method defined in \{modelMethod}.
#'  }
#' @param nFeatRep only used for method = "lasso" or "lasso_min_mse"
#' @param nFeatFolds Performs \code{nFeatFolds}-fold crossvalidation to determine penalization parameter (for lasso) or recursive feature selection. only used for method = "lasso", "lasso_min_mse", and "caret_rf_rfe"
#' @param thresh only used for method = "lasso"
#' @param chooseS determines whether the regularization parameter is chosen for each repetition of LASSO which
#' provides the minimal MSE ("min"), or the most regularized model within one standard error of the minimal MSE is chosen.
#' Only used for method = "lasso" or "lasso_min_mse"
#' @param alpha Elastic-net mixing parameter (see documentation of glmnet). alpha = 1 corresponds to standard LASSO.
#' Only used for method = "lasso" or "lasso_min_mse"
#' @param indComb Which sizes of sets of features need to be checked
#' Only used for method = "allComb"
#' @param modelMethod which method to use for trying all different combinations of features
#' Only used for method = "allComb"
#'
#' @return Names of the selected features

featureSelection <- function(X, y, method = "lasso", type = "classification",
                             nFeatRep = 100, chooseS = "min", nFeatFolds = 5, thresh = 1,
                             alpha = 1, indComb = 1, modelMethod = "pls",
                             outFlag = FALSE) {
    nClasses <- length(unique(y))
    if (method == "none") {
        selFeatures <- colnames(X)
    } else if (method == "forwSel") {
      oldMetric <- Inf
      continueFlag <- TRUE
      fixFeatures <- c()

      while (continueFlag) {
        testFeatures <- list()
        count <- 1
        for (ind in 1:dim(X)[2]) {
          if (!(colnames(X)[ind] %in% fixFeatures)) {
            testFeatures[[count]] <- c(fixFeatures, colnames(X)[ind])
            count <- count + 1
          }
        }


        # Metric based on which the best set of features is chosen
        metrics <- matrix(NA, ncol = nFeatRep, nrow = length(testFeatures))
        for (iRep in 1:nFeatRep) {
          folds <- createFolds(y, k = nFeatFolds, list = TRUE)
          for (indFeats in 1:length(testFeatures)) {
            yPred <- rep(NA, length = length(y))
            currentFeatures <- testFeatures[[indFeats]]
            for (iFold in 1:length(folds)) {
              XTrain <- X[-folds[[iFold]], which(colnames(X) %in% currentFeatures)]
              yTrain <- y[-folds[[iFold]]]
              XTest <- as.matrix(X[folds[[iFold]], which(colnames(X) %in% currentFeatures)])
              yTest <- y[folds[[iFold]]]
              if (modelMethod == "pls") {
                if (length(currentFeatures) == 1) {
                  trainedModel <- opls(as.matrix(XTrain), yTrain, predI = 1, permI = 0,
                                       fig.pdfC = "none", info.txtC = "none")
                } else {
                  pre_symbol <- try(
                    trainedModel<- opls(as.matrix(XTrain), yTrain, permI = 0,
                                        fig.pdfC = "none", info.txtC = "none"), silent = TRUE)
                  isError <- is(pre_symbol, "try-error")
                  if (isError) { # model failed because of lack of predictive power, force it to use one predictive component
                    trainedModel <- opls(as.matrix(XTrain), yTrain, predI = 1, permI = 0,
                                         fig.pdfC = "none", info.txtC = "none", silent = TRUE)
                  }
                }
              } else if (modelMethod == "randomForest") {
                if (type == "classification") {
                  trainedModel <- randomForest(as.matrix(XTrain), yTrain,
                                               sampsize = rep(min(summary(yTrain)), length(unique(y))))
                } else {
                  trainedModel <- randomForest(as.matrix(XTrain), yTrain)
                }
              }
              if (length(folds[[iFold]]) == 1) {
                yPred[folds[[iFold]]] <- predict(trainedModel, newdata = as.matrix(t(XTest)))
              } else {
                yPred[folds[[iFold]]] <- predict(trainedModel, newdata = as.matrix(XTest))
              }
            }
            if (type == "classification") {
              metrics[indFeats, iRep] <- mean(levels(y)[yPred] != y)
            } else {
              metrics[indFeats, iRep] <- mean(sqrt(mean((y - yPred)^2)))
            }
          }
        }
        if (min(rowSums(metrics)) < oldMetric) {
          indBest <- which(rowSums(metrics) == min(rowSums(metrics)))
          fixFeatures <- testFeatures[[indBest[1]]]
          oldMetric <- min(rowSums(metrics))
        } else {
          continueFlag <- FALSE
          selFeatures <- fixFeatures
        }
      }

    } else if (method == "allComb") {
      testFeatures <- list()
      count <- 1
      for (ind1 in indComb) {
        chooseInd <- combn(dim(X)[2], m = ind1)
        for (ind2 in 1:dim(chooseInd)[2]) {
          testFeatures[[count]] <- colnames(X)[chooseInd[, ind2]]
          count <- count + 1
        }
      }
      # Metric based on which the best set of features is chosen
      metrics <- matrix(NA, ncol = nFeatRep, nrow = length(testFeatures))
      for (iRep in 1:nFeatRep) {
        folds <- createFolds(y, k = nFeatFolds, list = TRUE)
        for (indFeats in 1:length(testFeatures)) {
          yPred <- rep(NA, length = length(y))
          currentFeatures <- testFeatures[[indFeats]]
          for (iFold in 1:length(folds)) {
            XTrain <- X[-folds[[iFold]], which(colnames(X) %in% currentFeatures)]
            yTrain <- y[-folds[[iFold]]]
            XTest <- as.matrix(X[folds[[iFold]], which(colnames(X) %in% currentFeatures)])
            yTest <- y[folds[[iFold]]]
            if (modelMethod == "pls") {
              if (length(currentFeatures) == 1) {
                trainedModel <- opls(as.matrix(XTrain), yTrain, predI = 1, permI = 0,
                                         fig.pdfC = "none", info.txtC = "none")
              } else {
                pre_symbol <- try(
                  trainedModel<- opls(as.matrix(XTrain), yTrain, permI = 0,
                                           fig.pdfC = "none", info.txtC = "none"), silent = TRUE)
                isError <- is(pre_symbol, "try-error")
                if (isError) { # model failed because of lack of predictive power, force it to use one predictive component
                  trainedModel <- opls(as.matrix(XTrain), yTrain, predI = 1, permI = 0,
                                           fig.pdfC = "none", info.txtC = "none", silent = TRUE)
                }
              }
            } else if (modelMethod == "randomForest") {
              if (type == "classification") {
                trainedModel <- randomForest(as.matrix(XTrain), yTrain,
                                             sampsize = rep(min(summary(yTrain)), length(unique(y))))
              } else {
                trainedModel <- randomForest(as.matrix(XTrain), yTrain)
              }
            }
            if (length(folds[[iFold]]) == 1) {
              yPred[folds[[iFold]]] <- predict(trainedModel, newdata = as.matrix(t(XTest)))
            } else {
              yPred[folds[[iFold]]] <- predict(trainedModel, newdata = as.matrix(XTest))
            }
          }
          if (type == "classification") {
            metrics[indFeats, iRep] <- mean(levels(y)[yPred] != y)
          } else {
            metrics[indFeats, iRep] <- mean(sqrt(mean((y - yPred)^2)))
          }
        }
      }
      indBest <- which(rowSums(metrics) == min(rowSums(metrics)))
      selFeatures <- testFeatures[[indBest[1]]]

    } else if (method == "PLS_rfe") {
        folds <- createFolds(y, k = nFeatFolds, list = TRUE)

        X_red <- X # matrix with reduced features
        featSel <- data.frame(matrix(1, ncol = dim(X)[2], nrow = dim(X)[2] + 1))
        rownames(featSel) <- c(colnames(X), "metric") # add column for the accuracy/mse
        colnames(featSel) <- 1:dim(X)[2]
        for (ind in 1:(dim(X)[2]-1)) {
          # Perform PLS
          VIPs <- matrix(NA, ncol = dim(X_red)[2], nrow = nFeatFolds)
          colnames(VIPs) <- colnames(X_red)
          yPred <- rep(NA, length = length(y))
          for (iFold in 1:length(folds)) {
            # Define training and test set
            if (nFeatFolds > 1) {
              XTrain <- X_red[-folds[[iFold]], ]
              yTrain <- y[-folds[[iFold]]]
              XTest <- as.matrix(X_red[folds[[iFold]], ])
              yTest <- y[folds[[iFold]]]
            } else {
              # No cross-valildation, whole data is used to train and evaluate model
              XTrain <- X_red
              yTrain <- y
              XTest <- X_red
              yTest <- y
            }
            pre_symbol <- try(trainedModel <- opls(as.matrix(XTrain), yTrain,
                                                   permI = 0, crossValI = 5, info.txtC = "none",
                                                   fig.pdfC = "none"))
            isError <- is(pre_symbol, "try-error")
            if (isError) {
              # No model could be build, provide a model with one latent variable (even if this is not significant)
              trainedModel <- opls(as.matrix(XTrain), yTrain, predI = 1,
                                   permI = 0, crossValI = 5, info.txtC = "none", fig.pdfC = "none")
            }
            if (nFeatFolds > 1) {
              if (length(folds[[iFold]]) == 1) {
                yPred[folds[[iFold]]] <- predict(trainedModel, newdata = as.matrix(t(XTest)))
              } else {
                yPred[folds[[iFold]]] <- predict(trainedModel, newdata = as.matrix(XTest))
              }
            } else {
              yPred <- predict(trainedModel, newdata = as.matrix(XTest))
            }
            VIPs[iFold, ] <- getVipVn(trainedModel)
          }

          if (type == "classification") {
            featSel[dim(X)[2] + 1, ind] <- mean(yPred != y)
          } else {
            featSel[dim(X)[2] + 1, ind] <- mean(sqrt(mean((y - yPred)^2)))
          }
          # Remove the feature which has the lowest importance
          if (ind < (dim(X)[2]-1)) {
            remFeature <- colnames(VIPs)[which(colSums(VIPs) == min(colSums(VIPs)))[1]]
            X_red <- X_red[, -which(colnames(X_red) == remFeature)]
            featSel[remFeature, ind:dim(X)[2]] <- 0
          }
        }
      metric_min <- which(featSel["metric", ] == min(featSel["metric", ]))
      selFeatures <- colnames(X)[which(featSel[1:(dim(featSel)[1] - 1),
                                               metric_min[length(metric_min)]] == 1)]
    } else if (method == "caret_rf_rfe") {
      control <- rfeControl(functions = rfFuncs, method = "cv", rerank = TRUE,
                            number = nFeatFolds)
      # run the RFE algorithm
      results <- rfe(X, y, rfeControl = control,
                     sizes = unique(c(2^c(1:floor(log2(dim(X)[2]))), dim(X)[2])))
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
                                  family = family, nfolds = nFeatFolds)
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
              stop("no feature chosen")
            }
        } else if (method == "lasso_min_mse") {
            indSel2 <- which(tmpFeat[, which(mses == min(mses)) + 1] == 1)
            if (length(indSel2) < 3) {
                selFeatures <- tmpFeat$features[indSel]
                selFeatures <- selFeatures[2:length(selFeatures)]  # remove intercept
                if (is.na(selFeatures)) {
                  stop("no feature chosen")
                }
            } else {
                selFeatures <- tmpFeat$features[indSel2]
                selFeatures <- selFeatures[2:length(selFeatures)]  # remove intercep
            }
        }

    } else {
        stop("method not defined")
    }
    if (outFlag) {
      return(output = list(selFeatures = selFeatures,
                           metrics = metrics,
                           testFeatures = testFeatures))
    } else {
      return(selFeatures)
    }


}
