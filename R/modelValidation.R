#' Model validation using permutation testing
#'
#' @param X data matrix
#' @param y label/values
#' @param nFolds for n-fold crossvalidatoin
#' @param nReps number of repetitions
#' @param nPerms number of permutations per repetition
#' @param type "classification" or "regression"
#' @param method for classification/regression, options are "pls", "randomForest", "logisticRegression",
#' @param featureMethod method for feature selection, @seealso \link{featureSelection.R}
#' @param nFeatRep @seealso \link{featureSelection.R}
#' @param nLassoFolds @seealso \link{featureSelection.R}
#' @param thresh @seealso \link{featureSelection.R}
#' @param alpha @seealsoe \link{featureSelection.R}
#' @param chooseS @seealso \link{featureSelection.R}
#' @param saveFlag whether to save the output
#' @param fileStr string where RData should be saved
#' @param yPredOut flag whether predicted output should be returned
#' @return Metrics of performance (accuracy or correlation/rmses) for models based on
#' real data or random features/permuted labels
#' @export

modelValidation <- function(X, y, nFolds = 5, nReps = 10, nPerms = 100, type = "classification",
                            method = "pls", featureMethod = "lasso",
                            nFeatRep = 100, nLassoFolds = 5, thresh = 1, alpha = 1, chooseS = "min",
                            saveFlag = FALSE, fileStr = "accuraciesFullModel_withPerm",
                            yPredOut = FALSE) {

    yPred <- matrix(NA, nrow = length(y), ncol = 1)
    acc <- matrix(NA, nrow = nReps, ncol = 1)
    corr <- matrix(NA, nrow = nReps, ncol = 1)
    rmses <- matrix(NA, nrow = nReps, ncol = 1)
    nClasses <- length(unique(y))
    if (nPerms > 0) {
        yPred_randFeatures <- matrix(NA, nrow = length(y), ncol = nPerms)
        yPred_permutedLabels <- matrix(NA, nrow = length(y), ncol = nPerms)
        if (type == "classification") {
            acc_randFeatures <- matrix(NA, nrow = nReps, ncol = nPerms)
            acc_permutedLabels <- matrix(NA, nrow = nReps, ncol = nPerms)
        } else {
            corr_randFeatures <- matrix(NA, nrow = nReps, ncol = nPerms)
            rmses_randFeatures <- matrix(NA, nrow = nReps, ncol = nPerms)
            corr_permutedLabels <- matrix(NA, nrow = nReps, ncol = nPerms)
            rmses_permutedLabels <- matrix(NA, nrow = nReps, ncol = nPerms)
        }
    }

    for (iRep in 1:nReps) {
        if (nFolds > 1) {
            folds <- createFolds(y, k = nFolds, list = TRUE)
        }
        for (iFold in 1:length(folds)) {
            if (nFolds > 1) {
                XTrain <- X[-folds[[iFold]], ]
                yTrain <- y[-folds[[iFold]]]
                XTest <- as.matrix(X[folds[[iFold]], ])
                yTest <- y[folds[[iFold]]]
            } else {
                XTrain <- X
                yTrain <- y
                XTest <- X
                yTest <- y
            }
            print(paste("Repetition:", iRep,  "Fold:", iFold, "/", nFolds, sep = " "))
            selFeatures <- featureSelection(XTrain, yTrain,
                                            method = featureMethod, type = type,
                                            chooseS = chooseS, nFeatRep = nFeatRep,
                                            nLassoFolds = nLassoFolds,
                                            thresh = thresh, alpha = alpha)
            print(selFeatures)
            XTrainSel <- XTrain[, which(colnames(X) %in% selFeatures)]

            if (length(folds[[iFold]]) == 1) {
                XTestSel <- as.matrix(XTest[which(colnames(X) %in% selFeatures)])
            } else {
                XTestSel <- as.matrix(XTest[, which(colnames(X) %in% selFeatures)])
            }

            if (type == "classification") {
                yTrain <- as.factor(yTrain)
            } else {
                yTrain <- as.vector(yTrain)
            }

            if (method == "randomForest") {
                if (type == "classification") {
                    trainedModel <- randomForest(as.matrix(XTrainSel), yTrain, sampsize = rep(min(summary(yTrain)), nClasses))
                } else {
                    trainedModel <- randomForest(as.matrix(XTrainSel), yTrain)
                }

            } else if (method == "pls") {
                if (length(levels(y)) > 2) {
                    pre_symbol <- try(trainedModel <- opls(as.matrix(XTrainSel), yTrain,
                                                           permI = 0, crossValI = 5, info.txtC = "none",
                                                           fig.pdfC = "none"))
                    isError <- is(pre_symbol, "try-error")
                    if (isError) {
                        trainedModel <- opls(as.matrix(XTrainSel), yTrain, predI = 1,
                                             permI = 0, crossValI = 5, info.txtC = "none", fig.pdfC = "none")
                    }
                } else {
                    pre_symbol <- try(trainedModel <- opls(as.matrix(XTrainSel), yTrain, orthoI = NA,
                                                           permI = 0, crossValI = 5, info.txtC = "none",
                                                           fig.pdfC = "none"))
                    isError <- is(pre_symbol, "try-error")
                    if (isError) {
                        trainedModel <- opls(as.matrix(XTrainSel), yTrain, predI = 1,
                                             permI = 0, crossValI = 5, info.txtC = "none", fig.pdfC = "none")
                    }
                }
            } else if (method == "logisticRegression") {
                cv.lasso <- cv.glmnet(XTrainSel, yTrain, alpha = 1, family = "binomial")
                trainedModel <- glmnet(XTrainSel, yTrain, alpha = 1, family = "binomial",
                                       lambda = cv.lasso$lambda.min)
            }
            if  (method == "logisticRegression") {
                if (nFolds > 1) {
                    if (length(folds[[iFold]]) == 1) {
                        yPred[folds[[iFold]]] <- predict(trainedModel, newx = as.vector(t(XTestSel)), type = "class")
                    } else {
                        yPred[folds[[iFold]]] <- predict(trainedModel, newx = as.matrix(XTestSel), type = "class")
                    }
                } else {
                    yPred <- predict(trainedModel, newx = XTest, type = "class")
                }
            } else {
                if (nFolds > 1) {
                    if (length(folds[[iFold]]) == 1) {
                      yPred[folds[[iFold]]] <- predict(trainedModel, newdata = as.vector(t(XTestSel)))
                    } else {
                      yPred[folds[[iFold]]] <- predict(trainedModel, newdata = as.matrix(XTestSel))
                    }
                } else {
                    yPred <- predict(trainedModel, newdata = XTest)
                }
            }

            if (nPerms > 0 & nFolds > 1) {
                for (iPerm in 1:nPerms) {
                  print(paste("Repetition:", iRep, "Permutation:", iPerm, "Fold:", iFold, "/", nFolds, sep = " "))

                  # null model 1 (random feature set of same length), only if feature selection is used
                  if (!(featureMethod == "none")) {
                    if (length(selFeatures) == 1) {
                      indPerm <- randperm(dim(X)[2])[1]
                    } else {
                      indPerm <- randperm(dim(X)[2])[1:dim(XTrainSel)[2]]
                    }
                    XTrainSel_randFeatures <- XTrain[, indPerm]
                    if (length(folds[[iFold]]) == 1) {
                      XTestSel_randFeatures <- XTest[indPerm]
                    } else {
                      XTestSel_randFeatures <- XTest[, indPerm]
                    }

                    if (method == "randomForest") {
                        if (type == "classification") {
                            trainedModel <- randomForest(as.matrix(XTrainSel_randFeatures), yTrain, sampsize = rep(min(summary(yTrain)), nClasses))
                        } else {
                            trainedModel <- randomForest(as.matrix(XTrainSel_randFeatures), yTrain)
                        }
                    } else if (method == "pls") {
                        if (length(levels(y)) > 2) {
                            pre_symbol <- try(trainedModel <- opls(XTrainSel_randFeatures, yTrain,
                                                                   permI = 0, crossValI = 5, info.txtC = "none",
                                                                   fig.pdfC = "none"))
                            isError <- is(pre_symbol, "try-error")
                            if (isError) {
                                trainedModel <- opls(XTrainSel_randFeatures, yTrain, predI = 1,
                                                     permI = 0, crossValI = 5, info.txtC = "none",
                                                     fig.pdfC = "none")

                            }
                        } else {
                          pre_symbol <- try(trainedModel <- opls(XTrainSel_randFeatures, yTrain, orthoI = NA,
                                                                 permI = 0, crossValI = 5, info.txtC = "none",
                            fig.pdfC = "none"))
                          isError <- is(pre_symbol, "try-error")
                          if (isError) {
                              trainedModel <- opls(XTrainSel_randFeatures, yTrain, predI = 1,
                                                   permI = 0, crossValI = 5, info.txtC = "none",
                                                   fig.pdfC = "none")

                          }
                        }
                    } else if (method == "logisticRegression") {
                        cv.lasso <- cv.glmnet(XTrainSel_randFeatures, yTrain, alpha = 1, family = "binomial")
                        trainedModel <- glmnet(XTrainSel_randFeatures, yTrain, alpha = 1, family = "binomial",
                                               lambda = cv.lasso$lambda.min)
                    }

                    if (length(folds[[iFold]]) == 1) {
                      yPred_randFeatures[folds[[iFold]], iPerm] <- predict(trainedModel, newdata = as.matrix(t(XTestSel_randFeatures)))
                    } else {
                      yPred_randFeatures[folds[[iFold]], iPerm] <- predict(trainedModel, newdata = as.matrix(XTestSel_randFeatures))
                    }
                  }

                  # null model 2 feature selection and model training is done with permuted data):
                  y_permutedLabels <- y[sample(1:length(y), size = length(y), replace = FALSE)]
                  yTrain_permutedLabels <- y_permutedLabels[-folds[[iFold]]]
                  selFeaturesPerm <- featureSelection(XTrain, yTrain_permutedLabels,
                                                      method = featureMethod, type = type, chooseS = chooseS,
                                                      nFeatRep = nFeatRep, nLassoFolds = nLassoFolds,
                                                      thresh = thresh, alpha = alpha)
                  XTrainSel_permutedLabels <- XTrain[, which(colnames(X) %in% selFeaturesPerm)]
                  if (length(folds[[iFold]]) == 1) {
                    XTestSel_permutedLabels <- XTest[which(colnames(X) %in% selFeaturesPerm)]
                  } else {
                    XTestSel_permutedLabels <- XTest[, which(colnames(X) %in% selFeaturesPerm)]
                  }

                  if (type == "classification") {
                    yTrain_permutedLabels <- as.factor(yTrain_permutedLabels)
                  } else {
                    yTrain_permutedLabels <- as.vector(yTrain_permutedLabels)
                  }

                  if (method == "randomForest") {
                    if (type == "classification") {
                        trainedModel <- randomForest(as.matrix(XTrainSel_permutedLabels), yTrain_permutedLabels,
                                                     sampsize = rep(min(summary(yTrain_permutedLabels)), nClasses))
                    } else {
                        trainedModel <- randomForest(as.matrix(XTrainSel_permutedLabels), yTrain_permutedLabels)
                    }
                  } else if (method == "pls") {
                      if (length(levels(y)) > 2) {
                          pre_symbol <- try(trainedModel <- opls(as.matrix(XTrainSel_permutedLabels), yTrain_permutedLabels,
                                                                 permI = 0, crossValI = 5, info.txtC = "none",
                                                                 fig.pdfC = "none"))
                          isError <- is(pre_symbol, "try-error")
                          if (isError) {
                              trainedModel <- opls(as.matrix(XTrainSel_permutedLabels), yTrain_permutedLabels, predI = 1,
                                                   permI = 0, crossValI = 5, info.txtC = "none",
                                                   fig.pdfC = "none")
                          }
                      } else {
                        pre_symbol <- try(trainedModel <- opls(as.matrix(XTrainSel_permutedLabels), yTrain_permutedLabels, orthoI = NA,
                                                               permI = 0, crossValI = 5, info.txtC = "none",
                          fig.pdfC = "none"))
                        isError <- is(pre_symbol, "try-error")
                        if (isError) {
                            trainedModel <- opls(as.matrix(XTrainSel_permutedLabels), yTrain_permutedLabels, predI = 1,
                                                 permI = 0, crossValI = 5, info.txtC = "none",
                                                 fig.pdfC = "none")
                        }
                      }
                  } else if (method == "logisticRegression") {
                      cv.lasso <- cv.glmnet(XTrainSel_permutedLabels, yTrain_permutedLabels, alpha = 1, family = "binomial")
                      trainedModel <- glmnet(XTrainSel_permutedLabels, yTrain_permutedLabels, alpha = 1, family = "binomial",
                                                 lambda = cv.lasso$lambda.min)
                  }
                  if (length(folds[[iFold]]) == 1) {
                    yPred_permutedLabels[folds[[iFold]], iPerm] <- predict(trainedModel, newdata = as.vector(XTestSel_permutedLabels))
                  } else {
                    yPred_permutedLabels[folds[[iFold]], iPerm] <- predict(trainedModel, newdata = as.matrix(XTestSel_permutedLabels))
                  }
                }
            }
        }

        if (type == "classification") {
            yPred <- levels(y)[as.numeric(yPred)]
            acc[iRep] <- length(which(yPred == y))/length(y)
            if (nPerms > 0) {
                for (iPerm in 1:nPerms) {
                    yPred_randFeatures_tmp <- levels(y)[yPred_randFeatures[,iPerm]]
                    yPred_permutedLabels_tmp <- levels(y)[yPred_permutedLabels[,iPerm]]
                    acc_randFeatures[iRep, iPerm] <- length(which(yPred_randFeatures_tmp == y_permutedLabels))/length(y)
                    acc_permutedLabels[iRep, iPerm] <- length(which(yPred_permutedLabels_tmp == y_permutedLabels))/length(y)
                }
            }
        } else {
            corr[iRep] <- cor(yPred, y)
            rmses[iRep] <- sqrt(mean((y - yPred)^2))
            if (nPerms > 0) {
                for (iPerm in 1:nPerms) {
                  corr_randFeatures[iRep, iPerm] <- cor(yPred_randFeatures[, iPerm], y)
                  rmses_randFeatures[iRep, iPerm] <- sqrt(mean((y - yPred_randFeatures[, iPerm])^2))
                  corr_permutedLabels[iRep, iPerm] <- cor(yPred_permutedLabels[, iPerm], y_permutedLabels)
                  rmses_permutedLabels[iRep, iPerm] <- sqrt(mean((y_permutedLabels - yPred_permutedLabels[, iPerm])^2))
                }
            }
        }
        if (saveFlag) {
            if (type == "classification") {
                if (nPerms > 0) {
                    output = list(acc = acc,
                                  acc_randFeatures = acc_randFeatures,
                                  acc_permutedLabels = acc_permutedLabels)
                } else {
                    if (yPredOut) {
                        output = list(acc = acc, yPred = yPred)
                    } else {
                        output = list(acc = acc)
                    }
                }
            } else {
                if (nPerms > 0) {
                    output = list(corr = corr,
                                  corr_randFeatures = corr_randFeatures,
                                  corr_permutedLabels = corr_permutedLabels,
                                  rmses = rmses,
                                  rmses_randFeatures = rmses_randFeatures,
                                  rmses_permutedLabels = rmses_permutedLabels)
                } else {
                    if (yPredOut) {
                        output = list(corr = corr,
                                      rmses = rmses,
                                      yPred = yPred)
                    } else {
                        output = list(corr = corr,
                                      rmses = rmses)
                    }
                }
            }
            saveRDS(output, file = paste(fileStr, ".rds", sep = ""))
        }
    }
    if (type == "classification") {
        if (nPerms > 0) {
          if (featureMethod == "none") {
            output = list(acc = acc,
                          acc_permutedLabels = acc_permutedLabels)
          } else {
            output = list(acc = acc,
                          acc_randFeatures = acc_randFeatures,
                          acc_permutedLabels = acc_permutedLabels)
          }
        } else {
            if (yPredOut) {
                output = list(acc = acc, yPred = yPred)
            } else {
                output = list(acc = acc)
            }
        }
    } else {
        if (nPerms > 0) {
          if (featureMethod == "none") {
            output = list(corr = corr,
                          corr_permutedLabels = corr_permutedLabels,
                          rmses = rmses,
                          rmses_permutedLabels = rmses_permutedLabels)
          } else {
            output = list(corr = corr,
                          corr_randFeatures = corr_randFeatures,
                          corr_permutedLabels = corr_permutedLabels,
                          rmses = rmses,
                          rmses_randFeatures = rmses_randFeatures,
                          rmses_permutedLabels = rmses_permutedLabels)
          }
        } else {
            if (yPredOut) {
                output = list(corr = corr,
                              rmses = rmses,
                              yPred = yPred)
            } else {
                output = list(corr = corr,
                              rmses = rmses)
            }
        }
    }

    return(output)
}
