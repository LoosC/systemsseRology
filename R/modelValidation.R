#' @export

modelValidation <- function(X, y, nFolds = 5, nReps = 10, nPerms = 100,
                            nTop = 5, featureMethod = "lasso", method = "pls",
                            type = "classification",
                            nFeatRep = 100, nLassoFolds = 5, thresh = 1,
                            alpha = 1, chooseS = "min", saveFlag = FALSE,
                            fileStr = "accuraciesFullModel_withPerm") {

    yPred <- matrix(NA, nrow = length(y), ncol = 1)
    acc <- matrix(NA, nrow = nReps, ncol = 1)
    corr <- matrix(NA, nrow = nReps, ncol = 1)
    rmses <- matrix(NA, nrow = nReps, ncol = 1)

    if (nPerms > 0) {
        yPredPerm1 <- matrix(NA, nrow = length(y), ncol = nPerms)
        yPredPerm2 <- matrix(NA, nrow = length(y), ncol = nPerms)
        if (type == "classification") {
            accPerm1 <- matrix(NA, nrow = nReps, ncol = nPerms)
            accPerm2 <- matrix(NA, nrow = nReps, ncol = nPerms)
        } else {
            corrPerm1 <- matrix(NA, nrow = nReps, ncol = nPerms)
            rmsesPerm1 <- matrix(NA, nrow = nReps, ncol = nPerms)
            corrPerm2 <- matrix(NA, nrow = nReps, ncol = nPerms)
            rmsesPerm2 <- matrix(NA, nrow = nReps, ncol = nPerms)
        }
    }

    for (iRep in 1:nReps) {
        print(paste("Repetition:", iRep, sep = " "))
        if (nFolds > 1) {
            folds <- createFolds(y, k = nFolds, list = TRUE)
        }
        for (iFold in 1:nFolds) {
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
            selFeatures <- featureSelection(XTrain, yTrain,
                                            method = featureMethod, type = type,
                                            chooseS = chooseS, nFeatRep = nFeatRep,
                                            nLassoFolds = nLassoFolds,
                                            thresh = thresh, alpha = alpha)
            XTrainSel <- XTrain[, which(colnames(X) %in% selFeatures)]

            if (length(y) == nFolds) {
                XTestSel <- XTest[which(colnames(X) %in% selFeatures)]
            } else {
                XTestSel <- XTest[, which(colnames(X) %in% selFeatures)]
            }

            if (type == "classification") {
                yTrain <- as.factor(yTrain)
            } else {
                yTrain <- as.vector(yTrain)
            }

            if (method == "randomForest") {
                trainedModel <- randomForest(as.matrix(XTrainSel), yTrain)
            } else if (method == "pls") {
                pre_symbol <- try(trainedModel <- opls(as.matrix(XTrainSel), yTrain, orthoI = NA,
                                                       permI = 0, crossValI = 5, info.txtC = "none",
                                                       fig.pdfC = "none"))
                isError <- is(pre_symbol, "try-error")
                if (isError) {
                    trainedModel <- opls(as.matrix(XTrainSel), yTrain, orthoI = 1, predI = 1,
                                         permI = 0, crossValI = 5, info.txtC = "none", fig.pdfC = "none")
                }
            } else if (method == "logisticRegression") {
                cv.lasso <- cv.glmnet(XTrainSel, yTrain, alpha = 1, family = "binomial")
                trainedModel <- glmnet(XTrainSel, yTrain, alpha = 1, family = "binomial",
                                       lambda = cv.lasso$lambda.min)
            }
            if  (method == "logisticRegression") {
                if (nFolds > 1) {
                    if (length(y) == nFolds) {
                        yPred[folds[[iFold]]] <- predict(trainedModel, newx = as.vector(t(XTestSel)), type = "class")
                    } else {
                        yPred[folds[[iFold]]] <- predict(trainedModel, newx = as.matrix(XTestSel), type = "class")
                    }
                } else {
                    yPred <- predict(trainedModel, newx = XTest, type = "class")
                }
            } else {
                if (nFolds > 1) {
                    if (length(y) == nFolds) {
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
                  # null model 1 (random feature set of same length):
                  if (length(selFeatures) == 1) {
                    indPerm <- randperm(dim(X)[2])[1]
                  } else {
                    indPerm <- randperm(dim(X)[2])[1:dim(XTrainSel)[2]]
                  }
                  XTrainSelPerm1 <- XTrain[, indPerm]
                  XTestSelPerm1 <- XTest[, indPerm]

                  if (method == "randomForest") {
                     trainedModel <- randomForest(as.matrix(XTrainSelPerm1), yTrain)
                  } else if (method == "pls") {
                    pre_symbol <- try(trainedModel <- opls(XTrainSelPerm1, yTrain, orthoI = NA,
                                                           permI = 0, crossValI = 5, info.txtC = "none",
                      fig.pdfC = "none"))
                    isError <- is(pre_symbol, "try-error")
                    if (isError) {
                        trainedModel <- opls(XTrainSelPerm1, yTrain, orthoI = 1,  predI = 1,
                                             permI = 0, crossValI = 5, info.txtC = "none",
                                             fig.pdfC = "none")

                    }
                  } else if (method == "logisticRegression") {
                      cv.lasso <- cv.glmnet(XTrainSelPerm1, yTrain, alpha = 1, family = "binomial")
                      trainedModel <- glmnet(XTrainSelPerm1, yTrain, alpha = 1, family = "binomial",
                                             lambda = cv.lasso$lambda.min)
                  }

                  if (length(y) == nFolds) {
                    yPredPerm1[folds[[iFold]], iPerm] <- predict(trainedModel, newdata = as.matrix(t(XTestSelPerm1)))
                  } else {
                    yPredPerm1[folds[[iFold]], iPerm] <- predict(trainedModel, newdata = as.matrix(XTestSelPerm1))
                  }

                  # null model 2 feature selection and model training is done with permuted data):
                  yPerm <- y[sample(1:length(y), size = length(y), replace = FALSE)]
                  yTrainPerm <- yPerm[-folds[[iFold]]]
                  selFeaturesPerm <- featureSelection(XTrain, yTrainPerm,
                                                      method = featureMethod, type = type, chooseS = chooseS,
                                                      nFeatRep = nFeatRep, nLassoFolds = nLassoFolds,
                                                      thresh = thresh, alpha = alpha)
                  XTrainSelPerm2 <- XTrain[, which(colnames(X) %in% selFeaturesPerm)]
                  XTestSelPerm2 <- XTest[, which(colnames(X) %in% selFeaturesPerm)]

                  if (type == "classification") {
                    yTrainPerm <- as.factor(yTrainPerm)
                  } else {
                    yTrainPerm <- as.vector(yTrainPerm)
                  }

                  if (method == "randomForest") {
                    trainedModel <- randomForest(as.matrix(XTrainSelPerm2), yTrainPerm)
                  } else if (method == "pls") {
                    pre_symbol <- try(trainedModel <- opls(as.matrix(XTrainSelPerm2), yTrainPerm, orthoI = NA,
                                                           permI = 0, crossValI = 5, info.txtC = "none",
                      fig.pdfC = "none"))
                    isError <- is(pre_symbol, "try-error")
                    if (isError) {
                        trainedModel <- opls(as.matrix(XTrainSelPerm2), yTrainPerm, predI = 1, orthoI = 1,
                                             permI = 0, crossValI = 5, info.txtC = "none",
                                             fig.pdfC = "none")
                    }
                  } else if (method == "logisticRegression") {
                      cv.lasso <- cv.glmnet(XTrainSelPerm2, yTrainPerm, alpha = 1, family = "binomial")
                      trainedModel <- glmnet(XTrainSelPerm2, yTrainPerm, alpha = 1, family = "binomial",
                                                 lambda = cv.lasso$lambda.min)
                  }
                  if (length(y) == nFolds) {
                    yPredPerm2[folds[[iFold]], iPerm] <- predict(trainedModel, newdata = as.vector(XTestSelPerm2))
                  } else {
                    yPredPerm2[folds[[iFold]], iPerm] <- predict(trainedModel, newdata = as.matrix(XTestSelPerm2))
                  }
                }
            }
        }

        if (type == "classification") {
            yPred[which(yPred == 1)] <- levels(y)[1]
            yPred[which(yPred == 2)] <- levels(y)[2]

            acc[iRep] <- length(which(yPred == y))/length(y)
            if (nPerms > 0) {
                yPredPerm1[which(yPredPerm1 == 1)] <- levels(y)[1]
                yPredPerm1[which(yPredPerm1 == 2)] <- levels(y)[2]

                yPredPerm2[which(yPredPerm2 == 1)] <- levels(y)[1]
                yPredPerm2[which(yPredPerm2 == 2)] <- levels(y)[2]
                for (iPerm in 1:nPerms) {
                  accPerm1[iRep, iPerm] <- length(which(yPredPerm1[, iPerm] == y))/length(y)
                  accPerm2[iRep, iPerm] <- length(which(yPredPerm2[, iPerm] == y))/length(y)
                }
            }
        } else {
            corr[iRep] <- cor(yPred, y)
            rmses[iRep] <- sqrt(mean((y - yPred)^2))
            if (nPerms > 0) {
                for (iPerm in 1:nPerms) {
                  corrPerm1[iRep, iPerm] <- cor(yPredPerm1[, iPerm], y)
                  rmsesPerm1[iRep, iPerm] <- sqrt(mean((y - yPredPerm1[, iPerm])^2))
                  corrPerm2[iRep, iPerm] <- cor(yPredPerm2[, iPerm], y)
                  rmsesPerm2[iRep, iPerm] <- sqrt(mean((y - yPredPerm2[, iPerm])^2))
                }
            }
        }
    }

    if (saveFlag) {
        if (type == "classification") {
            if (nPerms > 0) {
                output = list(acc = acc,
                              accPerm1 = accPerm1,
                              accPerm2 = accPerm2)
            } else {
                output = list(acc = acc)
            }
        } else {
            if (nPerms > 0) {
                output = list(corr = corr,
                              corrPerm1 = corrPerm1,
                              corrPerm2 = corrPerm2,
                              rmses = rmses,
                              rmsesPerm1 = rmsesPerm1,
                              rmsesPerm2 = rmsesPerm2)
            } else {
                output = list(corr = corr,
                              rmses = rmses)
            }
        }
        save(output, file = paste(fileStr, ".RData", sep = ""))
    }

    if (type == "classification") {
        if (nPerms > 0) {
            output = list(acc = acc,
                          accPerm1 = accPerm1,
                          accPerm2 = accPerm2)
        } else {
            output = list(acc = acc)
        }
    } else {
        if (nPerms > 0) {
            output = list(corr = corr,
                          corrPerm1 = corrPerm1,
                          corrPerm2 = corrPerm2,
                          rmses = rmses,
                          rmsesPerm1 = rmsesPerm1,
                          rmsesPerm2 = rmsesPerm2)
        } else {
            output = list(corr = corr,
                          rmses = rmses)
        }
    }

    return(output)
}
