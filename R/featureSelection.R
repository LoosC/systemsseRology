#' @export

featureSelection <- function(X, y, method = "lasso", type = "classification",
                             nFeatRep = 100, chooseS = "min", nLassoFolds = 5, thresh = 1,
                             alpha = 1) {

    if (method == "none") {
        selFeatures <- colnames(X)
    } else if (method == "randomForest_RFE") {
        X_red <- X
        featSel <- data.frame(matrix(1, ncol = dim(X)[2], nrow = dim(X)[2] + 1))
        rownames(featSel) <- c(colnames(X), "oob")
        colnames(featSel) <- 1:dim(X)[2]
        for (ind in 1:(dim(X)[2])) {
            if (type == "classification") {
                rf <- randomForest(as.matrix(X_red), as.factor(y))
                featSel[dim(X)[2] + 1, ind] <- mean(predict(rf) != y)
            } else {
                rf <- randomForest(as.matrix(X_red), as.vector(y))
                featSel[dim(X)[2] + 1, ind] <- mean(sqrt(mean((y - predict(rf))^2)))
            }
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
        # if (type == "regression") {
        #     y_vec <- rep(NA, length(y))
        #     for (indClass in 1:length(unique(y))) {
        #         y_vec[which(y == levels(y)[indClass])] <- indClass - 1
        #     }
        # }
        for (iRep in 1:nFeatRep) {
            if (type == "classification") {
              resLasso <- cv.glmnet(X, y, type.measure = "mse", alpha = alpha,
                                    family = "binomial", nfolds = nLassoFolds)
            } else {
              resLasso <- cv.glmnet(X, y, type.measure = "mse", alpha = alpha,
                                    family = "gaussian", nfolds = nLassoFolds)
            }
            mses[iRep] <- resLasso$cvm[which(resLasso$lambda == resLasso$lambda.min)]
            if (chooseS == "min") {
                c <- coef(resLasso, s = "lambda.min")
            } else {
                c <- coef(resLasso, s = "lambda.1se")  # check again
            }
            tmpInds <- 1
            for (ind in 2:length(c)) {
                if (c[ind] != 0) {
                  tmpInds <- c(tmpInds, ind)
                }
            }
            selec <- rep(0, dim(X)[2] + 1)
            selec[tmpInds] <- 1
            tmpFeat <- cbind(tmpFeat, selec)

            # last coefficient to set to 0
            indLastZero <- which(resLasso$glmnet.fit$df > 1)[1]
            if (!is.na(indLastZero)) {
                c <- coef(resLasso, s = resLasso$lambda[indLastZero])
                tmpInds <- 1
                for (ind in 2:length(c)) {
                  if (c[ind] != 0) {
                    tmpInds <- c(tmpInds, ind)
                  }
                }
                selec <- rep(0, dim(X)[2] + 1)
                selec[tmpInds] <- 1
                lastZero <- cbind(lastZero, selec)
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
        } else if (method == "lasso_min_mse") {
            indSel2 <- which(tmpFeat[, which(mses == min(mses)) + 1] == 1)
            if (length(indSel2) < 3) {
                selFeatures <- tmpFeat$features[indSel]
                selFeatures <- selFeatures[2:length(selFeatures)]  # remove intercept
            } else {
                selFeatures <- tmpFeat$features[indSel2]
                selFeatures <- selFeatures[2:length(selFeatures)]  # remove intercep
            }
        }

    }
    return(selFeatures)
}
