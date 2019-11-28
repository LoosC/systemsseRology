featureSelection <- function(X, y, method = "lasso", type = "classification", 
                             nFeatRep = 100, chooseS = "min", nLassoFolds = 5, thresh = 1, 
                             alpha = 1) {
    
    if (method == "none") {
        selFeatures <- colnames(X)
    } else if (method == "randomForest_RFE") {
        XRed <- X
        featSel <- data.frame(matrix(1, ncol = dim(X)[2], nrow = dim(X)[2] + 1))
        rownames(featSel) <- c(colnames(X), "oob")
        colnames(featSel) <- 1:dim(X)[2]
        for (ind in 1:(dim(X)[2])) {
            if (type == "classification") {
                rf <- randomForest(as.matrix(XRed), as.factor(y))
                featSel[dim(X)[2] + 1, ind] <- mean(predict(rf) != y)
            } else {
                rf <- randomForest(as.matrix(XRed), as.vector(y))
                featSel[dim(X)[2] + 1, ind] <- mean(sqrt(mean((y - predict(rf))^2)))
            }
            if (ind < dim(X)[2]) {
                remFeature <- rownames(rf$importance)[which(rf$importance == min(rf$importance))[1]]
                XRed <- XRed[, -which(colnames(XRed) == remFeature)]
                featSel[remFeature, ind:dim(X)[2]] <- 0
            }
        }
        selFeatures <- colnames(X)[which(featSel[1:(dim(featSel)[1] - 1), 
                                                 which(featSel["oob", ] == min(featSel["oob", ]))] == 1)]
    } else if (method == "lasso") {
        tmpFeat <- data.frame(features = c("(Intercept)", colnames(X)))
        lastZero <- data.frame(features = c("(Intercept)", colnames(X)))
        mses <- rep(NA, nFeatRep)
        for (iRep in 1:nFeatRep) {
            resLasso <- cv.glmnet(X, y, type.measure = "mse", alpha = alpha, 
                                  family = "gaussian", nfolds = nLassoFolds)
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
        indSel <- which(rowSums(tmpFeat[, -1]) > thresh)
        thresh2 <- thresh - 1
        while (length(indSel) < 3 && thresh2 > 0) {
            indSel <- which(rowSums(tmpFeat[, -1]) > thresh2)
            thresh2 <- thresh2 - 1
        }
        if (length(indSel) < 3 & !is.na(indLastZero)) {
            indSel <- which(rowSums(lastZero[, -1]) > thresh)
            thresh2 <- thresh - 1
            while (length(indSel) < 3 && thresh2 > 0) {
                indSel <- which(rowSums(lastZero[, -1]) > thresh2)
                thresh2 <- thresh2 - 1
            }
        }
        selFeatures <- tmpFeat$features[indSel]
        selFeatures <- selFeatures[2:length(selFeatures)]  # remove intercept
    }
    return(selFeatures)
}
