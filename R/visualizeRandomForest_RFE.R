#' Plots the out-of-bag error for recursive feature elimination (RFE) for random
#' forest
#' @param X data matrix
#' @param y label
#' @export

visualizeRandomForest_RFE <- function(X, y) {
    
    XRed <- X
    featSel <- data.frame(matrix(1, ncol = dim(X)[2], nrow = dim(X)[2] + 1))
    rownames(featSel) <- c(colnames(X), "oob")
    colnames(featSel) <- 1:dim(X)[2]
    
    for (ind in 1:(dim(X)[2] - 1)) {
        rf <- randomForest(as.matrix(XRed), as.factor(y))
        featSel["oob", ind] <- mean(predict(rf) != y)
        remFeature <- rownames(rf$importance)[which(rf$importance == min(rf$importance))]
        XRed <- XRed[, -which(colnames(XRed) == remFeature)]
        featSel[remFeature, ind:dim(X)[2]] <- 0
    }
    featSel["oob", dim(X)[2]] <- NA
    
    dfPlot <- data.frame(oob = t(featSel["oob", ]), iteration = c(1:80))
    p <- ggplot(dfPlot, aes(iteration, oob)) + geom_point() + geom_line()
    plot(p)
}
