#' Generates figures for the model validation using permutation tests and returns the p-values
#'
#' @export
#' @param res result file obtained from modelValidation
#' @param type "classification" or "regression"
#' @param saveFlag TRUE or FALSE
#' @param fileStrPart string where figures should be saved,
#' will be extended with "_acc" for classification or "_corr" and "_rmses" for regression problems
#' @param modelColor color for model violin plot
#' @param rotate_x_labels angle with with x labels should be rotated
#' @return p-values for model

visualizeModelValidation <- function(res,
                                     type,
                                     saveFlag = FALSE,
                                     fileStrPart = "modelValidation.pdf",
                                     modelColor = "gray",
                                     rotate_x_labels = 0) {
    if (!(rotate_x_labels == 0)) {
        hjust_tmp = 0
    } else {
        hjust_tmp = 1
    }
    if (type == "classification") {
        value <- c(as.vector(res$acc),
                   as.vector(res$acc_randFeatures),
                   as.vector(res$acc_permutedLabels))

        dfBox <- data.frame(value = value,
                            models = rep(c("model",
                                          "random features",
                                          "permuted labels"),
                                        c(length(as.vector(res$acc)),
                                          length(as.vector(res$acc_randFeatures)),
                                          length(as.vector(res$acc_permutedLabels)))))

        dfBox$model = factor(dfBox$model, levels = c("model", "random features", "permuted labels"))

        myComparisons <- list(c("model", "permuted labels"),
                              c("model", "random features"))

        # Generate violion plot for accuracies
        pltVio <- ggplot(dfBox, aes(x = models, y = value, fill = model)) +
            geom_violin() +
            stat_summary(fun.y = "mean", colour = "black", size = 2, geom = "point") +
            stat_summary(fun.data = "mean_sd", geom = "pointrange", color = "black") +
            scale_fill_manual(values = c(modelColor, "gray", "gray")) +
            labs(x = "", y = "accuracy") +
            ylim(c(0,1)) +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  strip.placement = "outside",
                  axis.line = element_line(colour = "black"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.text.x = element_text(angle = rotate_x_label, hjust = hjust_tmp)) +
            stat_compare_means(comparisons = myComparisons, size = 0)

        if (saveFlag) {
            pdf(paste(fileStrPart, "_acc.pdf", sep = ""), width = 5, height = 3)
        }
        print(pltVio)
        dev.off()
        print(pltVio)

        # Calculate exact p-values
        pvals <- data.frame(acc_randFeatures = vector(length = length(res$acc)), acc_permutedLabels = vector(length = length(res$acc)))
        for (iRep in 1:length(res$acc)) {
            pvals$acc_randFeatures[iRep] <- length(which(res$acc_randFeatures[iRep, ] > res$acc[iRep]))/dim(res$acc_randFeatures)[2]
            pvals$acc_permutedLabels[iRep] <- length(which(res$acc_permutedLabels[iRep, ] > res$acc[iRep]))/dim(res$acc_permutedLabels)[2]
        }
    } else {
        value <- c(as.vector(res$corr),
                   as.vector(res$corr_randFeatures),
                   as.vector(res$corr_permutedLabels))
        dfBox <- data.frame(value = value,
                            model = rep(c("model",
                                          "random features",
                                          "permuted labels"),
                                        c(length(as.vector(res$corr)),
                                          length(as.vector(res$corr_randFeatures)),
                                          length(as.vector(res$corr_permutedLabels)))))
        dfBox$model = factor(dfBox$model, levels = c("model", "random features", "permuted labels"))
        myComparisons <- list(c("model", "permuted labels"),
                              c("model", "random features"))

        # Generate violion plot for correlations
        pltVio1 <- ggplot(dfBox, aes(x = model, y = value, fill = model)) +
            geom_violin() +
            stat_summary(fun.y = "mean", colour = "black",
                         size = 2, geom = "point") +
            stat_summary(fun.data = mean_sd, geom = "pointrange", color = "black") +
            scale_fill_manual(values = c(modelColor,"gray", "gray")) +
            labs(x = "", y = "correlation") +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  strip.placement = "outside",
                  axis.line = element_line(colour = "black"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.text.x = element_text(angle = rotate_x_label, hjust = hjust_tmp)) +
            stat_compare_means(comparisons = myComparisons, size = 0)
        if (saveFlag) {
            pdf(paste(fileStrPart, "_corr.pdf", sep = ""), width = 5, height = 3)
        }
        print(pltVio1)
        dev.off()
        print(pltVio1)

        value <- c(as.vector(res$rmses),
                   as.vector(res$rmses_randFeatures),
                   as.vector(res$rmses_permutedLabels))
        dfBox$value <- value

        # Generate violion plot for mean squared errors
        pltVio2 <- ggplot(dfBox, aes(x = model, y = value, fill = model)) +
            geom_violin() +
            stat_summary(fun.y = "mean", colour = "black", size = 2, geom = "point") +
            stat_summary(fun.data = mean_sd, geom = "pointrange", color = "black") +
            scale_fill_manual(values = c(modelColor, "gray", "gray")) +
            labs(x = "", y = "RSME") +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  strip.placement = "outside",
                  axis.line = element_line(colour = "black"),
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.text.x = element_text(angle = rotate_x_label, hjust = hjust_tmp)) +
            stat_compare_means(comparisons = myComparisons, size = 0)
        if (saveFlag) {
            pdf(paste(fileStrPart, "_rmses.pdf", sep = ""), width = 5, height = 3)
        }
        print(pltVio2)
        dev.off()
        print(pltVio2)

        # Calculate exact p-values
        pvals <- data.frame(corr_randFeatures = vector(length = length(res$corr)),
                            corr_permutedLabels = vector(length = length(res$corr)),
                            rmse_randFeatures = vector(length = length(res$corr)),
                            rmse_permutedLabels = vector(length = length(res$corr)))
        for (iRep in 1:length(res$corr)) {
            pvals$corr_randFeatures[iRep] <- length(which(res$corr_randFeatures[iRep, ] > res$corr[iRep]))/dim(res$corr_randFeatures)[2]
            pvals$corr_permutedLabels[iRep] <- length(which(res$corr_permutedLabels[iRep, ] > res$corr[iRep]))/dim(res$corr_randFeatures)[2]
            pvals$rmse_randFeatures[iRep] <- length(which(res$rmses_randFeatures[iRep, ] < res$rmses[iRep]))/dim(res$corr_randFeatures)[2]
            pvals$rmse_permutedLabels[iRep] <- length(which(res$rmses_permutedLabels[iRep, ] < res$rmses[iRep]))/dim(res$corr_randFeatures)[2]
        }
    }
    return(pvals)
}


