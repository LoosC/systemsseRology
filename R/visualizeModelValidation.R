#' Generates figures for the model validation using permutation tests and returns the p-values
#'
#' @export
#' @param res result file obtained from modelValidation
#' @param type "classification" or "regression"
#' @param saveFlag TRUE or FALSE
#' @param fileStrPart string where figures should be saved,
#' will be extended with "_acc" for classification or "_corr" and "_rmses" for regression problems
#' @param modelColor color for model violin plot
#' @param rotate_xtick_labels angle with with x labels should be rotated
#' @return p-values for model and plotting handles

visualizeModelValidation <- function(res,
                                     type,
                                     saveFlag = FALSE,
                                     fileStrPart = "modelValidation",
                                     modelColor = "gray",
                                     rotate_xtick_labels = 0,
                                     fig_width = 5,
                                     fig_height = 3) {
    if (rotate_xtick_labels == 0) {
        hjust_tmp = 0.5
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

        # Calculate exact p-values
        pvals <- data.frame(acc_randFeatures = vector(length = length(res$acc)), acc_permutedLabels = vector(length = length(res$acc)))
        for (iRep in 1:length(res$acc)) {
            pvals$acc_randFeatures[iRep] <- length(which(res$acc_randFeatures[iRep, ] > res$acc[iRep]))/dim(res$acc_randFeatures)[2]
            pvals$acc_permutedLabels[iRep] <- length(which(res$acc_permutedLabels[iRep, ] > res$acc[iRep]))/dim(res$acc_permutedLabels)[2]
        }
        if (median(pvals$acc_randFeatures) == 0) {
            medPval_rand <- paste("p<", toString(1/length(as.vector(res$acc_randFeatures))), sep = "")
        } else {
            medPval_rand <- paste("p=", toString(median(pvals$acc_randFeatures)), sep = "")
        }
        if (median(pvals$acc_permutedLabels) == 0) {
            medPval_lab <- paste("p<", toString(1/length(as.vector(res$acc_permutedLabels))), sep = "")
        } else {
            medPval_lab <- paste("p=", toString(median(pvals$permutedLabels)), sep = "")
        }


        # Generate violin plot for accuracies
        pltVio <- ggplot(dfBox, aes(x = model, y = value, fill = model)) +
            geom_violin() +
            stat_summary(fun.data = "mean_sd", geom = "pointrange", size = 0.3,  color = "black") +
            #stat_summary(fun = "mean", colour = "black", size = 2, geom = "point") +
            scale_fill_manual(values = c(modelColor, "gray", "gray")) +
            #labs(x = "") +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = rotate_xtick_labels, hjust = hjust_tmp)) +
            geom_bracket(xmin = 1, xmax = 2, inherit.aes = FALSE, label.size = 3,
                         y.position = 1, label = medPval_rand) +
            geom_bracket(xmin = 1, xmax = 3, inherit.aes = FALSE, label.size = 3,
                         y.position = 1.07, label = medPval_lab) +
            scale_y_continuous("accuracy", breaks = c(0,0.5,1), labels = c("0", "0.5", "1"), limits = c(0,1.12)) +
            scale_x_discrete("", labels = c("model", "random \nfeatures", "permuted \nlabels"))


        if (saveFlag) {
            pdf(paste(fileStrPart, "_acc.pdf", sep = ""), width = fig_width, height = fig_height)
        }
        print(pltVio)
        dev.off()
        print(pltVio)


        return(list(pvals = pvals, pltVio = pltVio))

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
                         size = 1, geom = "point") +
            stat_summary(fun.data = mean_sd, geom = "pointrange", color = "black") +
            scale_fill_manual(values = c(modelColor,"gray", "gray")) +
            labs(y = "correlation") +
            scale_x_discrete("", labels = c("model", "random \nfeatures", "permuted \nlabels")) +
            ylim(c(min(dfBox$value), max(dfBox$value))) +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = rotate_xtick_labels, hjust = hjust_tmp)) +
            geom_bracket(xmin = 1, xmax = 2, inherit.aes = FALSE,
                         y.position = max(dfBox$value), label = medPval_rand, label.size = 3) +
            geom_bracket(xmin = 1, xmax = 3, inherit.aes = FALSE,
                         y.position = (max(dfBox$value) + 0.05), label = medPval_lab, label.size = 3)

        if (saveFlag) {
            pdf(paste(fileStrPart, "_corr.pdf", sep = ""), width = fig_width, height = fig_height)
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
            scale_x_discrete("", labels = c("model", "random \nfeatures", "permuted \nlabels")) +
            labs(y = "RSME") +
            ylim(c(min(dfBox$value), max(dfBox$value))) +
            theme_classic() +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = rotate_xtick_labels, hjust = hjust_tmp)) +
            geom_bracket(xmin = 1, xmax = 2, inherit.aes = FALSE,
                         y.position = max(dfBox$value), label = medPval_rand) +
            geom_bracket(xmin = 1, xmax = 3, inherit.aes = FALSE,
                         y.position = (max(dfBox$value) + 0.05), label = medPval_lab)
        if (saveFlag) {
            pdf(paste(fileStrPart, "_rmses.pdf", sep = ""), width = fig_width, height = fig_height)
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
        return(list(pvals = pvals, pltVio_corr = pltVio1, pltVio_rmses = pltVio2))
    }
}


