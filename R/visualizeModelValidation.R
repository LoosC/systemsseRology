#' @export

visualizeModelValidation <- function(res, type, saveFlag = FALSE, strSave = "", modelColor = "gray") {

    if (type == "classification") {
        value <- c(as.vector(res$acc),
                   as.vector(res$accPerm1),
                   as.vector(res$accPerm2))
        dfBox <- data.frame(value = value,
                            model = rep(c("model",
                                          "random features",
                                          "permuted labels"),
                                        c(length(as.vector(res$acc)),
                                          length(as.vector(res$accPerm1)),
                                          length(as.vector(res$accPerm2)))))

        dfBox$model = factor(dfBox$model, levels = c("model", "random features", "permuted labels"))
        myComparisons <- list(c("model", "permuted labels"),
                              c("model", "random features"))

        pltVio <- ggplot(dfBox, aes(x = model, y = value, fill = model)) +
            geom_violin() +
            stat_summary(fun.y = "mean", colour = "black", size = 2, geom = "point") +
            stat_summary(fun.data = mean_sd, geom = "pointrange", color = "black") +
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
                  legend.position = "none") +
            stat_compare_means(comparisons = myComparisons, size = 0)

        if (saveFlag) {
            pdf(paste("Figures/modelValidation_acc_", strSave, ".pdf", sep = ""), width = 5, height = 3)
        }
        print(pltVio)
        dev.off()
        print(pltVio)
        pvals <- data.frame(acc1 = vector(length = length(res$acc)), acc2 = vector(length = length(res$acc)))
        for (iRep in 1:length(res$acc)) {
            pvals$acc1[iRep] <- length(which(res$accPerm1[iRep, ] > res$acc[iRep]))/dim(res$accPerm1)[2]
            pvals$acc2[iRep] <- length(which(res$accPerm2[iRep, ] > res$acc[iRep]))/dim(res$accPerm2)[2]
        }
    } else {
        value <- c(as.vector(res$corr),
                   as.vector(res$corrPerm1),
                   as.vector(res$corrPerm2))
        dfBox <- data.frame(value = value,
                            model = rep(c("model",
                                          "random features",
                                          "permuted labels"),
                                        c(length(as.vector(res$corr)),
                                          length(as.vector(res$corrPerm1)),
                                          length(as.vector(res$corrPerm2)))))
        dfBox$model = factor(dfBox$model, levels = c("model", "random features", "permuted labels"))
        myComparisons <- list(c("model", "permuted labels"),
                              c("model", "random features"))

        pltVio1 <- ggplot(dfBox, aes(x = model, y = value, fill = model)) +
            geom_violin() +
            stat_summary(fun.y = "mean", colour = "black",
                         size = 2, geom = "point") +
            stat_summary(fun.data = mean_sd, geom = "pointrange", color = "black") +
            scale_fill_manual(values = c(modelColor,
            "gray", "gray")) + labs(x = "", y = "correlation") +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  strip.placement = "outside",
                  axis.line = element_line(colour = "black"),
                  legend.title = element_blank(),
                  legend.position = "none") +
            stat_compare_means(comparisons = myComparisons, size = 0)
        if (saveFlag) {
            pdf(paste("Figures/modelValidation_corr_", strSave, ".pdf", sep = ""), width = 5, height = 3)
        }
        print(pltVio1)
        dev.off()
        print(pltVio1)

        value <- c(as.vector(res$rmses),
                   as.vector(res$rmsesPerm1),
                   as.vector(res$rmsesPerm2))
        dfBox$value <- value
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
                  legend.position = "none") +
            stat_compare_means(comparisons = myComparisons, size = 0)
        if (saveFlag) {
            pdf(paste("Figures/modelValidation_rmses_", strSave, ".pdf", sep = ""), width = 5, height = 3)
        }
        print(pltVio2)
        dev.off()
        print(pltVio2)
        pvals <- data.frame(corr1 = vector(length = length(res$corr)),
                            corr2 = vector(length = length(res$corr)),
                            rmse1 = vector(length = length(res$corr)),
                            rmse2 = vector(length = length(res$corr)))
        for (iRep in 1:length(res$corr)) {
            pvals$corr1[iRep] <- length(which(res$corrPerm1[iRep, ] > res$corr[iRep]))/dim(res$corrPerm1)[2]
            pvals$corr2[iRep] <- length(which(res$corrPerm2[iRep, ] > res$corr[iRep]))/dim(res$corrPerm1)[2]
            pvals$rmse1[iRep] <- length(which(res$rmsesPerm1[iRep, ] < res$rmses[iRep]))/dim(res$corrPerm1)[2]
            pvals$rmse2[iRep] <- length(which(res$rmsesPerm2[iRep, ] < res$rmses[iRep]))/dim(res$corrPerm1)[2]
        }
    }
    return(pvals)
}


