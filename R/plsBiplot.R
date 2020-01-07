#' @export

plsBiplot <- function(oplsda, y, saveFlag = FALSE, fileStr = "", color1 = "red", color2 = "blue", shape1 = 21, shape2 = 21, fontsize = 4,
    alpha_loading = 1, feature_annot = data.frame()) {

    scoresLV1 <- getScoreMN(oplsda)
    scoresTmp <- getScoreMN(oplsda, orthoL = TRUE)
    dfScores <- data.frame(LV1 = scoresLV1,
                           LV2 = scoresTmp[, 1],
                           class = y)
    colnames(dfScores) <- c("LV1", "LV2", "class")

    pltScores <- ggplot(dfScores, aes(LV1, LV2, color = class, shape = class)) +
        geom_point(aes(fill = class), color = "black", size = 3,
                   stroke = 0.6) + theme(aspect.ratio = 1) +
        labs(x = paste("scores on LV1 (", toString(oplsda@modelDF$R2X[1] * 100), "%)", sep = ""),
             y = paste("scores on LV2 (", toString(oplsda@modelDF$R2X[2] * 100), "%)", sep = "")) +
        scale_shape_manual(breaks = levels(y), values = c(shape1, shape2)) +
        scale_fill_manual(breaks = levels(y), values = c(color1, color2)) +
        theme(panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              legend.key = element_blank(),
              legend.position = "none") +
        geom_vline(xintercept = 0, size = 0.3) +
        geom_hline(yintercept = 0, size = 0.3)

    if (saveFlag) {
        pdf(paste(fileStr, "scorePlot.pdf", sep = "_"), width = 4, height = 4)
        print(pltScores)
        dev.off()
    }
    print(pltScores)

    # Loading Plot
    loadingsLV1 <- getLoadingMN(oplsda)
    loadingsTmp <- getLoadingMN(oplsda, orthoL = TRUE)
    dfLoadings <- data.frame(LV1 = loadingsLV1,
                             LV2 = loadingsTmp[, 1])
    colnames(dfLoadings) <- c("LV1", "LV2")

    if (dim(feature_annot)[1] == 0) {
        feature_annot <- data.frame(useColor = rep("black", dim(dfLoadings)[1]),
                                    label = rownames(dfLoadings))
        rownames(feature_annot) <- rownames(dfLoadings)
    }

    arrow.df = data.frame(x1 = rep(0, length(dfLoadings[, 1])),
                          y1 = rep(0, length(dfLoadings[, 1])),
                          x2 = dfLoadings[, 1],
                          y2 = dfLoadings[, 2])

    pltLoadings <- ggplot(dfLoadings, aes(LV1, LV2, label = rownames(dfLoadings))) +
        geom_point(size = 0.5, color = feature_annot$useColor[match(rownames(dfLoadings),
                                                                    rownames(feature_annot))]) +
        theme(aspect.ratio = 1) +
        labs(x = "loadings on LV1", y = "loadings on LV2") +
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                     alpha = alpha_loading,
                     data = arrow.df,
                     color = feature_annot$useColor[match(rownames(dfLoadings), rownames(feature_annot))]) +
        geom_text_repel(size = fontsize,
                        aes(label = feature_annot$label[match(rownames(dfLoadings), rownames(feature_annot))]),
                        color = feature_annot$useColor[match(rownames(dfLoadings), rownames(feature_annot))]) +
        theme(panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              legend.title = element_blank()) +
        geom_vline(xintercept = 0, size = 0.3) +
        geom_hline(yintercept = 0, size = 0.3)

    if (saveFlag) {
        pdf(paste(fileStr, "loadingsPlot.pdf", sep = "_"), width = 4, height = 4)
        print(pltLoadings)
        dev.off()
    }
    print(pltLoadings)

}
