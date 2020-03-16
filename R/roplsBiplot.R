#' Visualization for PLS-R/PLS-DA results, namely, score and loadings plot
#'
#' @param ropls_obj ropls (O)PLS-DA object
#' @param y label/values
#' @param type "classification" or "regression"
#' @param feature_annot data frame with annotations for the features (colors,...)
#' @param saveFlag whether to save the figures
#' @param fileStr string where figures should be saved
#' @param alpha_loading value for plotting the loadings vectors
#' @param colors_y color for the classes
#' @param fontsize fontzise used for the plots
#' @param orth flag whether orthogonalized version of PLS is used
#' @return Plotting objects for score and loadings plot
#' @export

roplsBiplot <- function(ropls_obj,
                      y,
                      type = "classification",
                      feature_annot = data.frame(),
                      saveFlag = FALSE,
                      fileStr = "",
                      alpha_loading = 1,
                      colors_y = NA,
                      fontsize = 4,
                      orth = FALSE) {

    if (type == "classification") {
        nClasses <- length(levels(y))
        if (is.na(colors_y[[1]])) {
            library(RColorBrewer)
            colors_y <- brewer.pal(8, "Dark2")
            names(colors_y) <- levels(y)
        }
    }

    if (!is.na(getScoreMN(ropls_obj, orth = TRUE)[1])) { # orthogonal PLS-DA/R
        scoresLV1 <- getScoreMN(ropls_obj)
        scoresTmp <- getScoreMN(ropls_obj, orthoL = TRUE)
        dfScores <- data.frame(LV1 = scoresLV1,
                               LV2 = scoresTmp[, 1],
                               y = y)
    } else {
        scoresLV1_2 <- getScoreMN(ropls_obj)
        dfScores <- data.frame(LV1 = scoresLV1_2[ ,1],
                               LV2 = scoresLV1_2[, 2],
                               y = y)
    }
    colnames(dfScores) <- c("LV1", "LV2", "y")

    pltScores <- ggplot(dfScores, aes(LV1, LV2, fill = y)) +
        geom_vline(xintercept = 0, size = 0.3) +
        geom_hline(yintercept = 0, size = 0.3) +
        geom_point(color = "black", size = 3,
                   stroke = 0.5, shape = 21, show.legend = TRUE) +
        labs(x = paste("scores on LV1 (", toString(ropls_obj@modelDF$R2X[1] * 100), "%)", sep = ""),
             y = paste("scores on LV2 (", toString(ropls_obj@modelDF$R2X[2] * 100), "%)", sep = "")) +
        theme_classic() +
        theme(legend.position = "right", aspect.ratio = 1)
    if (type == "classification") {
        pltScores <- pltScores +
            scale_fill_manual(values = colors_y)

    }

    if (saveFlag) {
        pdf(paste(fileStr, "scorePlot.pdf", sep = "_"), width = 4, height = 4)
        print(pltScores)
        dev.off()
    }
    print(pltScores)

    # Loading Plot
    if (!is.na(getScoreMN(ropls_obj, orthoL = TRUE)[1])) { # orthogonal PLS-DA/R
        loadingsLV1 <- getLoadingMN(ropls_obj)
        loadingsTmp <- getLoadingMN(ropls_obj, orthoL = TRUE)
        dfLoadings <- data.frame(LV1 = loadingsLV1,
                                 LV2 = loadingsTmp[, 1])
    } else {
        loadings <- getLoadingMN(ropls_obj)
        dfLoadings <- data.frame(LV1 = loadings[, 1],
                                 LV2 = loadings[, 2])
        rownames(dfLoadings) <- rownames(loadings)
    }
    colnames(dfLoadings) <- c("LV1", "LV2")

    if (dim(feature_annot)[1] == 0) {
        feature_annot <- data.frame(useColor = rep("black", dim(dfLoadings)[1]),
                                    label = rownames(dfLoadings))
        rownames(feature_annot) <- rownames(dfLoadings)
    }
    if (length(which(colnames(feature_annot) == "useColor")) == 0) {
        feature_annot$useColor <- rep("black", dim(dfLoadings)[1])
    }

    arrow.df = data.frame(x1 = rep(0, length(dfLoadings[, 1])),
                          y1 = rep(0, length(dfLoadings[, 1])),
                          x2 = dfLoadings[, 1],
                          y2 = dfLoadings[, 2])

    pltLoadings <- ggplot(dfLoadings, aes(LV1, LV2, label = rownames(dfLoadings))) +
        geom_vline(xintercept = 0, size = 0.3) +
        geom_hline(yintercept = 0, size = 0.3) +
        geom_point(size = 0.5, color = feature_annot$useColor[match(rownames(dfLoadings),
                                                                    rownames(feature_annot))]) +
        labs(x = "loadings on LV1", y = "loadings on LV2") +
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                     alpha = alpha_loading,
                     data = arrow.df,
                     color = feature_annot$useColor[match(rownames(dfLoadings), rownames(feature_annot))]) +
        geom_text_repel(size = fontsize,
                        aes(label = feature_annot$label[match(rownames(dfLoadings), rownames(feature_annot))]),
                        color = feature_annot$useColor[match(rownames(dfLoadings), rownames(feature_annot))]) +
        theme_classic() +
        theme(aspect.ratio = 1)

    if (saveFlag) {
        pdf(paste(fileStr, "loadingsPlot.pdf", sep = "_"), width = 4, height = 4)
        print(pltLoadings)
        dev.off()
    }
    print(pltLoadings)

    output = list(pltScores = pltScores, pltLoadings = pltLoadings)

}
