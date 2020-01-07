#' @export

pcaBiplot <- function(pca, y, indX = 1, indY = 2, saveFlag = FALSE, fileStr = "", color1 = "red", color2 = "blue",
                      shape1 = 21, shape2 = 21, fontsize = 4,
                      alpha_loading = 1, feature_annot = data.frame()) {

  tmp <- summary(pca)
  dfPCA <- as.data.frame(pca$x)
  dfPCA <- dfPCA[, c(indX, indY)]
  colnames(dfPCA) <- c("PC_X", "PC_Y")
  dfPCA$class <- y
  pltScores <- ggplot(dfPCA, aes(x = PC_X, y = PC_Y, color = class, shape = class)) +
    geom_point(aes(fill = class), color = "black", size = 3,
               stroke = 0.6) +
    theme(aspect.ratio = 1) +
    scale_shape_manual(breaks = levels(y), values = c(shape1, shape2)) +
    scale_fill_manual(breaks = levels(y), values = c(color1, color2)) +
    labs(x = paste("PC", toString(indX), " (",
                   toString(round(tmp$importance["Proportion of Variance", indX]*100, digits = 2)), "%)", sep = ""),
         y = paste("PC", toString(indY), " (",
                   toString(round(tmp$importance["Proportion of Variance", indY]*100, digits = 2)) ,"%)", sep = ""))  +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          legend.key = element_blank(),
          legend.position = "none")

  if (saveFlag) {
    pdf(paste(fileStr, "scorePlot_PCA.pdf", sep = "_"), width = 4, height = 4)
    print(pltScores)
    dev.off()
  }
  print(pltScores)

  # Loading Plot
  dfLoadings <- data.frame(PC_X = pca$rotation[, indX],
                           PC_Y = pca$rotation[, indY])
  colnames(dfLoadings) <- c("PC_X", "PC_Y")

  if (dim(feature_annot)[1] == 0) {
    feature_annot <- data.frame(useColor = rep("black", dim(dfLoadings)[1]),
                                label = rownames(dfLoadings))
    rownames(feature_annot) <- rownames(dfLoadings)
  }

  arrow.df = data.frame(x1 = rep(0, length(dfLoadings[, 1])),
                        y1 = rep(0, length(dfLoadings[, 1])),
                        x2 = dfLoadings[, indX],
                        y2 = dfLoadings[, indY])

  pltLoadings <- ggplot(dfLoadings, aes(PC_X, PC_Y, label = rownames(dfLoadings))) +
    geom_point(size = 0.5, color = feature_annot$useColor[match(rownames(dfLoadings),
                                                                rownames(feature_annot))]) +
    theme(aspect.ratio = 1) +
    labs(x = paste("loadings on PC", toString(indX), sep = ""),
         y = paste("loadings on PC", toString(indY), sep = ""))  +
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
    pdf(paste(fileStr, "loadingsPlot_PCA.pdf", sep = "_"), width = 4, height = 4)
    print(pltLoadings)
    dev.off()
  }
  print(pltLoadings)

}
