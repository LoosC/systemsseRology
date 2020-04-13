#' Visualization for PLS-R/PLS-DA results, namely, barplots for VIP scores and loadings of LV1
#'
#' @param oplsda PLS object
#' @param y label/values
#' @param X data matrix (of selected features)
#' @param type "classification" or "regression"
#' @param feature_annot data frame with annotations for the features (colors,...)
#' @param saveFlag whether to save the figures
#' @param fileStrPart string where figures should be saved
#' @param colors_bars color for the classes
#' @param width of saved figure
#' @param height of saved figure
#' @param vipFlag whether only VIP scores > 1 should be plotted
#' @param orth flag whether orthogonalized version of PLS is used
#' @param markEnrich flag whether bars should be colored according to enrichment in class, only works for 2 classes
#' @return
#' @export

plsBarplot <- function(oplsda,
                       y,
                       X,
                       type = "classification",
                       feature_annot = data.frame(),
                       saveFlag = FALSE,
                       fileStrPart = "",
                       colors_bars = NA,
                       width = 5,
                       height = 4,
                       vipFlag = FALSE,
                       orth = TRUE,
                       markEnrich = TRUE) {

    if (type == "classification") {
        nClasses <- length(levels(y))
        if (is.na(colors_bars[[1]])) {
            library(RColorBrewer)
            colors_bars <- brewer.pal(8, "Dark2")
            names(colors_bars) <- levels(y)
        }
    }

     if (dim(feature_annot)[1] == 0) {
        feature_annot <- data.frame(useColor = rep("black", dim(X)[2]),
                                    label = colnames(X))
        rownames(feature_annot) <- colnames(X)
    }

    if (orth) {
        loadingsLV1 <- getLoadingMN(oplsda)

        pre_symbol <- try(plsda <- opls(X, factor(y), orthoI = 0,
                                        permI = 0, info.txtC = "none", fig.pdfC = "none"))
        isError <- is(pre_symbol, "try-error")
        if (isError) {
            plsda <- opls(X, factor(y), predI = 1, orthoI = 0,
                          permI = 0, info.txtC = "none", fig.pdfC = "none")
        }
    } else {
        loadingsLV1_tmp <- getLoadingMN(oplsda)
        loadingsLV1 <- loadingsLV1_tmp[, 1]
        plsda <- oplsda
    }

    # perform a standard PLS-DA (not orthogonal) and use these VIP scores loadingsLV1
    # are still used from the orthogonal PLS-DA
    # (oplsda) above because they discriminante between classes for </> 0
    dfBar <- data.frame(vipScores = getVipVn(plsda),
                        loadingsLV1 = loadingsLV1,
                        features = colnames(X),
                        feature_labels = feature_annot$label[match(colnames(X),rownames(feature_annot))])
    colnames(dfBar) <- c("vipScores", "loadingsLV1", "features", "feature_labels")

    warning("Please, check whether coloring and assignment is right!")

    if (markEnrich & nClasses == 2 & type == "classification") {
        for (indFeat in 1:dim(dfBar)[1]) {
            if (median(X[y == levels(y)[2], dfBar$features[indFeat]]) > median(X[y == levels(y)[1], dfBar$features[indFeat]])) {
                dfBar$mark[indFeat] <- levels(y)[2]
            } else {
                dfBar$mark[indFeat] <- levels(y)[1]
            }
        }
        dfBar$mark <- factor(dfBar$mark, levels = levels(y))
    }  else {
        dfBar$mark <- rep(NA, dim(dfBar)[1])
    }

    dfBar <- dfBar[order(dfBar$vipScores), ]
    dfBar$features <- factor(dfBar$features, levels = unique(dfBar$features))

    # only VIP > 1
    if (vipFlag) {
        dfBar <- dfBar[dfBar$vipScores > 1, ]
    }
    # plot loadings sorted according to the VIP score and color coding it
    # according to enrichent in classes
    pltBar <- ggplot(data = dfBar, aes(x = features, y = loadingsLV1, fill = mark)) +
        geom_bar(stat = "identity", color = "black") +
        theme_classic() +
        coord_flip() +
        xlab("") +
        ylab("LV1 loadings") +
        scale_x_discrete(labels = dfBar$feature_labels) +
        scale_fill_manual(values = colors_bars) +
        theme(legend.position = "none",
              axis.text.y = element_text(colour = as.character(
                  feature_annot$useColor[match(dfBar$features[order(dfBar$vipScores)],
                                               rownames(feature_annot))])))

    if (saveFlag) {
        pdf(paste(fileStrPart, "barPlot.pdf", sep = "_"), width = width, height = height)
        print(pltBar)
        dev.off()
    }

    # plot VIP scores and color coding it according to enrichent in classes
    pltVip <- ggplot(data = dfBar, aes(x = features, y = vipScores, fill = mark)) +
        geom_bar(stat = "identity", color = "black") +
        theme_classic() +
        coord_flip() +
        xlab("") +
        ylab("VIP scores") +
        scale_x_discrete(labels = dfBar$feature_labels) +
        scale_fill_manual(values = colors_bars) +
        theme(legend.position = "none",
              axis.text.y = element_text(colour = as.character(
                  feature_annot$useColor[match(dfBar$features[order(dfBar$vipScores)],
                                               rownames(feature_annot))])))

    if (saveFlag) {
        pdf(paste(fileStrPart, "vipPlot.pdf", sep = "_"), width = width, height = height)
        print(pltVip)
        dev.off()
    }

    output <- list(pltBar = pltBar, pltVip = pltVip, dfBar = dfBar)
    return(output)

}
