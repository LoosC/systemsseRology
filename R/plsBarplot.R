#' Visualization for PLS-R/PLS-DA results, namely, barplots for VIP scores and loadings of LV1
#'
#' @param oplsda PLS object
#' @param y label/values
#' @param X data matrix (of selected features)
#' @param type "classification" or "regression"
#' @param feature_annot data frame with annotations for the features (colors,...)
#' @param saveFlag whether to save the figures
#' @param fileStr string where figures should be saved
#' @param colors_y color for the classes
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
                       fileStr = "",
                       colors_y = "NA",
                       width = 5,
                       height = 4,
                       vipFlag = FALSE,
                       orth = TRUE,
                       markEnrich = TRUE) {


    if (type == "classification") {
        nClasses <- length(levels(y))
        if (is.na(colors_y[[1]])) {
            library(RColorBrewer)
            colors_y <- brewer.pal(8, "Dark2")
            names(colors_y) <- levels(y)
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
                        features = colnames(X))
    colnames(dfBar) <- c("vipScores", "loadingsLV1", "features")


    if (markEnrich & nClasses == 2 & type == "classification") {
        # check here whether the order is right!
        dfBar$mark[dfBar$loadingsLV1 < 0] = "<0"
        dfBar$mark[dfBar$loadingsLV1 > 0] = ">0"
        dfBar$mark <- factor(dfBar$mark, levels = c("<0", ">0"))
    }  else {
        dfBar$mark <- feature_annot$antigen[match(dfBar$features[order(dfBar$vipScores)],
                                                   rownames(feature_annot))]
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
        theme_minimal() +
        coord_flip() +
        xlab("") +
        ylab("LV1 loadings") +
        theme(legend.position = "none",
              axis.text.y = element_text(colour = as.character(
                  feature_annot$useColor[match(dfBar$features[order(dfBar$vipScores)],
                                               rownames(feature_annot))])))
    if (markEnrich & nClasses == 2 & type == "classification") {
        pltBar <- pltBar + scale_fill_manual(values = c("<0" = colors_y[[1]],
                                                        ">0" = colors_y[[2]]),
                                             breaks = c("<0", ">0"))
    } else {
        pltBar <- pltBar + scale_fill_manual(values = colors_y,
                                             breaks = levels(color_y))
    }

    if (saveFlag) {
        pdf(paste(fileStr, "barPlot.pdf", sep = "_"), width = width, height = height)
        print(pltBar)
        dev.off()
    }
    print(pltBar)

    # plot VIP scores and color coding it according to enrichent in classes
    pltVip <- ggplot(data = dfBar, aes(x = features, y = vipScores, fill = mark)) +
        geom_bar(stat = "identity", color = "black") +
        theme_minimal() +
        coord_flip() +
        xlab("") +
        ylab("VIP scores") +
        theme(legend.position = "none",
              axis.text.y = element_text(colour = as.character(
                  feature_annot$useColor[match(dfBar$features[order(dfBar$vipScores)],
                                               rownames(feature_annot))])))
    if (markEnrich & nClasses == 2 & type == "classification") {
        pltBar <- pltBar + scale_fill_manual(values = c("<0" = colors_y[[1]],
                                                        ">0" = colors_y[[2]]),
                                             breaks = c("<0", ">0"))
    }

    if (saveFlag) {
        pdf(paste(fileStr, "vipPlot.pdf", sep = "_"), width = width, height = height)
        print(pltVip)
        dev.off()
    }
    print(pltVip)

    output <- list(pltBar = pltBar, pltVip = pltVip, dfBar = dfBar)
    return(output)

}
