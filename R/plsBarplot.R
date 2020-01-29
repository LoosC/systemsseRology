#' @export

plsBarplot <- function(oplsda, y, X_sel, saveFlag = FALSE, fileStr = "",
                       color1 = "red", color2 = "blue", feature_annot = data.frame(),
                       width = 5, height = 4, vipFlag = FALSE) {

    if (dim(feature_annot)[1] == 0) {
        feature_annot <- data.frame(useColor = rep("black", dim(X_sel)[2]),
                                    label = colnames(X_sel))
        rownames(feature_annot) <- colnames(X_sel)
    }

    loadingsLV1 <- getLoadingMN(oplsda)

    pre_symbol <- try(plsda <- opls(X_sel, factor(y), orthoI = 0,
                                    permI = 0, info.txtC = "none", fig.pdfC = "none"))
    isError <- is(pre_symbol, "try-error")
    if (isError) {
        plsda <- opls(X_sel, factor(y), predI = 1, orthoI = 0,
                      permI = 0, info.txtC = "none", fig.pdfC = "none")
    }

    # perform a standard PLS-DA (not orthogonal) and use these VIP scores loadingsLV1
    # are still used from the orthogonal PLS-DA
    # (oplsda) above because they discriminante between classes for </> 0
    dfBar <- data.frame(vipScores = getVipVn(plsda),
                        loadingsLV1 = loadingsLV1,
                        features = colnames(X_sel))
    colnames(dfBar) <- c("vipScores", "loadingsLV1", "features")


    # check here whether the order is right!
    dfBar$mark[dfBar$loadingsLV1 < 0] = "<0"
    dfBar$mark[dfBar$loadingsLV1 > 0] = ">0"
    dfBar$mark <- factor(dfBar$mark, levels = c("<0", ">0"))

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
        scale_fill_manual(values = c("<0" = color1,
                                     ">0" = color2), breaks = c("<0", ">0")) +
        theme_minimal() +
        coord_flip() +
        xlab("") +
        ylab("LV1 loadings") +
        theme(legend.position = "none",
              axis.text.y = element_text(colour = as.character(
                  feature_annot$useColor[match(dfBar$features[order(dfBar$vipScores)],
                                               rownames(feature_annot))])))

    if (saveFlag) {
        pdf(paste(fileStr, "barPlot.pdf", sep = "_"), width = width, height = height)
        print(pltBar)
        dev.off()
    }
    print(pltBar)

    # plot VIP scores and color coding it according to enrichent in classes
    pltVip <- ggplot(data = dfBar, aes(x = features, y = vipScores, fill = mark)) +
        geom_bar(stat = "identity", color = "black") +
        scale_fill_manual(values = c("<0" = color1,
                                     ">0" = color2), breaks = c("<0", ">0")) +
        theme_minimal() + coord_flip() + xlab("") +
        ylab("VIP scores") +
        theme(legend.position = "none",
              axis.text.y = element_text(colour = as.character(
                  feature_annot$useColor[match(dfBar$features[order(dfBar$vipScores)],
                                               rownames(feature_annot))])))
    if (saveFlag) {
        pdf(paste(fileStr, "vipPlot.pdf", sep = "_"), width = width, height = height)
        print(pltVip)
        dev.off()
    }
    print(pltVip)

    output <- list(pltBar = pltBar, pltVip = pltVip, dfBar = dfBar)
    return(output)

}
