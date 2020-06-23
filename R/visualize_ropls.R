#' Title
#'
#' @param model
#' @param y
#' @param options
#'
#' @return
#' @export
#'
#' @examples
visualize_ropls_scores <- function(model, y, options = list()) {

  # ----------------- OPTIONS ----------------- #
  if (!("alpha" %in% names(options))) {
    options$alpha <- 1
  }
  if (!("size" %in% names(options))) {
    options$size <- 2.5
  }
  if (!("stroke" %in% names(options))) {
    options$stroke <- 0.5
  }
  # confidence levels for ellipses
  if (!("level" %in% names(options))) {
    options$level <- 0.95
  }
  # color for the scores and name of the grouping
  if (!("color" %in% names(options))) {
    options$colors <- list(class = c())
    for (ind in 1:nlevels(y)) {
      options$colors$class[[levels(y)[ind]]] <- RColorBrewer::brewer.pal(n = max(3, nlevels(y)),
                                                                         name = 'Dark2')[ind]
    }
    y_name <- "class"
  } else {
    y_name <- names(options$colors)[grep(levels(y)[1], options$colors)]
  }
  # which latent variables to check, defaults to the first two
  # if they are provided, ensure that the model has the required number of LVs
  if (!("LV_ind" %in% names(options))) {
    options$LV_ind <- c(1,2)
  } else if (ropls::getSummaryDF(model)$pre +
             ropls::getSummaryDF(model)$ort < max(options$LV_ind)) {
      stop("required LV exceed existing LVs")
  }
  # ----------------- END OPTIONS ----------------- #

  # ----------------- GET SCORES ----------------- #
  # check first whether its a orthogonal PLS or a regular PLS
  if (ropls::getSummaryDF(model)$ort > 0) {
    if (options$LV_ind[1] == 1) {
      df_scores <- data.frame(LV1 = ropls::getScoreMN(model),
                              LV2 = ropls::getScoreMN(model, orthoL = TRUE)[, options$LV_ind[2] - 1],
                              y = y)
    } else {
      df_scores <- data.frame(LV1 = ropls::getScoreMN(model, orthoL = TRUE)[, options$LV_ind[1] - 1],
                              LV2 = ropls::getScoreMN(model, orthoL = TRUE)[, options$LV_ind[2] - 1],
                              y = y)
    }
  } else {
    df_scores <- data.frame(LV1 = ropls::getScoreMN(model)[,options$LV_ind[1]],
                            LV2 = ropls::getScoreMN(model)[,options$LV_ind[2]],
                            y = y)
  }

  # --------------------- END GET SCORES ------------------- #


  # ---------------------- BEGIN PLOT ---------------------- #
  plt_scores <- ggplot2::ggplot(df_scores,ggplot2::aes(LV1, LV2, fill = y)) +
    ggplot2::geom_vline(xintercept = 0, size = 0.3) +
    ggplot2::geom_hline(yintercept = 0, size = 0.3) +
    ggplot2::geom_point(color = "black",
                        size = options$size,
                        alpha = options$alpha,
                        stroke = options$stroke,
                        shape = 21,
                        show.legend = TRUE) +
    ggplot2::labs(x = paste("scores on LV", options$LV_ind[1], "(",
                            toString(model@modelDF$R2X[options$LV_ind[1]] * 100), "%)", sep = ""),
                  y = paste("scores on LV", options$LV_ind[2], "(",
                            toString(model@modelDF$R2X[options$LV_ind[2]] * 100), "%)", sep = ""),
                  fill = y_name, color = y_name) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "right",
                   aspect.ratio = 1,
                   axis.text = ggplot2::element_text(color = "black"))

  # in the case of a classification, add ellipses
  if (is.factor(y)) {
    plt_scores <- plt_scores +
      ggplot2::stat_ellipse(ggplot2::aes(color = y), level = options$level) +
      ggplot2::scale_fill_manual(values = options$colors[[y_name]]) +
      ggplot2::scale_color_manual(values = options$colors[[y_name]])
  }
  # ---------------------- END PLOT ---------------------- #

  return(plt_scores)
}


#' Title
#'
#' @param model
#' @param y
#' @param options
#'
#' @return
#' @export
#'
#' @examples
visualize_ropls_loadings <- function(model, options = list()) {

  # ----------------- BEGIN OPTIONS I ----------------- #
  if (!("loading_alpha" %in% names(options))) {
    options$loading_alpha <- 0.8
  }
  if (!("LV_ind" %in% names(options))) {
    options$LV_ind <- c(1,2)
  } else if (ropls::getSummaryDF(model)$pre +
             ropls::getSummaryDF(model)$ort < max(options$LV_ind)) {
    stop("required LV exceed existing LVs")
  }
  # ----------------- END OPTIONS I ----------------- #


  # ----------------- BEGIN GET LOADINGS  ----------------- #

  # check first whether its a orthogonal PLS or a regular PLS
  if (ropls::getSummaryDF(model)$ort > 0) {
    if (options$LV_ind[1] == 1) {
      df_loadings <- data.frame(LV1 = ropls::getLoadingMN(model),
                                LV2 = ropls::getLoadingMN(model, orthoL = TRUE)[,options$LV_ind[2] - 1])
    } else {
      df_loadings <- data.frame(LV1 = ropls::getLoadingMN(model, orthoL = TRUE)[, options$LV_ind[1] - 1],
                              LV2 = ropls::getLoadingMN(model, orthoL = TRUE)[, options$LV_ind[2] - 1])
    }
  } else {
    df_loadings <- data.frame(LV1 = ropls::getLoadingMN(model)[,options$LV_ind[1]],
                              LV2 = ropls::getLoadingMN(model)[,options$LV_ind[2]])
  }

  # ----------------- END GET LOADINGS  ----------------- #



  # ----------------- BEGIN OPTIONS II----------------- #

  if (!("df_features" %in% names(options))) {
    options$df_features <- data.frame(name = rownames(df_loadings),
                                      label = rownames(df_loadings))
  }

  if (!("color_features" %in% names(options))) {
    df_loadings[[options$color_features]] <-
      options$df_features[match(rownames(df_loadings), options$df_features$name),
                          options$color_features]
  }
  # ----------------- END OPTIONS II ----------------- #



  # ----------------- BEGIN VISUALIZATION ----------------- #

  df_loadings$label <- options$df_features$label[match(rownames(df_loadings),
                                                       options$df_features$name)]

  plt_loadings <-  ggplot2::ggplot(df_loadings, ggplot2::aes(LV1, LV2)) +
    ggplot2::geom_vline(xintercept = 0, size = 0.3) +
    ggplot2::geom_hline(yintercept = 0, size = 0.3) +
    ggplot2::labs(x = "loadings on LV1",
                  y = "loadings on LV2") +
    ggplot2::theme_classic() +
    ggplot2::theme(aspect.ratio = 1,
          axis.text =  ggplot2::element_text(color = "black"))

  if (!("color_features" %in% names(options))) {
    plt_loadings <- plt_loadings +
      ggplot2::geom_point(size = 0.5,
                          ggplot2::aes_string(color = options$color_features)) +
      ggplot2::geom_segment(y = 0, x = 0,
                            ggplot2::aes_string(xend = "LV1", yend = "LV2",
                            color = options$color_features),
                 alpha = options$loading_alpha) +
      ggrepel::geom_text_repel(size = 3,
                               ggplot2::aes_string(color = options$color_features,
                                                   label = "label")) +
      ggplot2::scale_color_manual(values = options$colors[[options$color_features]])
  } else {
    plt_loadings <-  plt_loadings +
      ggplot2::geom_point(size = 0.5) +
      ggplot2::geom_segment(y = 0, x = 0,
                            ggplot2::aes(xend = LV1, yend = LV2),
                   alpha = options$loading_alpha) +
      ggrepel::geom_text_repel(size = 3,  ggplot2::aes(label = label))
  }

  # ----------------- END VISUALIZATION ----------------- #

  return(plt_loadings)
}


#' Title
#'
#' @param model
#' @param ind_LV
#'
#' @return
#' @export
#'
#' @examples
visualize_ropls_loadings_bar <- function(model, y, ind_LV, options = list()) {

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
  if (!("useColor" %in% colnames(feature_annot))) {
    feature_annot$useColor <- rep("black", dim(feature_annot)[1])
  }

  if (ropls::getSummaryDF(model)$ort == 0) {
    loading <- ropls::getLoadingMN(oplsda)


  } else {
    loadingsLV1_tmp <- ropls::getLoadingMN(model)[,1]
    loadingsLV1 <- loadingsLV1_tmp[, 1]
    plsda <- oplsda
  }

  # perform a standard PLS-DA (not orthogonal) and use these VIP scores
  # the loadings values are still used from the priginal PLS-DA
  # (oplsda) above because they discriminante between classes for </> 0
  df_bar <- data.frame(vip_scores = getVipVn(plsda),
                       loadingsLV1 = loadingsLV1,
                       features = colnames(X),
                       feature_labels = feature_annot$label[match(colnames(X),rownames(feature_annot))])

  colnames(dfBar) <- c("vipScores", "loadingsLV1", "features", "feature_labels")

  warning("Please, check whether coloring and assignment is right!")

  if (markEnrich & nClasses == 2 & type == "classification") {
    for (indFeat in 1:dim(dfBar)[1]) {
      if (median(X[which(y == levels(y)[2]), which(colnames(X) == dfBar$features[indFeat])]) >
          median(X[which(y == levels(y)[1]), which(colnames(X) == dfBar$features[indFeat])])) {
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
    ylab("LV1 loadings") + labs(fill = "enriched in") +
    scale_x_discrete(labels = dfBar$feature_labels) +
    scale_fill_manual(values = colors_bars) +
    theme(legend.position = "none", axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(colour = as.character(
            feature_annot$useColor[match(dfBar$features[order(dfBar$vipScores)],
                                         rownames(feature_annot))])))



}

