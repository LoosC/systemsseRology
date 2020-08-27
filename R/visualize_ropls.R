#' Score plot

#' @param model ropls object
#' @param y vector of labels
#' @param options
#'
#' @return
#' @export
#'
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
  if ("y" %in% names(options)) {
    y <- options$y
    if (is.factor(y)) {
      n_groups <- nlevels(y)
    } else {
      n_groups <- NA
    }
  } else {
    n_groups <- NA
  }
  if (!("y_name" %in% names(options))) {
    y_name <- "y"
  } else {
    y_name <- options$y_name
  }
  # color for the scores and name of the grouping
  if (!("colors" %in% names(options)) | length(grep(y_name, names(options$colors))) == 0) {
    if (is.factor(y)) {
        tmp <- rep(NA, length = nlevels(y))
        names(tmp) <- levels(y)
        for (ind in 1:nlevels(y)) {
          tmp[ind] <- RColorBrewer::brewer.pal(n = max(3, nlevels(y)), name = 'Dark2')[ind]
        }
        options$colors <- list()
        options$colors[[y_name]] <- tmp
    } else {
      # For regression, a color palette needs to be provided
      options$colors$y <- list(low = "#C7E4F9", high = "#004D7F")
    }
  }


  # which latent variables to check, defaults to the first two
  # if they are provided, ensure that the model has the required number of LVs
  if (!("LV_ind" %in% names(options))) {
    options$LV_ind <- c(1,2)
  } else if (ropls::getSummaryDF(model)$pre +
             ropls::getSummaryDF(model)$ort < max(options$LV_ind)) {
      stop("required LV exceed existing LVs")
  } else if (!(length(options$LV_ind) == 2)) {
    stop("two LVs required")
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
  plt_scores <- ggplot2::ggplot(df_scores, ggplot2::aes(LV1, LV2, fill = y)) +
    ggplot2::geom_vline(xintercept = 0, size = 0.3) +
    ggplot2::geom_hline(yintercept = 0, size = 0.3) +
    ggplot2::geom_point(color = "black",
                        size = options$size,
                        alpha = options$alpha,
                        stroke = options$stroke,
                        shape = 21,
                        show.legend = TRUE) +
    ggplot2::labs(x = paste("scores on LV", options$LV_ind[1], " (",
                            toString(model@modelDF$R2X[options$LV_ind[1]] * 100), "%)", sep = ""),
                  y = paste("scores on LV", options$LV_ind[2], " (",
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
  } else {
    plt_scores <- plt_scores +
      ggplot2::scale_fill_gradient(low = options$colors$y[["low"]], high = options$colors$y[["high"]])
  }
  # ---------------------- END PLOT ---------------------- #

  return(plt_scores)
}


#' Loadings plot
#'
#' @param model ropls object
#' @param y vector with labels
#' @param options
#'
#' @return
#' @export
#'
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
  } else if (!(length(options$LV_ind) == 2)) {
    stop("two LVs required")
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

  if ("color_features" %in% names(options)) {
    if (!(options$color_features %in% colnames(options$df_features))) {
      stop(paste(options$color_features, "is not defined in df_features"))
    }
    if (!("colors" %in% names(options))) {
       options$colors[[options$color_features]] <- colorRampPalette(RColorBrewer::brewer.pal(name = "Dark2",
                                                   n = 8))(nlevels(options$df_features[,options$color_features]))
    } else if (!(options$color_feature %in% names(options$colors))) {
      options$colors[[options$color_features]] <- colorRampPalette(RColorBrewer::brewer.pal(name = "Dark2",
                                                   n = 8))(nlevels(options$df_features[,options$color_features]))
    }
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

  if ("color_features" %in% names(options)) {
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


#' Barplot for loadings
#'
#' @param model ropls object
#' @param ind_LV latent variable index
#'
#' @return
#' @export
#'
visualize_ropls_loadings_bar <- function(model, options = list()) {

  # ----------------- BEGIN OPTIONS ----------------- #
  if (!("LV_ind" %in% names(options))) {
    options$LV_ind <- c(1)
  }
  if ("y" %in% names(options)) {
    y <- options$y
    if (is.factor(y)) {
      n_groups <- nlevels(y)
    } else {
      n_groups <- NA
    }
  } else {
    n_groups <- NA
  }
  if (!("mark_enrichment" %in% names(options)) | is.na(n_groups)) {
    options$mark_enrichment <- FALSE
  }
  if (options$mark_enrichment & (is.na(n_groups) | !("X" %in% names(options))))  {
    stop("Enrichment only works for classification and when X and y are provided")
  }

  # color for the scores and name of the grouping
  if (!("y_name" %in% names(options))) {
    y_name <- "y"
  } else {
    y_name <- options$y_name
  }
  # color for the scores and name of the grouping
  if (!("colors" %in% names(options)) | length(grep(y_name, names(options$colors))) == 0) {
    if (is.factor(y)) {
      tmp <- rep(NA, length = nlevels(y))
      names(tmp) <- levels(y)
      for (ind in 1:nlevels(y)) {
        tmp[ind] <- RColorBrewer::brewer.pal(n = max(3, nlevels(y)), name = 'Dark2')[ind]
      }
      options$colors <- list()
      options$colors[[y_name]] <- tmp
    } else {
      # For regression, a color palette needs to be provided
      options$colors$y <- list(low = "#C7E4F9", high = "#004D7F")
    }
  }

  if (ropls::getSummaryDF(model)$pre +
      ropls::getSummaryDF(model)$ort < options$LV_ind) {
    stop("required LV exceed existing LVs")
  }

  if (!("df_features" %in% names(options))) {
    options$df_features <- data.frame(name = rownames(model@loadingMN),
                                      label = rownames(model@loadingMN))
  }

  # ----------------- END OPTIONS ----------------- #

  # check first whether its a orthogonal PLS or a regular PLS
  if (ropls::getSummaryDF(model)$ort > 0) {
    stop("orthogonal PLS-DA not supported yet")
    # if (options$LV_ind[1] == 1) {
    #   df_loadings <- data.frame(LV1 = ropls::getLoadingMN(model),
    #                             LV2 = ropls::getLoadingMN(model, orthoL = TRUE)[,options$LV_ind[2] - 1])
    # } else {
    #   df_loadings <- data.frame(LV1 = ropls::getLoadingMN(model, orthoL = TRUE)[, options$LV_ind[1] - 1],
    #                             LV2 = ropls::getLoadingMN(model, orthoL = TRUE)[, options$LV_ind[2] - 1])
    # }
  } else {
    df_loadings <- data.frame(LV = ropls::getLoadingMN(model)[,options$LV_ind[1]],
                              vip_scores = ropls::getVipVn(model))
    df_loadings$features <- rownames(df_loadings)
    df_loadings$labels <- options$df_features$label[match(rownames(df_loadings), options$df_features$name)]
  }



  # TODO: catch if its an orthogonal

  if (options$mark_enrichment & !is.na(n_groups)) {
    df_loadings$mark <- NA
    X <- options$X

    for (ind_feat in 1:nrow(df_loadings)) {
      tmp_mean <- rep(NA, length = nlevels(y))
      for (ind_class in 1:nlevels(y)) {
        tmp_mean[ind_class] <- mean(X[which(y == levels(y)[ind_class]),
                                      which(colnames(X) == df_loadings$features[ind_feat])])
      }
      df_loadings$mark[ind_feat] <- levels(y)[which.max(tmp_mean)]
    }
    df_loadings$mark  <- factor(df_loadings$mark, levels = levels(y))
  }

  df_loadings <- df_loadings[order(df_loadings$vip_scores), ]
  df_loadings$features <- factor(df_loadings$features, levels = unique(df_loadings$features))

  # plot loadings sorted according to the VIP score and color coding it
  # according to enrichent in classes
  if (options$mark_enrichment) {
    plt_bar <- ggplot2::ggplot(data = df_loadings, ggplot2::aes(x = features, y = LV, fill = mark)) +
      ggplot2::scale_fill_manual(values = options$colors[[y_name]])
  } else {
    plt_bar <- ggplot2::ggplot(data = df_loadings, ggplot2::aes(x = features, y = LV))
  }
  plt_bar <- plt_bar +
    ggplot2::geom_bar(stat = "identity", color = "black") +
    ggplot2::coord_flip() +
    ggplot2::xlab("") +
    ggplot2::ylab(paste("LV", options$LV_ind, " loadings", sep = "")) +
    ggplot2::labs(fill = "enriched in") +
    ggplot2::scale_x_discrete(labels = df_loadings$labels) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black"))#,
                   #axis.text.y = element_text(colour = as.character(feature_annot$useColor[match(dfBar$features[order(dfBar$vipScores)],
                                        # rownames(feature_annot))])))

}

