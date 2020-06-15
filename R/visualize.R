#' Title
#'
#' @param ropls_obj
#' @param y
#' @param options
#'
#' @return
#' @export
#'
#' @examples
visualize_ropls_scores <- function(ropls_obj, y, options = list()) {

  y_name <- names(options$colors)[grep(levels(y)[1], options$colors)]

  # check first whether its a orthogonal PLS or a regular PLS
  if (!is.na(ropls::getScoreMN(ropls_obj, orth = TRUE)[1])) {
    df_scores <- data.frame(LV1 = ropls::getScoreMN(ropls_obj),
                            LV2 = ropls::getScoreMN(ropls_obj, orthoL = TRUE)[,1],
                            y = y)
  } else {
    df_scores <- data.frame(LV1 = ropls::getScoreMN(ropls_obj)[,1],
                            LV2 = ropls::getScoreMN(ropls_obj)[,2],
                            y = y)
  }

  plt_scores <- ggplot2::ggplot(df_scores,  ggplot2::aes(LV1, LV2, fill = y)) +
    ggplot2::geom_vline(xintercept = 0, size = 0.3) +
    ggplot2::geom_hline(yintercept = 0, size = 0.3) +
    ggplot2::geom_point(color = "black",
                        size = 2.5,
                        alpha = 1,
                        stroke = 0.5,
                        shape = 21,
                        show.legend = TRUE) +
    ggplot2::labs(x = paste("scores on LV1 (", toString(ropls_obj@modelDF$R2X[1] * 100), "%)", sep = ""),
                  y = paste("scores on LV2 (", toString(ropls_obj@modelDF$R2X[2] * 100), "%)", sep = ""),
                  fill = y_name, color = y_name) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "right",
                   aspect.ratio = 1,
                   axis.text = ggplot2::element_text(color = "black"))

  if (is.factor(y)) {
    plt_scores <- plt_scores +
      ggplot2::stat_ellipse(ggplot2::aes(color = y), level = .95) +
      ggplot2::scale_fill_manual(values = options$colors[[y_name]]) +
      ggplot2::scale_color_manual(values = options$colors[[y_name]])
  }

  return(plt_scores)
}


#' Title
#'
#' @param ropls_obj
#' @param y
#' @param options
#'
#' @return
#' @export
#'
#' @examples
visualize_ropls_loadings <- function(ropls_obj, options = list()) {

  # check first whether its a orthogonal PLS or a regular PLS
  if (!is.na(ropls::getScoreMN(ropls_obj, orth = TRUE)[1])) {
    df_loadings <- data.frame(LV1 = ropls::getLoadingMN(ropls_obj),
                              LV2 = ropls::getLoadingMN(ropls_obj, orthoL = TRUE)[,1])
  } else {
    df_loadings <- data.frame(LV1 = ropls::getLoadingMN(ropls_obj)[,1],
                              LV2 = ropls::getLoadingMN(ropls_obj)[,2])
  }
  df_loadings$label <- options$df_features$label[match(rownames(df_loadings),
                                                       options$df_features$name)]

  if ("color_features" %in% names(options$loadings)) {
    df_loadings[[options$loadings$color_features]] <-
        options$df_features[match(rownames(df_loadings), options$df_features$name),
                            options$loadings$color_features]
  }

  plt_loadings <-  ggplot2::ggplot(df_loadings, ggplot2::aes(LV1, LV2)) +
    ggplot2::geom_vline(xintercept = 0, size = 0.3) +
    ggplot2::geom_hline(yintercept = 0, size = 0.3) +
    ggplot2::labs(x = "loadings on LV1",
                  y = "loadings on LV2") +
    ggplot2::theme_classic() +
    ggplot2::theme(aspect.ratio = 1,
          axis.text =  ggplot2::element_text(color = "black"))

  if ("color_features" %in% names(options$loadings)) {
    plt_loadings <-  plt_loadings +
      ggplot2::geom_point(size = 0.5,
                          ggplot2::aes_string(color = options$loadings$color_features)) +
      ggplot2::geom_segment(y = 0, x = 0,
                            ggplot2::aes_string(xend = "LV1", yend = "LV2",
                            color = options$loadings$color_features),
                 alpha = options$loadings$alpha) +
    ggrepel::geom_text_repel(size = 3,
                             ggplot2::aes_string(color = options$loadings$color_features,
                                         label = "label")) +
      ggplot2::scale_color_manual(values = options$colors[[options$loadings$color_features]])
  } else {
    plt_loadings <-  plt_loadings +
      ggplot2::geom_point(size = 0.5) +
      ggplot2::geom_segment(y = 0, x = 0,
                            ggplot2::aes(xend = LV1, yend = LV2),
                   alpha = options$loadings$alpha) +
      ggrepel::geom_text_repel(size = 3,  ggplot2::aes(label = label))
  }


  return(plt_loadings)
}
