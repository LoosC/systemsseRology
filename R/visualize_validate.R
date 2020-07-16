visualize_validate <- function(vals) {

  # ----------------- OPTIONS ----------------- #
  if (!("y_label" %in% names(options))) {
    options$y_label <- "score"
  }
  # if (!("model_color" %in% names(options))) {
  #   model_color <- "blue"
  # }
  # ----------------- END OPTIONS ----------------- #


  df_val <- data.frame(score = unlist(vals), model = gsub("_.*", "", names(unlist(vals))))

  df_val$model <- factor(df_val$model, levels = unique(df_val$model))

  # assign x-labels
  x_labels <- levels(df_val$model)
  x_labels[which(x_labels == "cv")] <- "model"
  x_labels[which(x_labels == "rf")] <- "random \nfeatures"
  x_labels[which(x_labels == "pt")] <- "permuted \nlabels"

  pval_rf <- "*"
  pval_pt <- "**"

  # for (ind in 1:length(vals)) {
  #   pval_1 <-
  # }

  y_pos <- max(df_val$score) + 0.1

  plt <- ggplot2::ggplot(df_val, ggplot2::aes(x = model, y = score), fill = "gray") +
    ggplot2::geom_violin() +
    ggplot2::stat_summary(fun.data = ggplot2::mean_se, geom = "pointrange", size = 0.3,  color = "black") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x =  ggplot2::element_text(angle = 0, hjust = 0.5)) +
    ggplot2::ylab(options$y_label) +
    ggplot2::scale_x_discrete("", labels = x_labels) +
    ggpubr::geom_bracket(xmin = 1, xmax = 2, inherit.aes = FALSE, label.size = 3,
                          y.position = y_pos, label = pval_1)

  if (nlevels(df_val$model) == 3) {
    plt <- plt +  ggpubr::geom_bracket(xmin = 1, xmax = 3, inherit.aes = FALSE, label.size = 3,
                                       y.position = y_pos + 0.07, label = pval_2)
  }

  # if (options$y_label == "accuracy") {
  #   plt <- plt + ggplot2::scale_y_continuous(breaks = c(0,0.5,1), labels = c("0", "0.5", "1"), limits = c(0,1.12))
  # }

}
