visualize_validate <- function(validate) {

  # ----------------- OPTIONS ----------------- #
  if (!("y_label" %in% names(options))) {
    options$y_label <- "score"
  }
  if (!("model_color" %in% names(options))) {
    model_color <- "blue"
  }
  # ----------------- END OPTIONS ----------------- #


  df_val <- data.frame(score = return_vals$cv_score,
                       model = "model")
  df_rf <- data.frame(score = return_vals$rf_scores,
                      model = "rf")

  df_all <- rbind(df_val, df_rf)

  pval_rf <- "*"
  pval_pt <- "**"

  pltVio <- ggplot2::ggplot(df_all, ggplot2::aes(x = model, y = score, fill = model)) +
    ggplot2::geom_violin() +
    ggplot2::stat_summary(fun.data = "mean_sd", geom = "pointrange", size = 0.3,  color = "black") +
    ggplot2::scale_fill_manual(values = c(model_color, "gray", "gray")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x =  ggplot2::element_text(angle = 0, hjust = 0.5)) +
    ggpubr::geom_bracket(xmin = 1, xmax = 2, inherit.aes = FALSE, label.size = 3,
                 y.position = 1, label = pval_rf) +
    ggpubr::geom_bracket(xmin = 1, xmax = 3, inherit.aes = FALSE, label.size = 3,
                 y.position = 1.07, label = pval_pt) +
    ggplot2::ylab(options$y_label) +
   #ggplot2::scale_y_continuous(options$y_label, breaks = c(0,0.5,1), labels = c("0", "0.5", "1"), limits = c(0,1.12)) +
    ggplot2::scale_x_discrete("", labels = c("model", "random \nfeatures", "permuted \nlabels"))
}
