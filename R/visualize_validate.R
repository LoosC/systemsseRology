#' Visualization of validation results
#'
#' @param vals
#' @param options
#'
#' @return plot handle
#' @export
visualize_validate <- function(vals, options = list()) {

  # ----------------- OPTIONS ----------------- #
  if (!("y_label" %in% names(options))) {
    options$y_label <- "score"
  }
  # ----------------- END OPTIONS ----------------- #


  tmp_vals <-  unlist(vals)
  tmp_vals <- tmp_vals[which(grepl("score", names(tmp_vals)))]
  df_val <- data.frame(score = tmp_vals, model = gsub("_.*", "", names(tmp_vals)))
  df_val$model <- factor(df_val$model, levels = unique(df_val$model))

  # assign x-labels
  x_labels <- levels(df_val$model)
  x_labels[which(x_labels == "cv")] <- "model"
  x_labels[which(x_labels == "rf")] <- "random \nfeatures"
  x_labels[which(x_labels == "pt")] <- "permuted \nlabels"


  # Calculate p-values and generate label
  if (is.null(names(vals))) {
    n_repl <- length(vals)
  } else {
    n_repl <- 1
  }

  if (n_repl > 1) {
    # random features
    if ("rf" %in% levels(df_val$model)) {
      pval_rf <- rep(NA, length = length(vals))

      for (ind in 1:length(vals)) {
        pval_rf[ind] <- length(which(vals[[ind]]$rf_scores > vals[[ind]]$cv_score))/length(vals[[ind]]$rf_scores)
      }

      if (median(pval_rf) == 0) {
        label_rf <- paste0("p<", 1/length(vals[[ind]]$rf_scores))
      } else {
        label_rf <- paste0("p=", mean(pval_rf))
      }
    }

    # permuted labels
    if ("pt" %in% levels(df_val$model)) {
      pval_pt <- rep(NA, length = length(vals))

      for (ind in 1:length(vals)) {
        pval_pt[ind] <- length(which(vals[[ind]]$pt_scores > vals[[ind]]$cv_score))/length(vals[[ind]]$pt_scores)
      }

      if (median(pval_pt) == 0) {
        label_pt <- paste0("p<", 1/length(vals[[ind]]$pt_scores))
      } else {
        label_pt <-  paste0("p=", mean(pval_pt))
      }
    }
  } else {
    # random features
    if ("rf" %in% levels(df_val$model)) {
      pval_rf <- length(which(vals$rf_scores > vals$cv_score))/length(vals$rf_scores)
      if (pval_rf == 0) {
        label_rf <- paste0("p<", 1/length(vals$rf_scores))
      } else {
        label_rf <- paste0("p=", pval_rf)
      }
    }

    # permuted labels
    if ("pt" %in% levels(df_val$model)) {
      pval_pt <- length(which(vals$pt_scores > vals$cv_score))/length(vals$pt_scores)
      if (pval_pt == 0) {
        label_pt <- paste0("p<", 1/length(vals$pt_scores))
      } else {
        label_pt <-  paste0("p=", pval_pt)
      }
    }
  }


  y_pos <- max(df_val$score) + 0.05

  data_summary <- function(x) {
    m <- mean(x)
    ymin <- m - sd(x)
    ymax <- m + sd(x)
    return(c(y = m, ymin = ymin, ymax = ymax))
  }

  plt <- ggplot2::ggplot(df_val, ggplot2::aes(x = model, y = score), fill = "gray") +
    ggplot2::geom_violin(fill = "gray", color = "gray") +
    ggplot2::stat_summary(fun.data = data_summary, geom = "pointrange", size = 0.6, fatten = .8, color = "black") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none",
                   axis.text = ggplot2::element_text(color = "black", size = 6),
                   axis.title = ggplot2::element_text(color = "black", size = 8),
                   axis.text.x =  ggplot2::element_text(size = 8, angle = 0, hjust = 0.5)) +
    ggplot2::ylab(options$y_label) +
    ggplot2::scale_x_discrete("", labels = x_labels)

  if ("rf" %in% levels(df_val$model)) {
    plt <- plt +  ggpubr::geom_bracket(xmin = 1, xmax = which(levels(df_val$model) == "rf"),
                                       inherit.aes = FALSE, label.size = 2.5,
                                       y.position = y_pos, label = label_rf)
  }

  if ("pt" %in% levels(df_val$model)) {
    plt <- plt +  ggpubr::geom_bracket(xmin = 1, xmax = which(levels(df_val$model) == "pt"),
                                       inherit.aes = FALSE, label.size = 2.5,
                                       y.position = y_pos + 0.12, label = label_pt)
  }

  if (grepl("ccuracy", options$y_label)) {
   plt <- plt + ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1),
                                            labels = c("0", "0.5", "1"),
                                            limits = c(0, max(1, max(df_val$score) + 0.22)))
  }
  return(plt)

}
