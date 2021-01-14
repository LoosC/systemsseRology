#' Making a correlation network
#'
#' @param X n_samples x n_features matrix
#' @param y sel_features
#' @param options options
#'
#' @return plot object
#' @export

correlation_network <- function(X, sel_features, options = list()) {
  if (!("threshold" %in% names(options))) {
    options$threshold <- 0.7
  }
  if (!("df_features" %in% names(options))) {
    options$df_features <- data.frame(name = colnames(X),
                                      label = colnames(X))
  }
  if (!("node_size" %in% names(options))) {
    options$node_size <- 8
  }
  if (!("FDR" %in% names(options))) {
    options$FDR <- 1 # no filter based on signifance
  }
  if (!("text_size" %in% names(options))) {
    options$text_size <- 2.5
  }
  if (!("algorithm" %in% names(options))) {
    options$algorithm <- 'with_kk'
  }



  tidy_cors <- X %>%
    correlate(method = "spearman") %>%
    stretch()

  tidy_cors$p <- NA
  for (ind in 1:nrow(tidy_cors)) {
    if (!(tidy_cors$x[ind] == tidy_cors$y[ind])) {
      tmp <- cor.test(X[,tidy_cors$x[ind]], X[,tidy_cors$y[ind]], method = "spearman")
      tidy_cors$p[ind] <- tmp$p.value
    }
  }

  tidy_cors$p <- p.adjust(tidy_cors$p, method = "BH", n = length(tidy_cors$p) -
                            length(which(is.na(tidy_cors$r))))



  graph_cors <- tidy_cors %>%
    filter(abs(r) > options$threshold  &
             p <= options$FDR &
             (is.element(x, sel_features) |
                is.element(y, sel_features))) %>%
    igraph::graph_from_data_frame(directed = FALSE)

  layout <- ggraph::create_layout(graph_cors, layout = 'igraph', algorithm = options$algorithm)

  layout$name <- options$df_features$label[match(layout$name, options$df_features$name)]

  node_colors <- vector(mode = "list", length = length(V(graph_cors)$name))
  node_colors[is.element(V(graph_cors)$name, sel_features)] <- 'gray' # selected features
  node_colors[!is.element(V(graph_cors)$name, sel_features)] <- 'white' # other features

  plt_graph <- ggraph::ggraph(layout) +
    ggraph::geom_edge_link(aes(color = r), edge_width = 1.5) +
    ggplot2::guides(edge_alpha = "none", edge_width = "none") +
    ggraph::scale_edge_colour_gradientn(limits = c(-1, 1),
                                        colors = rev(colorRampPalette(RColorBrewer::brewer.pal(n = 10, name = "RdBu"))(100))) +
    ggraph::geom_node_point(shape = 21, color = "gray",
                            fill = node_colors, size = options$node_size, stroke = 0.5) +
    ggraph::geom_node_text(aes(label = name), size = options$text_size,
                           point.padding = NA, box.padding = 0, force = 0.1, repel = T) +
    ggplot2::theme(aspect.ratio = 1, legend.text = ggplot2::element_text(size = 7)) +
    ggraph::theme_graph(background = "white", base_family = 'Helvetica')
  plt <- plot(plt_graph)
  return(plt)
}
