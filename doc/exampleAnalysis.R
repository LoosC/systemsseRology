## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE------------------------------------------------
library(systemsseRology)
library(caret)
library(pracma)
library(glmnet)
library(ropls)
library(ggrepel)

## ----colors and shapes etc-----------------------------------------------
myColors = list(
    feature_class = c(titer = "#797d7f", FcR = "#424949", func = "#cacfd2"),
    antigen = c(ag1 = "#FB9301", ag2 = "#0B1892", ag3 = "#9A1C73", ag4 = "#1A8E00"),
    class = c(class1 = "#1f618d", class2 = "#5dade2")
    )

## ----load data-----------------------------------------------------------
# generate random data as an example
X_unscaled <- matrix(rnorm(30*4*7), 30, 4*7)  
X_unscaled[1:10, 3] <- X_unscaled[1:10, 3] - 0.5
X_unscaled[1:10, 5] <- X_unscaled[1:10, 3] + 0.5
X_unscaled[, 4] <- X_unscaled[, 5] + rnorm(30)/10
rownames(X_unscaled) <- seq(1:30)

measurement <- c("IgG1", "IgG2", "IgG3", "Fcg2AR", "Fcg3AR", "ADCP", "ADNP")
antigen <- c("ag1", "ag2", "ag3", "ag4")
feature_names <- apply(expand.grid(measurement, antigen), 1, paste, collapse = "_")
feature_names <- feature_names[order(feature_names)]
colnames(X_unscaled) <- feature_names

X <- scale(X_unscaled, center = TRUE, scale = TRUE) 

#y <- rep(c(0,1), c(10, 20))
yName <- factor(rep(c("class1", "class2"), c(10, 20), levels = c("class1", "class2")))

nAntigens <- 4
nSamples <- dim(X)[1]
  
feature_annot <- data.frame(label = feature_names,
                         antigen = gsub(".*_","", feature_names),
                         measurements = gsub("_.*","", feature_names),
                         unit = rep(c("Phagocytosis Score", "MFI", "MFI"), c(8, 8, 12)),
                         feature_class = rep(c("func", "FcR", "titer"), c(8, 8, 12)))
feature_annot$antigen <- factor(feature_annot$antigen, levels = antigen)
rownames(feature_annot) <- colnames(X)

## ----heatmap, fig.height=6, fig.width=6, message=TRUE--------------------
library(pheatmap)

annotation_row = data.frame(class = yName)
rownames(annotation_row) <- rownames(X)

p <-  pheatmap(X, cellwidth = 5, cellheight = 5, fontsize = 6,
               annotation_row = annotation_row, 
               annotation_col = feature_annot[,c("antigen", "feature_class")], 
               annotation_colors = myColors,
               gaps_row = length(which(yName == yName[1])), 
               gaps_col = seq(nAntigens, dim(X)[2], nAntigens), 
               cluster_rows = FALSE, 
               cluster_cols = FALSE)
#pdf("Figures/heatmap.pdf", width = 12, height = 5) 
print(p)
#garb <- dev.off()
#print(p)

## ----univariate dotplots 1, message=FALSE, warning=FALSE, fig.width=10, fig.height=6----
library(ggplot2)
library(reshape2)
library(ggpubr)

dfUniv <- melt(cbind(as.data.frame(X_unscaled), yName))
dfUniv$antigen <- sub(".*_","", dfUniv$variable)
dfUniv$variable <- sub("_.*","", dfUniv$variable)

plt <- list()
for (ind in 1:(dim(X)[2]/nAntigens)) {
  plt[[ind]] <- ggplot(dfUniv[dfUniv$variable == unique(dfUniv$variable)[ind],], 
                       aes(x = antigen, y = value, fill = antigen)) +
          geom_dotplot(binaxis = "y",  stackdir = "center", stackratio = 0.7, dotsize = 1.5) +
          stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                  eom = "crossbar", width = 0.5, size = 0.3) +
          scale_fill_manual(values = myColors$antigen) + 
          labs(x = "",  y = feature_annot$unit[which(feature_annot$measurement == 
                                                       unique(dfUniv$variable)[ind])[1]]) +
          ggtitle(unique(dfUniv$variable)[ind]) + 
          theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  strip.placement = "outside",
                  axis.line = element_line(colour = "black"), 
                  legend.title = element_blank(),
                  legend.position = "none",
                  axis.title = element_text(size = 10),
                  plot.title = element_text(size = 10, face = "bold", hjust = 0.5)) 
}
p <- ggarrange(plotlist = plt, ncol = ceiling((dim(X)[2]/nAntigens)/4), nrow = 4)

#pdf("univariate_dotPlot1.pdf", width = ceiling((dim(X)[2]/nAntigens)), height = 8) 
print(p)
#garb <- dev.off()
#print(p)

## ----univariate dotplots 2, message=FALSE, fig.width=10, fig.height=6----
library(ggplot2)
library(reshape2)

dfUniv <- melt(cbind(as.data.frame(X_unscaled), yName))
dfUniv$antigen <- sub(".*_","", dfUniv$variable)
dfUniv$variable <- sub("_.*","", dfUniv$variable)

p <- ggplot(dfUniv, aes(x = antigen, y = value, fill = antigen)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 0.7, dotsize = 1.5) + 
      stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.5, size = 0.3) +
      scale_fill_manual(values = myColors$antigen) + 
      facet_wrap(.~variable, nrow = 5, strip.position = "left", scales = "free") +
      theme_bw() +
      #scale_y_continuous(limits = c(0,3)) +
      labs(x = "", y = "") +
      theme(panel.background = element_blank(),
            axis.line = element_line(colour = "black"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.border = element_rect(colour = "black"),
            strip.placement = "outside",
            legend.title = element_blank()
            ) 

#pdf("univariate_dotPlot2.pdf", width = ceiling((dim(X)[2]/nAntigens)), height = 8) 
print(p)
#garb <- dev.off()
#print(p)

## ----PCA-----------------------------------------------------------------


## ----LASSO OPLS-DA for visualization, fig.width=10, fig.height=6, warnings=FALSE, message=FALSE----
selFeatures <- featureSelection(X, yName, method = "lasso",
                                nFeatRep = 100, nLassoFolds = 5, thresh = 25, alpha = 1)
print(selFeatures)
  
X_sel <- X[,which(colnames(X) %in% selFeatures)]
pre_symbol <- try(oplsda <- opls(X_sel, yName, orthoI = NA, permI = 0, crossValI = 5))
isError <- is(pre_symbol, 'try-error')
if (isError) {
  oplsda <- opls(X_sel, yName, predI = 1, orthoI = 1, permI = 0, crossValI = 5)
}

## ----Visualization-------------------------------------------------------
feature_annot$useColor <- myColors$antigen[feature_annot$antigen]
plsBiplot(oplsda, yName, color1 = myColors$class[["class1"]], 
          color2 = myColors$class[["class2"]], alpha_loading = 0.5,
          feature_annot = feature_annot)

plsBarplot(oplsda, yName, X_sel, color1 = myColors$class[["class1"]], 
           color2 = myColors$class[["class2"]], 
           feature_annot = feature_annot)


## ----Model validation, echo=FALSE, message=FALSE, warnings=FALSE, results='hide'----
knitr::opts_chunk$set(warning = FALSE)

modVal <- modelValidation(X, yName, nFolds = 5, nReps = 2, nPerms = 10, 
                          nFeatRep = 10, thresh = 3)

pvals <- visualizeModelValidation(modVal, type = "classification", modelColor = "gray")
print(pvals)

## ----Co-correlation network, message=FALSE-------------------------------
library(tidyverse)
library(corrr)
library(igraph)
library(ggraph)
library(ggrepel)


tidyCors <- X %>% 
  correlate(method = "spearman") %>% 
  stretch()

tidyCors$p <- 0
for (ind in 1:dim(tidyCors)[1]) {
  tmp <- cor.test(X[,tidyCors$x[ind]], X[,tidyCors$y[ind]], method = "spearman")
  tidyCors$p[ind] <- tmp$p.value
}
tidyCors$p <- p.adjust(tidyCors$p, method = "BH", n = length(tidyCors$p))
  
plotFeatures <- selFeatures
  
graphCors <- tidyCors %>% 
  filter(p < 0.05 & (is.element(x, plotFeatures) | is.element(y, plotFeatures))) %>% 
  graph_from_data_frame(directed = FALSE)

layout <- create_layout(graphCors, layout = 'igraph', algorithm = 'nicely')

nodeColours <- vector(mode = "list", length = length(V(graphCors)$name))
nodeColours[is.element(V(graphCors)$name, plotFeatures)] <- 'gray' # selected features
nodeColours[!is.element(V(graphCors)$name, plotFeatures)] <- 'white' # other features

pltGraph <- ggraph(layout) +
  geom_edge_link(aes(color = r, edge_width = abs(r))) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(shape = 21, color = "gray", fill = nodeColours, size = 8, stroke = 0.5) +
  geom_node_text(aes(label = name), size = 3, repel = T) +
  theme(aspect.ratio = 1) +
  theme_graph(background = "white") 
plt <- plot(pltGraph)

