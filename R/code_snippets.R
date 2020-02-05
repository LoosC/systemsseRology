# #snippets
#
# #LOG10 SCALES!
#
#   plt <- ggplot(dfBox, aes(x = antigen, y = value, fill = class)) +
#   stat_boxplot(position = position_dodge(width = 0.75), geom = 'errorbar', width = 0.2) +
#   geom_boxplot(outlier.size = 0.5) +
#   ggtitle("Total IgG - E") +
#   scale_fill_manual(values = myColors$class) +
#   theme(panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         strip.placement = "outside",
#         axis.line = element_line(colour = "black"),
#         legend.title = element_blank(),
#         legend.position = "none",
#         axis.title = element_text(size = 12),
#         axis.text.x = element_text(colour = myColors$antigen, size = 12),
#         plot.title = element_text(hjust = 0.5, size = 12)
#   ) +
#   scale_y_log10(breaks = c(10^4, 10^5, 10^6), #breaks = scales::trans_breaks("log10", function(x) 10^x),
#                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   annotation_logticks(sides = "l") +
#   ylab("MFI") +
#   xlab("")
