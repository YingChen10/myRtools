
cat("Usage: plot(data, parameter, ylab)\n",
    "Notes:\n",
    "  data: data frame containing condition and value information\n",
    "  parameter: column name of the value of interest\n",
    "  ylab: title of y axis\n",
    "  'Condition' column should be set as a factor with the desired group order,\n",
    "    e.g., Condition <- factor(Condition, levels = c('GFP', 'Cre'))\n"
    " library(cowplot)\n aligned <- align_plots(p1, p2, p3, align = "hv", axis = "tblr")\n final_plot <- plot_grid(plotlist = aligned, nrow = 1)\n ggsave("tg.pdf", final_plot, width = 3.5, height = 1.6)"
)
plot <- function(result, parameter, ylab){
  
  tmp <- dplyr::select(result, Condition, parameter)
  names(tmp) <- c('Condition', 'value')
  
  p_val <- wilcox.test(tmp$value[tmp$Condition == levels(tmp$Condition)[1]],
                       tmp$value[tmp$Condition == levels(tmp$Condition)[2]])$p.value
  
  # subtitle_text <- paste0("italic(p) == ", p_val)
  
  
  mean <- as.data.frame(
    tmp %>% 
      group_by(Condition) %>%
      summarise(mean = mean(value),sd = sd(value)))
  
  
  ymax <- max(tmp$value) * 1.1 
  
  theme <- theme_bw() +
    theme(
      axis.text = element_text(color = "black", size = 8),
      axis.title = element_text(size = 8),
      plot.title = element_text(size = 8),
      axis.line = element_line(linewidth = 0.2),      
      axis.ticks = element_line(linewidth = 0.2),    
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      panel.border = element_blank(), # no outside border
      legend.position = "none"
    )
  
  p1 <- ggplot() + 
    geom_col(data = mean, aes(x = Condition, y = mean, color = Condition), 
             width = 0.6, fill = 'white', linewidth = 0.22) +  
    geom_errorbar(data = mean, aes(x = Condition, 
                                   ymin = mean - sd/(5^0.5), 
                                   ymax = mean + sd/(5^0.5), 
                                   color = Condition),
                  width = 0.2, linewidth = 0.22) +             
    geom_jitter(data = tmp, aes(x = Condition, y = value, color = Condition),
                size = 1.5, width = 0.08, shape = 1, stroke = 0.5) + 
    theme + 
    scale_color_manual(values = c("grey30", "red")) +
    labs(x = '', y = ylab) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
    annotate("segment", x = 1, xend = 2, y = ymax, yend = ymax, linewidth = 0.22) +
    #annotate("text", x = 1.5, y = ymax * 1.1, label = bquote(italic(p) == .(signif(p_val, 2))))
    annotate("text", x = 1.5, y = ymax * 1.1, label = paste0('p = ',signif(p_val, digit = 2)), size = 3) 
  #annotate("text", x = 1.5, y = ymax * 1.1, label = bquote(italic(p) == .(signif(p_val, 2))), size = 3)
  
  return(p1) 
  
}
