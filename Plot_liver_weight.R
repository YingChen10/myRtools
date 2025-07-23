plot <- function(result, parameter, ylab){
  
  theme <- theme_bw() +
    theme(axis.text = element_text(color = "black", size = 8),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 8),
          axis.line = element_line(linewidth = 0.2),
          axis.ticks = element_line(linewidth = 0.2),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank()) +
    theme(legend.position = "none")
  
  tmp <- dplyr::select(result, Condition, all_of(parameter))
  names(tmp) <- c('Condition', 'value')
  
  p <- wilcox.test(tmp$value[tmp$Condition == levels(tmp$Condition)[1]],
                   tmp$value[tmp$Condition == levels(tmp$Condition)[2]])$p.value
  
  mean <- as.data.frame(
    tmp %>% 
      group_by(Condition) %>%
      summarise(mean = mean(value),sd = sd(value)))
  
  
  p1 <- ggplot() + 
    geom_col(data = mean, aes(x = Condition, y = mean, fill = Condition), width = 0.5) +
    geom_errorbar(data = mean, aes(x = Condition, ymin = mean - sd/(5^0.5), ymax = mean + sd/(5^0.5), color = Condition), width = 0) +
    geom_point(data = tmp, aes(x = Condition, y = value), size = 0.3, color = 'grey30') + 
    theme + 
    scale_fill_manual(values = c("grey70", "red")) +
    scale_color_manual(values = c("grey70", "red")) +
    labs(x = '', y = ylab) +
    annotate("text", x = 1.5, y = Inf, label = paste0('p = ', signif(p, digit = 2)), vjust = 2) 
  
  return(p1) 
  
}
