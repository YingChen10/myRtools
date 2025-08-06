# install.packages(c("usethis", "devtools"))
# library(usethis)
# usethis::create_package("path/to/myRtools")  # 替换为你本地路径
# 
# 
# plot <- function(result, parameter, ylab){
#   
#   theme <- theme_bw() +
#     theme(axis.text = element_text(color = "black", size = 8),
#           axis.title = element_text(size = 8),
#           plot.title = element_text(size = 8),
#           axis.line = element_line(linewidth = 0.2),
#           axis.ticks = element_line(linewidth = 0.2),
#           panel.grid.major = element_blank(),  
#           panel.grid.minor = element_blank()) +
#     theme(legend.position = "none")
#   
#   tmp <- dplyr::select(result, Condition, all_of(parameter))
#   names(tmp) <- c('Condition', 'value')
#   
#   p <- wilcox.test(tmp$value[tmp$Condition == levels(tmp$Condition)[1]],
#                    tmp$value[tmp$Condition == levels(tmp$Condition)[2]])$p.value
#   
#   mean <- as.data.frame(
#     tmp %>% 
#       group_by(Condition) %>%
#       summarise(mean = mean(value),sd = sd(value)))
#   
#   
#   p1 <- ggplot() + 
#     geom_col(data = mean, aes(x = Condition, y = mean, fill = Condition), width = 0.5) +
#     geom_errorbar(data = mean, aes(x = Condition, ymin = mean - sd/(5^0.5), ymax = mean + sd/(5^0.5), color = Condition), width = 0) +
#     geom_jitter(data = tmp, aes(x = Condition, y = value), size = 0.8) + 
#     theme + 
#     scale_fill_manual(values = c("grey70", "red")) +
#     scale_color_manual(values = c("grey70", "red")) +
#     labs(x = '', y = ylab) +
#     annotate("text", x = 1.5, y = Inf, label = paste0('p = ', signif(p, digit = 2)), vjust = 2) 
#   
#   return(p1) 
#   
# }
rm(list = ls()) 

#source("https://raw.githubusercontent.com/YingChen10/myRtools/main/Plot_liver_weight.R")


plot <- function(result, parameter, ylab){
  
  # theme <- theme_bw() +
  #   theme(axis.text = element_text(color = "black", size = 8),
  #         axis.title = element_text(size = 8),
  #         plot.title = element_text(size = 8),
  #         axis.line = element_line(linewidth = 0.2),
  #         axis.ticks = element_line(linewidth = 0.2),
  #         panel.grid.major = element_blank(),  
  #         panel.grid.minor = element_blank()) +
  #   theme(legend.position = "none")
  # 
  tmp <- dplyr::select(result, Condition, all_of(parameter))
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
  
  
  # p1 <- ggplot() + 
  #   geom_col(data = mean, aes(x = Condition, y = mean, color = Condition), width = 0.4, fill = 'white') +
  #   geom_errorbar(data = mean, aes(x = Condition, ymin = mean - sd/(5^0.5), ymax = mean + sd/(5^0.5), color = Condition), width = 0.2) +
  #   geom_jitter(data = tmp, aes(x = Condition, y = value, color = Condition), size = 0.3, shape = 1) + 
  #   theme + 
  #   #scale_fill_manual(values = c("grey70", "red")) +
  #   scale_color_manual(values = c("grey70", "red")) +
  #   labs(x = '', y = ylab) +
  #   annotate("text", x = 1.5, y = Inf, label = paste0('p = ', signif(p, digit = 2)), vjust = 2) 
  
  return(p1) 
  
}
