#### file for functions


barplot_var <- function(var){
  romania %>%
    ggplot(aes(x= {{var}}, y= ..count.. , fill = {{var}})) +
    geom_bar() +
    labs(x = "",
         y = "total number") +
    geom_text(stat = "count", aes(label= paste0(..count.., " (", round(..count../nrow(romania),4)*100 , "%)")), vjust = -0.5) +
    
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey") ,
      plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5)
      
    )
}


boxplot_for_barplot <- function(var){
  romania %>%
    ggplot(aes(y = as.numeric({{var}}))) +
    geom_boxplot(width = 0.5) +
    coord_flip() +
    
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"),
      plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5)
    ) 
}