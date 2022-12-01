#### file for functions
boxplot_age <- function(var1, var2, var3){
  romania %>% 
    ggplot(aes(x = {{var1}}, y = {{var2}})) +
    geom_boxplot(aes(fill = {{var3}})) +
    
    theme(
      axis.title.x = element_blank(),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      #axis.title.y = element_blank(),
      #axis.text.y = element_blank(),
      #axis.ticks.y = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"),
      plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5)
      
    ) +
    scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80))
}

activity_fun <- function(var, var_char){
  romania %>% select(happy, {{var}}) %>% rename(member = {{var}}) %>% mutate(cat = {{var_char}})
}

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


mosaic_theme <-   theme( 
  #legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_text(angle=90),
  panel.background = element_rect(fill = "white"),
  panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                    colour = "white") ,
  legend.position = "none",
  plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5)
) 


ties_var_summary <- function(var, var_char){
  romania %>% select({{var}},contains("member_"), -contains("active"), -contains("tot"), -contains("any"))  %>% rename(member = {{var}}) %>% melt() %>%
    group_by(value, member) %>%
    summarise(n = n()) %>%
    mutate(prop = n/sum(n)) %>%
    
    mutate(value = replace(value, value == 1, "member"),
           value = replace(value, value == 0, "no member"),
           value = as_factor(value)) %>%
    
    ggplot(aes(x = (value), y = prop, fill = as_factor(member))) +
    geom_bar(stat = "identity") +
    labs(y = "Distribution (%)") + 
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle=0),
      #axis.title.y = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                        colour = "grey") ,
      plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5) ,
      plot.subtitle = element_text(color = "black", size = 10, face = "italic")
      
    ) +
    
    scale_y_continuous(labels = scales::percent,
                       breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) 
}


theme_cat <-   theme( 
  axis.title.x = element_blank(),
  axis.text.x = element_text(angle=0),
  #axis.title.y = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                    colour = "grey") ,
  plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5) 
) 


fun_happy <- function(cat, var, var_char){
  romania %>% select({{cat}}, {{var}}) %>% rename(member = {{var}}) %>% mutate(cat = {{var_char}}) %>%
    
    mutate(member = as.character(as_factor(member)),
           member = replace(member, member == "Mentioned", "yes"),
           member = replace(member, member == "Not mentioned", "no"))
}

trust_var_summary <- function(var, var_char){
  romania %>% select(trust_fac, {{var}}) %>% rename(member = {{var}}) %>% mutate(cat = {{var_char}})
}

