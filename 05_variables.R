# Getting Started
rm(list = ls())

# load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(webr)
library(patchwork)
library(reshape2)
library(factoextra)
library(spatstat.utils)
library(sjlabelled)

# Working Directory
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  INPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
  
}

# load dataframe
load(paste0(INPUT, "Romania.rda"))

# Cause I'm rather frustrated...
load("C:/Users/david/OneDrive/Desktop/Data Analytics Data/romania.rda")

# In the following, we will investigate whether there is a difference in active associational memberships and passive memberships. First, we will investigate how the activities are distributed over the Romanian population and will start with univariate analysis of the activity variables.

# Step1 - creating a counting function for memberships
ties_var_summary <- function(var, var_char){
  romania %>% select({{var}},contains("member_")) %>% rename(member = {{var}}) %>% melt() %>%
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


ties_var_summary(data)

##############################################################################################

bind_rows( fun_happy(happy, member_activity, "an activity"),
           fun_happy(happy, member_religion, "a religion"),
           fun_happy(happy, member_selfhelp, "a selfhelp group"),
           fun_happy(happy, member_charity, "a charity"),
           fun_happy(happy, member_labor_union, "a labor union"),
           fun_happy(happy, member_sports, "a sports group")) %>%
  
  group_by(member, cat, happy) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)) %>%
  
  ggplot(aes(x = as_factor(member), y = perc, fill = as_factor(happy))) +
  geom_bar(stat = "identity") +
  labs(fill = "Level of Happiness",
       y = "Distribution (%)",
       title = "Lebel of happiness, given somebody is a member of ...") +
  
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  
  facet_grid( ~ cat) +
  
  theme_cat





