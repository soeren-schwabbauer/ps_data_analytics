rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 13.10.2022
# Purpose: 1) explore main variables
#          2) compare main variables to other variables
################################################################################

# load libraries
library(dplyr)
library(haven)
library(ggplot2)
library(ggmosaic)
library(lazyeval)
library(gridExtra)

# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/02_analysis/INPUT/"
  OUTPUT = "G:/Geteilte Ablagen/data_analytics/02_analysis/OUTPUT/"
  CODEBOOK = "G:/Geteilte Ablagen/data_analytics/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  
  INPUT = "G:/Shared drives/data_analytics/02_analysis/INPUT/"
  OUTPUT = "G:/Shared drives/data_analytics/02_analysis/OUTPUT/"
  CODEBOOK = "G:/Shared drives/data_analytics/"
  
}

# define themes ################################################################
theme_mosaic <- theme( 
  #legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_text(angle=90),
  panel.background = element_rect(fill = "white"),
  panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                    colour = "white") ,
  
  plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5)
) 
################################################################################


# load dataframe
load(paste0(INPUT, "Romania.rda"))


###### 1. explore main variables (UNIVARIATE ANALYSIS) #########################

# factorize both variables (maybe add to 01)
romania$satisfaction_life <- as_factor(romania$satisfaction_life)
romania$feel_happy <- as_factor(romania$feel_happy)

distri_var <- function(var, varchar){
  
  romania %>%
    group_by({{var}}) %>%
    summarize(n = n()) %>%
    rename(factor = {{var}}) %>%
    mutate(perc = n/ sum(n), 
           group = varchar)
# this function outputs a %distibution and a total distribution of the main
# varibales
}

#### A170 satisfaction in life #################################################
# summary for quick overview
summary(romania$satisfaction_life)
# only serious answers, no na's, missing, or unrealistic values

# plot satsfaction
distri_var(satisfaction_life, "satisfaction_life") %>%
  ggplot(aes(fill = factor, y= n, x= factor)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Overall life satisfaction",
       x = "",
       y = "total number",
       fill = "Level of satisfaction") 

# CONCLUSION: tbc...


### A008 Feeling of happiness ##################################################

# summary for quick overview
summary(romania$feel_happy)
# only serious answers, no na's, missing, or unrealistic values

# plot happiness
distri_var(feel_happy, "feel_happy") %>%
  ggplot(aes(fill = factor, y= n, x= factor)) + 
    geom_bar(position="dodge", stat="identity") +
  labs(title = "Overall feeling of happiness",
       x = "",
       y = "total number",
       fill="Level of happiness")

# CONCLUSION: tbc...




###### 2. MULTIVARIATE ANALYSIS ################################################
# comparing two quantitative variables by mosaic plot
romania$satisfaction_life <- as_factor(romania$satisfaction_life) %>%
  droplevels()
romania$income_scale <- as_factor(romania$income_scale) %>%
  droplevels()
romania$feel_happy <- as_factor(romania$feel_happy) %>%
  droplevels()

### income (Sören) #############################################################

### income distribution
romania %>%
  ggplot(aes(x= income_scale, y= ..count.. , fill = income_scale )) +
  geom_bar() +
  labs(title = "Income distribution",
       x = "",
       y = "total number",
       fill="Level of happiness") +
  theme(legend.position = "none")

# DESCRIBE WHAT YOU SEE

inc_happy <- romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(feel_happy, income_scale), fill = feel_happy)) +
  
  labs(title ="Comparison of happiness and income",
       fill = "Scale of happiness") +
  theme(legend.position = "none") +
  theme_mosaic

inc_satis <- romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(satisfaction_life, income_scale), fill = satisfaction_life)) +
  
  labs(title ="Comparison of satisfaction in life and income",
       fill = "Scale of satisfaction") +
  theme(legend.position = "none") +
  theme_mosaic

grid.arrange(inc_happy, inc_satis, nrow = 1)
### Conclusion: 


### health #####################################################################



### age ########################################################################



### marital status #############################################################


### additional variables #######################################################
