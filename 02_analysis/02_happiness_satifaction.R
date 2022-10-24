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
library(ggmosaic)

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
  legend.position = "none",
  plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5)
) 

theme_barplot <- theme(
  legend.position = "none",
  axis.title.x = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey") ,
) 
  
################################################################################


# load dataframe
load(paste0(INPUT, "Romania.rda"))


###### 1. explore main variables (UNIVARIATE ANALYSIS) #########################

barplot_variable <- function(var){
  romania %>%
    ggplot(aes(x= {{var}}, y= ..count.. , fill = {{var}})) +
    geom_bar() +
    labs(x = "",
         y = "total number") +
    geom_text(stat = "count", aes(label=..count..), vjust = -0.5) +
    
    theme_barplot
}


mosaic_var1_var2 <- function(var1, var2){
  romania %>%
    ggplot() +
    geom_mosaic(aes(x =  product({{var1}}, {{var2}}), fill = {{var1}})) +

    theme_mosaic

}

#### A170 satisfaction in life #################################################
# summary for quick overview
summary(romania$satisfaction_life)
# only serious answers, no na's, missing, or unrealistic values

# plot satsfaction


barplot_variable(satisfaction_life_fac) + 
  labs(title = "Overall life satisfaction") 
# CONCLUSION: tbc...


### A008 Feeling of happiness ##################################################

# summary for quick overview
summary(romania$feel_happy)
# only serious answers, no na's, missing, or unrealistic values

# plot happiness
barplot_variable(feel_happy_fac) +
  labs(title = "Overall feeling of happiness")

# CONCLUSION: tbc...




###### 2. MULTIVARIATE ANALYSIS ################################################
# comparing two quantitative variables by mosaic plot
romania$satisfaction_life_fac <- droplevels(romania$satisfaction_life_fac)
romania$income_scale_fac <- droplevels(romania$income_scale_fac)
romania$feel_happy_fac <- droplevels(romania$feel_happy_fac)

### income (Sören) #############################################################

### income distribution
barplot_variable(income_scale_fac) +
  
  labs(title = "Income distribution") 
# DESCRIBE WHAT YOU SEE


### mosaic plots
inc_happy <- mosaic_var1_var2(feel_happy_fac, income_scale_fac) +
  labs(title ="Comparison of satisfaction in life and income",
       fill = "Scale of satisfaction")


inc_satis <- romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(satisfaction_life_fac, income_scale_fac), fill = satisfaction_life_fac)) +
  
  labs(title ="Comparison of satisfaction in life and income",
       fill = "Scale of satisfaction") +
  theme_mosaic

grid.arrange(inc_happy, inc_satis, nrow = 1)
### Conclusion: 


### health #####################################################################
barplot_variable(health_fac) +
  
  labs(title = "Health distribution")
## INTERPRETATION: 


### age ########################################################################



### marital status #############################################################
barplot_variable(marst_fac) +
  
  labs(title = "Marriage distribution")
## INTERPRETATION: 

### additional variables #######################################################
