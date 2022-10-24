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

# define theme for mosaic ######################################################
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

# define function, incl. theme for barplots ####################################

barplot_variable <- function(var){
  romania %>%
    ggplot(aes(x= {{var}}, y= ..count.. , fill = {{var}})) +
    geom_bar() +
    labs(x = "",
         y = "total number") +
    geom_text(stat = "count", aes(label=..count..), vjust = -0.5) +
    
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey") 
    )
}


boxplot_variable <- function(var){
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
      
      
    )
  
  
}

################################################################################


# load dataframe
load(paste0(INPUT, "Romania.rda"))

################################################################################
###### 1. explore main variables (UNIVARIATE ANALYSIS) #########################

# A170 satisfaction in life ####################################################
# summary for quick overview
summary(romania$satisfaction_life)
# Interpreatition: 
# The summary command allows us to have a rough overview of the variable A170 - satisfaction in life.
# The variable is scaled from 1 to 10, with 10 being the highest level of satisfaction.
# Since one can rank the level of satisfaction (e.g: a level of 8 being higher ranked than
# a level 2), but the distnace between the levels is subjective (e.g: a person on a level 8
# is not necesearily twice as happy as a person on the level 4) Life satisfaciton can be considered
# an ordinally scaled varibale. 

# The summary command shows, that everybody had replied to to questions (no NA's).
# Another conclusion which can be drawn from the summary command is the average level of
# satisfaction. With a median level of 8, and a mean of 7.5, the Romanian people are generally
# not extremely satisfied, but also not unsatisfied with their life as a whole. 
# In addition to that, it can be seen, that the half of the aksed population ranks themself
# between a 6 and a 9.

# The following plot allows a look at the disrtibution of satisfaction
bar_satiscation <- barplot_variable(satisfaction_life_fac) 

box_satiscation <- boxplot_variable(satisfaction_life) +
  labs(title = "Overall life satisfaction") 

grid.arrange(box_satiscation, bar_satiscation, nrow = 2, heights = c(0.5,2))
# Interpreation: The barplot on the distribution of satisfaction in life adds additional
# information to the summary command. As one can see, with the highest level "satisfied"
# the most people can identify. It remains unclear, why there is such a rapid drop one level below.
# Furthermore it can be seen, that the lower 25% are much more distributed (from 2 to 5), 
# whereas "Satisfied" contains all the upper 25% of the counts. 


### A008 Feeling of happiness ##################################################

# summary for quick overview
summary(romania$feel_happy)
# Also the variable happy only contains interpretable values. Neither has nobody not answered,
# nor has the question being skipped, other other problems occured. The varibale is scaled from one to
# 4. One indicates "very happy", 4 indicates "not at all happy". Just like with 
# satisfaction, the feeling of happiness counts as a orindally scaled variable.

# the following barplot allows a closer look at the distribution of happiness within
# the questioned people in romania
barplot_variable(feel_happy_fac) +
  labs(title = "Overall feeling of happiness")
# From the barplot one can conclde, that most people (approx. 50%) are "quite happy",
# 25% of the questioned population are "very happy" and roughly 25% are "not very happy", or 
# "not at all happy"



################################################################################
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
inc_happy <-  romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(feel_happy_fac, income_scale_fac), fill = feel_happy_fac)) +
  
  labs(title ="Comparison of happiness in life and income",
       fill = "Scale of satisfaction") +
  theme_mosaic
  
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
