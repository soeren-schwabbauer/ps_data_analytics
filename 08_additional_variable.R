rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 28.11.2022
# Tasks: Pick one variable you think may be important and you are interested in. Argue for your chosen variable. Provide some descritptive statistics for it as well.
# Variables: freedom
################################################################################

# load libraries
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(gridExtra)
library(tidyverse)

# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  INPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("~/Google Drive/Geteilte Ablagen/")) {
  
  INPUT = "~/Google Drive/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
}


### load dataframe
load(paste0(INPUT, "romania.rda"))
source("99_functions.R")


# Barplot
barplot_var(sex_fac)


# Mosaic plot
income_eq_happy <-  romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(happy_fac, sex_fac), fill = happy_fac)) +
  
  labs(title ="Comparison of happiness in life \n and income equality",
       fill = "Scale of happiness") +
  mosaic_theme

income_eq_satis <- romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(satisfaction_group, sex_fac), fill = satisfaction_group)) +
  
  labs(title ="Comparison of satisfaction in life \n and income equality",
       fill = "Scale of satisfaction") +
  mosaic_theme

grid.arrange(income_eq_happy, income_eq_satis, nrow = 1)



##################################################################################################################
# Add another variable that may be important and you are interested in.
# Another variable that can increase happyness and satisfaction, both subjective measures, is income equality.
# I would argue that income equality is highly subjective measure as well, as I could imagine living in poverty
# directly beside somebody who is very well of would increase dissatisfaction and unhappiness. So we will find out if
# income equality does or does not increase happiness and satisfaction. 


# First step is to have a general overview of the distribution of income equality
bar_incomeeq <- barplot_var(income_eq_fac)
box_incomeeq <- boxplot_for_barplot(income_equality) +
  labs(title = "How much income equality") 
grid.arrange(box_incomeeq, bar_incomeeq, nrow = 2, heights = c(0.5,2))

# From looking at the boxplot we can see, that the median distribution is at the level of 8, which comes as a 
# surprise. Most people 45% voted for needing a larger income difference. the rest of the 55% are distributed rather 
# arbitrarily with group 1,5 & 8 with about 10%. 

# Let us now look if we can find some associations between lifesatisfaction or happiness in life together with  
# income equality in life. 

income_eq_happy <-  romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(happy_fac, income_eq_fac), fill = happy_fac)) +
  
  labs(title ="Comparison of happiness in life \n and income equality",
       fill = "Scale of happiness") +
  mosaic_theme

income_eq_satis <- romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(satisfaction_group, income_eq_fac), fill = satisfaction_group)) +
  
  labs(title ="Comparison of satisfaction in life \n and income equality",
       fill = "Scale of satisfaction") +
  mosaic_theme

grid.arrange(income_eq_happy, income_eq_satis, nrow = 1)

# Interpretation nach gruppieren? oder keine gruppierungen?