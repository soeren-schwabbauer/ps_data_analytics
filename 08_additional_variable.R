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
barplot_var(romania$sex_married)

# Mosaic plot

grid.arrange(romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(happy_fac, sex_married), fill = happy_fac)) +
               
               labs(title ="Comparison of happiness in life \n depending on sex & partnershp status",
                    fill = "Scale of happiness") +
               mosaic_theme, 
             
             romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(satisfaction_group, sex_married), fill = satisfaction_group)) +
               
               labs(title ="Comparison of satisfaction in life \n depending on sex & partnershp status",
                    fill = "Scale of satisfaction") +
               mosaic_theme,
             
             nrow = 1)


