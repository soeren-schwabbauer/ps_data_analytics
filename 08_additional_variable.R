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

# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  INPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
  
}


### load dataframe
load(paste0(INPUT, "romania.rda"))
source("99_functions.R")


