rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 19.12.2022
# Tasks:  Quantify the relationship between “life satisfaction” and “control over life”, “health” and your variable of interest with an appropriate bivariate statistic measure and a corresponding test for statistical significance
# Variables: satisfaction_life, freedom, health, education
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


