rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 01.11.2022
# Tasks: 
# For the trust dimension of social capital you may use the variable asking whether people can generally be trusted or not, for the weak ties dimension you could use the sum of the variables asking whether one is a member of ....
################################################################################

# load libraries
library(dplyr)

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

# load dataframe
load(paste0(INPUT, "Romania.rda"))


##### define themes and create functions for plotting ##########################



################################################################################




##### START REPORT #############################################################

# For the following report, 

# The literature shows that Social Capital is an important factor for increasing the level of SWB

romania$trust

