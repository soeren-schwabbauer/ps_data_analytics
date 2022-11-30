rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 28.11.2022
# Tasks: analysis of strong ties (How important in your life: family)
# Variables: imp_family
################################################################################

# load libraries
library(dplyr)
library(ggplot2)

# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  INPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
  
}


### load dataframe
load(paste0(INPUT, "romania.rda"))
source("99_functions.R")



# The variable "Important in life: Family" indicates another strong tie. In order to get an overview of the variable, let us have a look at the distribution. 
barplot_var(imp_fam_fac) +
  labs( title = "Important in life: Family")

# As we can see from the distribution, the vast majority of the  people in Romania (91%) see their family as "very important". This is for sure not a bad thing for the Romanian people, however it is not very helpful for our further analysis, since the distribution is very unequal. We are therefor not sure yet, if we are going to keep this variable in our further analysis, due the the lower power we have in the categories "rather important" , "not very important" and "not important at all".

