rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 01.11.2022
# Tasks: 
# Try to find appropriate components for the political interest and attitude dimension, e.g. run a PCA using the variables “Interested in politics”, “Satisfaction with the political system”, “Having a democratic political system”, “Importance of democracy”, ..
################################################################################

# load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(webr)
library(patchwork)
library(reshape2)
library(factoextra)



# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  INPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
  
}

# load dataframe
load(paste0(INPUT, "Romania.rda"))


#possible variables for political interest:
pc.politics <- romania %>% 
  select(intrests_politics, politics_satisfaction, politics_democracy, importance_democracy, politics_petition,
       #  politics_boycott,
      #   politics_demo,
      #   politics_strikes,
  ) %>%
  
  prcomp( scale = TRUE)

fviz_eig(pc.politics, addlabels = TRUE)
