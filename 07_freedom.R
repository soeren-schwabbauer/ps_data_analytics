rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 28.11.2022
# Tasks: As an additional important variable for life satisfaction please look at “How much freedom of choice and control”, i.e. A173. Do you find evidence for a strong association between life satisfaction or happiness and this variable
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



# An additional variable, that can influence life satisfaction and happiness in life is "How much freedom of choice and control" somebody has. The authors of the study describe the question as follows: ome people feel they have completely free choice and control over their lives, and other people feel that what they do has no real effect on what happens to them. Please use the scale to indicate how much freedom of choice and control you feel you have over the way your life turns out?

# In order to get a first overview, let us have a look at the distribution of the variable.
bar_freedom <- barplot_var(freedom_fac)
box_freedom <- boxplot_for_barplot(freedom) +
  labs(title = "How much freedom of choice and control") 
grid.arrange(box_freedom, bar_freedom, nrow = 2, heights = c(0.5,2))
# From looking at the boxplot we can see, that the median of the distribuiton is at the level of 8. Most people (32%) see themself to be very free, by answering at the highest level possible. almost 50% of the distribution chose a level the level of 9. The additional 50% of the remaining answers are distibuted in a declining distibution from 8 to 1. The lower levels, especially 1 to 4) may not have a lot of observations, however we do have quiet some oversvations for the upper levels. We therefor decided to bundle the variable as the follows: 

#  | **id**  | **variable**          |
#  |---------|-----------------------|
#  | 1-4     | not much       |
#  | 5-7    | a little     |
#  | 8-9    | rather agree    |
#  | 10    | A great deal     |

# This leaves us with the following distribution:
summary(as_factor(romania$freedom_group))
barplot_var(freedom_group) +
  labs(title = "How much freedom of choice and control - by groups")
# Let us now look if we can find some associations between lifesatisfaction or happiness in life together with the freedom in life. 
freedom_happy <-  romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(happy_fac, freedom_group), fill = happy_fac)) +
  
  labs(title ="Comparison of happiness in life \n and freedom of choice and control",
       fill = "Scale of happiness") +
  mosaic_theme

freedom_satis <- romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(satisfaction_group, freedom_group), fill = satisfaction_group)) +
  
  labs(title ="Comparison of satisfaction in life \n and freedom of choice and control",
       fill = "Scale of satisfaction") +
  mosaic_theme

grid.arrange(freedom_happy, freedom_satis, nrow = 1)

# From looking at the stacked barplot showing the realtion between happiness and freedom of choice and control we can
# see that: 
# - a very large portion of persons rate their happiness as "very happy" and "quite happy" group 3&4 more than 1&2
# - looking at the "very happy" persons we can see that there is a trend that more people are "very happy" the
# more control and freedom they possess in their lives, except for the persons in group 1 "not much" freedom are
# almost equally "very happy" as grouß 3 which "rather agrees" with freedom of choice and control. Looking at the
# "quite happy" portion of the graph there is no real trend as grouß 2&3 have more "quite happy" mentions than for 
# for example group 4 where it rather would be expected. 
# In the "not very happy" and "not at all happy" portions we cannot see a clear trend.

# Analyzing the right graph which compares satisfaction in life with freedom of choice and control we notice:
# here a much clearer trend across the board, we could generally say that the more freedom of choice and control
# where perceived the more satisfied the persons where. The small irregularities in the very satisfied portion
# where group 1 " not much" freedom was more "very satisfied" or in the portion "not satisfied at all" in group 1 &3
# could be explained by samplesize, as the median was mentioned to be 8 so not a lot of sample size at least in 
# "not much" and "a little" freedom of choice and control. 

# We should further do correlation test and significance tests to show if trends are significant or not due to 
# sample size or variance