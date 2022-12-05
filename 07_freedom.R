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



# An additional variable, that can influence life satisfaction and happiness in life is "How much freedom of choice and control" somebody has. The authors of the study describe the question as follows: some people feel they have completely free choice and control over their lives, and other people feel that what they do has no real effect on what happens to them. Please use the scale to indicate how much freedom of choice and control you feel you have over the way your life turns out?

# In order to get a first overview, let us have a look at the distribution of the variable.
bar_freedom <- barplot_var(freedom_fac)
box_freedom <- boxplot_for_barplot(freedom) +
  labs(title = "How much freedom of choice and control do you think you have?") 
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

# Looking at this distribution, we can observe that most of the respondents seem to feel at least a little in control of how their lives turn out. This is something we need to keep in mind when looking at the correlations between this feeling of control and our dependent variables.


# Let us now look if we can find some associations between life satisfaction or happiness in life together with the perceived control that citizens feel they have in their lives. 


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

# From looking at the mosaic plot, showing the relation between perceived freedom and happiness, a trend that we can observe is that the more people feel in control of their lives, the happier they tend to be. As previously mentioned we need to keep in mind that for those who feel not so much in control of their lives (group 1) we have a rather small sample size. This could be the reason why the trend that we can observe in groups 2 and 3 is not so much reflected in group 1. Looking at group 4 in particular, the trend also seems to be interrupted. However we can observe a higher percentage of respondents who feel like they have a lot of influence on their lives who also claim to be very happy as compared to groups 2 or 3.

# Analyzing the right graph which compares satisfaction in life to freedom of choice and control, we notice a much clearer trend across the board. We could generally say that the more the Romanian people feel in control of their lives, the more satisfied they were. It needs to be noted, that especially in the group that feels not so much in control (group 1) we are working with a rather small sample size. However, this does not diminish the trend otherwise observed in the dataset and leads us to the conclusion that we will consider this variable further.

