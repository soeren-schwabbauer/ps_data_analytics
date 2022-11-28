rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 01.11.2022
# Tasks: 
# Investigate variables and decide if you are going to take them into your furher analysis or not.
# Varibales: 
#   “Number of active associational memberships”,
#   “Number of passive associational memberships”, 
#   “How important in your life: friends and acquaintances”
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
library(spatstat.utils)
library(sjlabelled)
library(ggmosaic)

# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  INPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
  
}

# load dataframe
load(paste0(INPUT, "Romania.rda"))
source("99_functions.R")


#VARIABLLE: MEMBER OF SOMETHING

barplot_var(member_tot_fac) +
  labs(title ="Number of associations one is a member ") 

# As one can see from the distribution of number of memberships, it does not make sense to keep the total number memberships as a variable, since the wide majority is not even in a single organization. Nevertheless, knowing this also helps for the further analysis. We can now add the variable "are you member in at least one organisation?". Doing so includes the variable of activities that respondents may or may not be part of in a more reasonable way. Let us have a look an the distribution then.

# binary variable

barplot_var(member_any_fac) +
  labs(title ="Are you a member in any association")

# we decided to keep the variable "are you a member of any of these organisations". Let us therefore have a quick look on how this weak tie influences the happiness and the life satisfaction of the Romanians.


anymember_happy <-  romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(happy_fac, member_any_fac), fill = happy_fac)) +
  
  labs(title ="Comparison of happiness in life \n and member in any organisation",
       fill = "Scale of happiness") +
  mosaic_theme

anymember_satis <- romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(satisfaction_group, member_any_fac), fill = satisfaction_group)) +
  
  labs(title ="Comparison of satisfaction in life \n and member in any organisation",
       fill = "Scale of satisfaction") +
  mosaic_theme

grid.arrange(anymember_happy, anymember_satis, nrow = 1)

# The impact that being a member of any organisation has on life satisfaction or happiness of the romanian population is rather similar. For both cases we see a visible, but not overwhelming increase of respondents that report to be "very happy". For all else, there doesn't seem to be a large change in responses.

# [UP FOR DISCUSSION]
# This puts the decision to include the variable into perspective since the impact that it has on the responses seems to be rather limited.            









##### Additional variable: importance of friends
# kurzes Vorwort
barplot_var(imp_friends_fac) +
  labs(title ="Number of associations one is a member ") 
# INTERPRETATION


# kurzes vorwort zum mosaic plot
imp_friends_happy <-  romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(happy_fac, imp_friends_fac), fill = happy_fac)) +
  
  labs(title ="Comparison of happiness in life \n and member in any organisation",
       fill = "Scale of happiness") +
  mosaic_theme

imp_friends_satis <- romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(satisfaction_group, imp_friends_fac), fill = satisfaction_group)) +
  
  labs(title ="Comparison of satisfaction in life \n and member in any organisation",
       fill = "Scale of satisfaction") +
  mosaic_theme

grid.arrange(imp_friends_happy, imp_friends_satis, nrow = 1)
# INTERPRETATION