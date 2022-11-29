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

# Looking at the variable of activities as a binary "are you a member of any of these organisations" variable seems to make more sense since it splits the dataset into two comparable groups. Let us therefore have a quick look on how this weak tie influences the happiness and the life satisfaction of the Romanians.


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

# This puts the decision to include the variable into perspective since the impact that it has on the responses seems to be rather limited. After consideration, it might be better to omit the variable of activity membership for the case of Romania since the influence seems to be rather limited.        


##### Additional variable: importance of friends
# The next variable we will have a look at is the importance of friends. This can be seen as another weak tie as those who value the friends they have made in a certain area can be expected to be better integrated in their social system and be overall more content with their living situation. Let's start by looking at the overall distribution for the Romanian people:


barplot_var(imp_friends_fac) +
  labs(title ="How important are Friends to you? ") 

# The barplot shows, that most of the romanian respondents seem to place at least a certain bit of importance on their friends. Most of the respondents stated that friends are "rather important" to them. Closely following up is the group for which friends are "very important". Taking these groups together, we can estimate that around 82.22% of the Romanian respondents place at least a certain bit of value on their friends. This leads us to speculate that this variable might be relevant to include in further analysis. However, in order to be sure whether we should consider this variable further, let's look at how the importance of friends impacts life satisfaction and overall happiness


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
# Starting with the impact that the importance of friends has on happiness, we can see that the more important friends get, the larger are the percentages of respondents claiming to be quite or very happy. However, keeping the previous illustration in mind, we need to remember that the group sizes for friend importance were rather asymmetrical, with much more respondents stating that friends would be rather important. The overall theme however still seems to be that the more important friends are to a respondent, the more happy they appear to be. We can observe a similar effect when looking at life satisfaction. The effects previously mentioned can still be seen here, although slightly less pronounced. When comparing the groups it shows that those who consider their friends to be rather or very important also seem to respond being quite or very satisfied with their lives. And although the effects might not be as pronounced as we might have hoped, we still think it pays off to keep this variable in mind when proceeding in the analysis.