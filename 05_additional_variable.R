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

####################################################################
mosaic_theme <-   theme( 
  #legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_text(angle=90),
  panel.background = element_rect(fill = "white"),
  panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                    colour = "white") ,
  legend.position = "none",
  plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5)
) 
####################################################################


# explore variables
attach(romania)

activity_df <- romania %>% 
  select(contains("member_")) %>%
  rowwise() %>%
  mutate(tot_member = sum(cur_data())) %>%
  ungroup() #%>% 
 # group_by(tot_member) %>%
 # summarise(n = n())

ggplot(activity_df, aes(x = tot_member)) +
  geom_bar()

# As one can see from the distribution of number of memeberships, it does not make sense to keep the total number memberships as a varible, since the wide majority is not even in a single organization. Nevertheless, knowing this also helps for the further analysis. We can now add the variable "are you member in at least one organisation?". Let us have a look an the distribution then.

romania <- romania %>% 
  
  rowwise() %>%
  mutate(tot_member = sum(member_religion, member_activity, member_labor_union, member_party, member_association, member_sports, member_consumer, member_other, member_charity, member_selfhelp)) %>%
  ungroup() %>%
  mutate(member_any = ifelse(tot_member >= 1, 1, 0))

ggplot(romania, aes(x = member_any)) +
  geom_bar()

# we decided to keep the variable "are you a member of any of these organisations". Let us therefor have a quick look on how this weak tie influences the happiness and the life satisfaction of the romanians.

anymember_happy <-  romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(happy_fac, as.factor(member_any), fill = as.factor(member_any)))) +
  
  labs(title ="Comparison of happiness in life and health",
       fill = "Scale of happiness") +
  mosaic_theme

            


