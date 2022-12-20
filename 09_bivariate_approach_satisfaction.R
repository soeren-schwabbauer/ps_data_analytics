rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 19.12.2022
# Tasks:  Quantify the relationship between “life satisfaction” and “control over life”, “health” and your variable of interest with an appropriate bivariate statistic measure and a corresponding test for statistical significance
# Variables: satisfaction, freedom, health, education
################################################################################

# load libraries
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(gridExtra)
library(tidyverse)
library(janitor)

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


#### satisfaction ~ freedom
# With satisfaction and freedom, we have to qualitative variables. For simplicity and due to the lack of a larger dataset, we would like to stick with the groups which we have created in the previous sections of this report. Let's have a look at the absolute frequencies in our data. 
sum_satisfaction <- romania %>% group_by(satisfaction_group) %>%
  summarise(sum = n()) %>% t() %>% row_to_names(row_number = 1)  %>% as_tibble() %>% mutate('freedom_group' = "sum")
rowid_to_column(sum_satisfaction, var = "test")
sum_freedom <- romania %>% group_by(freedom_group) %>%
  summarise(sum = n()) %>% data.frame() 
freq <- romania %>% group_by(freedom_group, satisfaction_group) %>%
  summarise(n = n()) %>%
  data.frame() %>%
  spread(satisfaction_group, n) %>%
  left_join(sum_freedom, by = c('freedom_group'))

bind_rows(freq, sum_satisfaction)

  crossta
# As we can see from the table, the value "not satisfied at all" is rather poorly supplied with data. 

ggplot(data = romania, aes(freedom_fac, satisfaction)) +
         geom_boxplot() +
  geom_smooth(stat = "smooth")

romania$health


#### satisfaction ~ educ

romania$educ_group
