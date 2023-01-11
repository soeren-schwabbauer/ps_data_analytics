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
# With satisfaction and freedom, we have two qualitative variables. For simplicity and due to the lack of a larger dataset, we would like to stick with the groups which we have created in the previous sections of this report. Let's have a look at the absolute frequencies in our data. 
tab <- xtabs(~ freedom_group + satisfaction_group, data = romania)
tab
#In this table we can see, that the number of observations is indeed very small for the not satisfied at all group. Nevertheless, we can check, if there is a dependency of the level of freedom of choice and control on the satisfaction, meaning: Are people who believe their live to be free and who are believe to have more control in live, on average more satisfied in life?
# We are going to perform a chi-square test. Our hypothesis shall be:
# H0: The level of satisfaction does not depend on the amount of freedom of choice and control
# H1: The level of satisfaction does depend on the amount of freedom of choice and control somebody believe to have in life.
# We are going to test at a 5% level of significance.
chisq.test(tab, correct = FALSE)
# With a p-value of very close to 0, and a rather large value for the Chi^2 test statistic, we can conclude, that there is a relationship between the two categorical variables. 


#### satisfaction ~ health
# With the health variable, we have a variable which is, just like the freedom variable qualitative and we are therefor going to perform a chi-square test, with first looking at the absolute distribution.
tab <- xtabs(~ health + satisfaction_group, data = romania)
tab
# Our null-/ and alternative hypothesis shall be: 
# H0: The status of health has no effect on the level of satisfaction
# H1: The status of health indeed has an effect on the level of satisfaction
# and we are going to test at a 5% level of significance.
chisq.test(tab, correct = FALSE)
# We again retrieve a p-value very close to zero and a rather large test statistic. This leads us to the conclusion, that we can reject the null-hypothesis and assume, that the status of health as an impact on the level of satisfaction. 

#### satisfaction ~ educ
# As an additional variable, we chose education. Hereby, we also decided to group education into three categories:
unique(romania$educ_group)
# Let us have a look at the distribution of the qualitative dependent variable education and the qualitative explanatory variable life satisfaction.
tab <- xtabs(~ educ_group + satisfaction_group, data = romania)
tab
# In a next step, we shall find out, if the level of education has an effect on the satisfaction in life. We are going to perform a chi-square test. Testing at a 5% level of significance, our null -& alternative hypothesis are going to be:
# H0: The level of education has no effect on the satisfaction in life.
# H1: The level of education indeed has an effect on the satisfaction in life.
chisq.test(tab, correct = TRUE)
# With a p-value close to zero, we can reject the null hypothesis. This allows us to conclude, that the level of education has an impact on the life satisfaction. However, we would like to note, that this only implies a correlation. We believe, that the actual causation is biased through other factors. Since usually a higher level of education also implies a higher level of income. Money may not make somebody happier, however a sufficient income can guarantee a decent standard of living and therefor increase satisfaction in life. In order to test this hypothesis, we would have to run a multiple linear regression. 


#### Simple linear regression model

lm <-  lm(satisfaction_group ~ freedom_group + health + educ_group, data = romania)


