rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 28.11.2022
# Tasks: Pick one variable you think may be important and you are interested in. Argue for your chosen variable. Provide some descritptive statistics for it as well.
# Variables: education
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

# Education can lay the foundation for all future life. It's imporatance can not only dirctly affect the income when starting to work, but also the number of children, the age of the first birth of the child, and finally, we would like to find out, if we find evidence, for the level of education having influence on the happiness and life satisfaction. Let us therefor start with an overview on the variable and its distribution. 
barplot_var(educ_fac)
# We can see, that almost half of the Romanians in our sample have a upper secondary degree. Which, according to our research is equivalent with a high school degree. In order to clearly identify groups, we would like to group the variable as follows:
#  | **level of education **  | **group**          |
#  |---------|-----------------------|
#  | Less than primary    | Less than High School      |
#  | Primary    | Less than High School     |
#  | Lower secondary    | Less than High School    |
#  | Upper secondary    | High School Education   |
#  | Post-secondary non tertiary   | High School Education     |
#  | Short-cycle tertiary    | High School Education     |
#  | Bacheloror equivalent    | College Education     |
#  | Master or equivalent    | College Education     |
# This leaves us with the following groups:
barplot_var(educ_group)

# We shall now compare the impact of happiness and life satisfaction on the education variable.

#The variable we have chosen for this exercise is the one of marital status subject to the gender of a given respondent. The reason for this is, that the way a respondent lives at home (in a partnership or by oneself) is an important indicator of their lifestyle and can be expected to influence their happiness or satisfaction to a certain extent. However, there are limitations to this variable. We have recoded the original data and grouped the responses for marital status into two broad categories: Living in a partnership or by oneself. The original data does not contain a response for non-official partnerships which would be an interesting information to have. Furthermore we cannot tell from this data how much value a given respondent places on their partnership. Nonetheless, the following barplot displays the distribution of marital status subject to gender of a respondent.

barplot_var(romania$sex_married)

#We can see, that generally there are more people in our dataset that live in a relationship. This is true for male and female respondents alike. However,the distribution is not too asymmetrical, which is why the following plots will examine the impact of marital status on happiness and satisfaction.


grid.arrange(romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(happy_fac, educ_group), fill = happy_fac)) +
               
               labs(title ="Comparison of happiness in life \n depending on level of education",
                    fill = "Scale of happiness") +
               mosaic_theme, 
             
             romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(satisfaction_group, educ_group), fill = satisfaction_group)) +
               
               labs(title ="Comparison of satisfaction in life \n depending on level of education",
                    fill = "Scale of satisfaction") +
               mosaic_theme,
             
             nrow = 1)

# It seems like, the distribution among the very happy and very satisfied people is relativly constant across the three levels of education. However, we can find an incline in the not very satisfied/happy people towards a lower level of education.


#Starting with happiness in life, we can observe a slight increase in the frequency of responses for "quite" or "very" happy. The main difference seems to be between the two groups "partnership" and "no partnership". The difference between the two genders is rather small. The main increase happens in the percentage of respondents claiming to be "quite happy" with their situation.

#For satisfaction, the picture is a lot less clear. It is almost impossible to observe a trend within the plot, as for instance as for instance the female respondents without a partnership seem to behave rather similarly as the male respondents living in a partnership. 

# Since we would expect that marital status has a uniform effect on happiness or satisfaction and the effect on happiness mentioned earlier is not overwhelmingly large, we decided to not include this variable in further analysis.




