rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 19.12.2022
# Tasks:  Choose an appropriate statistical test and investigate whether “life satisfaction” differ with respect to “gender”.
# Variables: satisfaction_life, sex_fac
################################################################################

# load libraries
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(gridExtra)
library(tidyverse)
library(coin)

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


 # note: find out, if the two groups differ significantly

# In the following, we shall find out, if the life satisfaction of the participants differs significantly across the two genders. Let us first have a look at a summary table in order to get a clearer picture.
romania %>% group_by(sex_fac) %>%
  summarise(n = n(),
            mean = mean(satisfaction),
            median = median(satisfaction),
            sd = sd(satisfaction),
            iqr = IQR(satisfaction),
            min = min(satisfaction),
            max = max(satisfaction)) %>% data.frame(row.names = 1) %>% t() %>% round(2)

# We can see, that there are 255 male and 302 female participants in the sample. While to outlier robust median is identical for both groups, they seem to differ in the means. The standard deviation, the minimum & maximum value, as well as the interquartile range seem to be identical. Let us now visualize the two groups in a boxplot, including the mean as a red dot. 
grid.arrange(
  #barplot, male
  barplot_var_gender(satisfaction_fac, "Male") + scale_y_reverse() + coord_flip(),
  # boxplot
  romania %>%
  ggplot(aes(x = sex_fac, y = satisfaction)) +
  geom_boxplot() +
  geom_point(aes(y = mean(satisfaction)), col ="red")+ 
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(axis.title.x = element_blank(),
        axis.title.y= element_blank(),
        axis.line = element_blank(),
        panel.grid  = element_blank(),
        axis.text.y = element_blank()),
  #barplot female
  barplot_var_gender(satisfaction_fac, "Female") + coord_flip() , ncol = 3)
# The boxplots together with the distribution may explain the differentiating means. Within the male subset, there are more people rating their level of satisfaction on a 5 or a 4, than the female population does. 
# With the help of a Wilcoxon-Mann test, we shall find out, whether the mean satisfaction of the two genders differs significantly. Our null hypothesis and the alternative hypothesis shall be the following:
# H0: The mean of the male and female population does not differ
# H1: The mean of the male and female population differs
# We are going to test at a 5% level of significance.
#WE CHOSE A WILCOXON TEST BECAUSE
wilcox_test(satisfaction ~ sex_fac, data = romania, var.equal = TRUE)
# The output leaves us with a p-value of 0.6954. This implies, that we cannot reject the Null hypothesis and assume, that the means among the male and female population does not differ significantly.

#BECAUSE A LIKERT SCALE CAN ALSO BE INTERPRETED AS A CATEGORICAL VARIABLE WE ALSO CONDUCTED...
t.test(romania$satisfaction ~ romania$sex_fac)
#INTERPRETATION 
