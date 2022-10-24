rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 13.10.2022
# Purpose: 1) explore main variables
#          2) compare main variables to other variables
################################################################################

# load libraries
library(dplyr)
library(haven)
library(ggplot2)
library(ggmosaic)
library(lazyeval)
library(gridExtra)
library(ggmosaic)

# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/02_analysis/INPUT/"
  OUTPUT = "G:/Geteilte Ablagen/data_analytics/02_analysis/OUTPUT/"
  CODEBOOK = "G:/Geteilte Ablagen/data_analytics/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  
  INPUT = "G:/Shared drives/data_analytics/02_analysis/INPUT/"
  OUTPUT = "G:/Shared drives/data_analytics/02_analysis/OUTPUT/"
  CODEBOOK = "G:/Shared drives/data_analytics/"
  
}

# define theme for mosaic ######################################################
theme_mosaic <- theme( 
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

# define function, incl. theme for barplots ####################################

barplot_variable <- function(var){
  romania %>%
    ggplot(aes(x= {{var}}, y= ..count.. , fill = {{var}})) +
    geom_bar() +
    labs(x = "",
         y = "total number") +
    geom_text(stat = "count", aes(label= paste0(..count.., " (", round(..count../nrow(romania),4)*100 , "%)")), vjust = -0.5) +
  
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey") 
    )
}


boxplot_variable <- function(var){
  romania %>%
    ggplot(aes(y = as.numeric({{var}}))) +
    geom_boxplot(width = 0.5) +
    coord_flip() +
    
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"),
      
      
    )
  
  
}

################################################################################


# load dataframe
load(paste0(INPUT, "Romania.rda"))

################################################################################
###### 1. explore main variables (UNIVARIATE ANALYSIS) #########################

# A170 satisfaction in life ####################################################
# summary for quick overview
summary(romania$satisfaction_life)
# Interpreatition: 
# The summary command allows us to have a rough overview of the variable A170 - satisfaction in life.
# The variable is scaled from 1 to 10, with 10 being the highest level of satisfaction.
# Since one can rank the level of satisfaction (e.g: a level of 8 being higher ranked than
# a level 2), but the distnace between the levels is subjective (e.g: a person on a level 8
# is not necesearily twice as happy as a person on the level 4) Life satisfaciton can be considered
# an ordinally scaled varibale. 

# The summary command shows, that everybody had replied to to questions (no NA's).
# Another conclusion which can be drawn from the summary command is the average level of
# satisfaction. With a median level of 8, and a mean of 7.5, the Romanian people are generally
# not extremely satisfied, but also not unsatisfied with their life as a whole. 
# In addition to that, it can be seen, that the half of the aksed population ranks themself
# between a 6 and a 9.

# The following plot allows a look at the disrtibution of satisfaction
bar_satiscation <- barplot_variable(satisfaction_life_fac) 

box_satiscation <- boxplot_variable(satisfaction_life) +
  labs(title = "Overall life satisfaction") 

grid.arrange(box_satiscation, bar_satiscation, nrow = 2, heights = c(0.5,2))
# Interpreation: The barplot on the distribution of satisfaction in life adds additional
# information to the summary command. As one can see, with the highest level "satisfied"
# the most people can identify. It remains unclear, why there is such a rapid drop one level below.
# Furthermore it can be seen, that the lower 25% are much more distributed (from 2 to 5), 
# whereas "Satisfied" contains all the upper 25% of the counts. 


### A008 Feeling of happiness ##################################################

# summary for quick overview
summary(romania$feel_happy)
# Also the variable happy only contains interpretable values. Neither has nobody not answered,
# nor has the question being skipped, other other problems occured. The varibale is scaled from one to
# 4. One indicates "very happy", 4 indicates "not at all happy". Just like with 
# satisfaction, the feeling of happiness counts as a orindally scaled variable.

# the following barplot allows a closer look at the distribution of happiness within
# the questioned people in romania
barplot_variable(feel_happy_fac) +
  labs(title = "Overall feeling of happiness")
# From the barplot one can conclde, that most people (approx. 56%) are "quite happy",
# one fifth of the questioned population are "very happy" and roughly the same amount of people are "not very happy", or 
# "not at all happy"



################################################################################
###### 2. MULTIVARIATE ANALYSIS ################################################
# comparing two quantitative variables by mosaic plot
romania$satisfaction_life_fac <- droplevels(romania$satisfaction_life_fac)
romania$income_scale_fac <- droplevels(romania$income_scale_fac)
romania$feel_happy_fac <- droplevels(romania$feel_happy_fac)

### income #####################################################################
summary(romania$income_scale)
# The variable income was evaluated by showing the questioned household a range incomegroups 
# within their country. The participants then had to match themself to the incomegroup.
# The income then got scaled from 1 (the 1st decile) and 10 (the highest decile).

# The following two plots allow an insight in the income distribution of Romania
bar_income <- barplot_variable(income_scale_fac)
box_income <- boxplot_variable(income_scale) +
  labs(title = "Income distribution") 
grid.arrange(box_income, bar_income, nrow = 2, heights = c(0.5,2))
# It can be seen, that the distribution is left skewed. This indicates, that the vast 
# majority belongs to a lower income group.
# The boxplot shows, that 50% of the people lay in between the 3rd and the 5th decile.
# 

lorenz <- romania %>%
  group_by(income_scale) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n),
         cum_perc = cumsum(perc))

cum_perc <- as.vector(lorenz$cum_perc)
  
ggplot(lorenz, aes(x = cum_perc, y = income_scale/10)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  
  scale_x_continuous(labels = scales::percent,
                     breaks = cum_perc) +
  
  labs(title = "Lorenz curve of Romania",
       x = "Cumulative houshold number",
       y = "Cululative total income (%)") +
  
  theme(
    axis.text.x = element_text(angle=90, vjust = 0.4),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "grey") ,

  )
# The Lorenz curve underlines the statement of a rather unequal distributet income.
# For further analysis, a comparison between other countries would have to be considered.


### mosaic plots
inc_happy <-  romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(feel_happy_fac, income_scale_fac), fill = feel_happy_fac)) +
  
  labs(title ="Comparison of happiness in life and income",
       fill = "Scale of satisfaction") +
  theme_mosaic
  
class(as.numeric(romania$satisfaction_life))
inc_satis <- romania %>%
  mutate(satisfaction_group = case_when(as.numeric(satisfaction_life) == 1  ~ "Not satisfied at all",
                                        as.numeric(satisfaction_life) == 2 |
                                          as.numeric(satisfaction_life) == 3 |
                                          as.numeric(satisfaction_life) == 4 |
                                          as.numeric(satisfaction_life) == 5  ~ "Not very satisfied",
                                        as.numeric(satisfaction_life) == 6 |
                                          as.numeric(satisfaction_life) == 7  |
                                          as.numeric(satisfaction_life) == 8 |
                                        as.numeric(satisfaction_life) == 9 ~ "Quiet satisfied",
                                          as.numeric(satisfaction_life) == 10  ~ "Very satisfied")) %>%
  ggplot() +
  geom_mosaic(aes(x =  product(satisfaction_group, income_scale_fac), fill = satisfaction_group)) +
  
  labs(title ="Comparison of satisfaction in life and income",
       fill = "Scale of satisfaction") +
  theme_mosaic

grid.arrange(inc_happy, inc_satis, nrow = 1)
### The two mosaic plots allow a bivariate exploratory analysis among the variables
# income and happiness, as well as income and satisfaction.
# The comparison between income and satisfaction in life shows, that the group with the lowest income 
# is the group with the second highest "very happy" people. However the people in the 2nd decile are.
# Grouping the happiness variable and saying, that "quite happy" and "very happy"
# can be grouped, the lower three deciles make up the lowest share. 
# This picture is opposite, if one looks at the top 30% of the income scale. Among theses groups
# almost everybody ist at least "quiet happy". 

# For a better overview, we grouped the levels of satisfaction into 1 = "Not satisfied at all,
# 2-5 = "not very satisfied", 6-9 = "quiet satisfied" and 10  = very satisfied.
# Comparing the satisfaction and the income, the group with the lowest income, again has one 
# of the highest rates of satisfaction. Tough it should also be mentioned, that they also share
# one of the highest rates of Dissatisfaction. Once again it shows, that a higher income
# rules out the chance of being "not satisfied at all". At the same time, the top 10% income
# group accounts for the lowest amount of very satisfied people. 


### health #####################################################################
barplot_variable(health_fac) +
  
  labs(title = "Health distribution")
## INTERPRETATION: 
##The average citizen seems to be in rather good health condition. Most of the answers
##group around the "good" health answer. The second most frequent answer is the
##"fair" health response. A very small percentage of respondents report being of
##"poor" or "very poor" health. 
median(romania$health)
##This is supported by the median value of the "health" variable which confirms,
##that the average citizen is of Good health


### age ########################################################################
 
 ##overall overview
summary(romania$age)
sd(romania$age)
 #The range of the data is 63 years with the youngest respondent being 19 and the oldest
 #being 82 years old. The average respondent is around 50 years old.

barplot_variable(romania$age)
 #The barplot doesn't reveal any significant peaks within the population. There are a 
 #couple values which appear more frequent than others but none of them qualifies as 
 #an absolute peak


 ##Looking closer into the distribution of age
 ###Making a copy Dataset so I don't screw anything up
data <-  romania
data$age <-  as.numeric(data$age)

male <-  subset(data, data$sex == 1)
female <-  subset(data, data$sex ==2)

par(mfrow=c(1,2))
hist(female$age)
hist(male$age)
mean(male$age)
mean(female$age)
 #The visualisation of age distribution overall and separated by sex supports
 #the insight, that there is no significant difference or trend in age distribution.


### marital status #############################################################
barplot_variable(marst_fac) +
  
  labs(title = "Marriage distribution")
## INTERPRETATION: 

### additional variables #######################################################
