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
library(lemon)

# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  INPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"

}

# load dataframe
load(paste0(INPUT, "Romania.rda"))
source("99_functions.R")


################################################################################
###### 1. explore main variables (UNIVARIATE ANALYSIS) #########################

# A170 satisfaction in life ####################################################
# summary for quick overview
summary(romania$satisfaction)
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

grid.arrange(boxplot_for_barplot(satisfaction) +
               labs(title = "Overall life satisfaction"), 
             
             barplot_var(satisfaction_fac) , 
             
             nrow = 2, heights = c(0.5,2))
# Interpreation: The barplot on the distribution of satisfaction in life adds additional
# information to the summary command. As one can see, with the highest level "satisfied"
# the most people can identify. It remains unclear, why there is such a rapid drop one level below.
# Furthermore it can be seen, that the lower 25% are much more distributed (from 2 to 5), 
# whereas "Satisfied" contains all the upper 25% of the counts. 


### A008 Feeling of happiness ##################################################

# summary for quick overview
summary(romania$happy)
# Also the variable happy only contains interpretable values. Neither has nobody not answered,
# nor has the question being skipped, other other problems occured. The varibale is scaled from one to
# 4. One indicates "very happy", 4 indicates "not at all happy". Just like with 
# satisfaction, the feeling of happiness counts as a orindally scaled variable.

# the following barplot allows a closer look at the distribution of happiness within
# the questioned people in romania
barplot_var(happy_fac) +
  labs(title = "Overall feeling of happiness")
# From the barplot one can conclude, that most people (approx. 56%) are "quite happy",
# one fifth of the questioned population are "very happy" and roughly the same amount of people are "not very happy", or 
# "not at all happy"



################################################################################
###### 2. MULTIVARIATE ANALYSIS ################################################

### income #####################################################################
summary(romania$income_scale)
# The variable income was evaluated by showing the questioned household a range incomegroups 
# within their country. The participants then had to match themself to the incomegroup.
# The income then got scaled from 1 (the 1st decile) and 10 (the highest decile).

# The following two plots allow an insight in the income distribution of Romania
grid.arrange(boxplot_for_barplot(income_scale) +
               labs(title = "Income distribution") , 
             
             barplot_var(income_scale_fac), 
             
             nrow = 2, heights = c(0.5,2))
# It can be seen, that the distribution is left skewed. This indicates, that the vast 
# majority belongs to a lower income group.
# The boxplot shows, that 50% of the people lay in between the 3rd and the 5th decile.
# 

### mosaic plots
grid.arrange(romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(happy_fac, income_scale_fac), fill = happy_fac)) +
               labs(title ="Comparison of happiness in life and income",
                    fill = "Scale of happiness") +
               mosaic_theme,
             
             romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(satisfaction_group, income_scale_fac), fill = satisfaction_group)) +
               labs(title ="Comparison of satisfaction in life and income",
                    fill = "Scale of satisfaction") +
               mosaic_theme,
             
             nrow = 1)
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
grid.arrange(boxplot_for_barplot(health) +
               labs(title = "Health distribution"),
             
             barplot_var(health_fac) , 
             
             nrow = 2, heights = c(0.5,2))
#The average citizen seems to be in rather good health condition. Most of the answers
#group around the "good" health answer. The second most frequent answer is the
#"fair" health response. A very small percentage of respondents report being of
#"poor" or "very poor" health. 
median(romania$health)
#This is supported by the median value of the "health" variable which confirms,
#that the average citizen is of Good health

### health mosaic plots
grid.arrange(romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(happy_fac, health_fac), fill = happy_fac)) +
               labs(title ="Comparison of happiness in life and health",
                    fill = "Scale of happiness") +
               mosaic_theme,
             
             romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(satisfaction_group, health_fac), fill = satisfaction_group)) +
               labs(title ="Comparison of satisfaction in life and health",
                    fill = "Scale of happiness") +
               mosaic_theme,
             
             nrow = 1)

# INTERPRETATION
# These graphs show a connection between health of an individual and the satisfaction
# with life and overall happiness. Looking at happiness in life first, what stands
# out is, that those with poor and very poor health are to a large extent not very
# happy. This also translates to overall life satisfaction. Those with poor health 
# are to a large extent not very satisfied.
#
# The opposite to this observation also holds true. Those with good and very good
# health are to a very large extent either very happy or quite happy. Respectively,
# those with good and very good health are also quite satisfied and very satisfied.

### age ########################################################################
 
 ##overall overview
summary(romania$age)
# The summary command tells us the mean and the median age. Bothe are around 50.
# With the mean and the median age laying close together, one can say, that 
# the distribution is rater equal. 
# Since the age scaling end at the age of 82, one can not say, how old the oldest person
# in the dataframe is. The youngest however is at the age of 19.

grid.arrange(boxplot_age(happy_fac, age, sex_fac) +
               labs(title = "Range of age for the level of happiness",
                    fill = "age"),
             
             boxplot_age(satisfaction_fac, age, 0) +
               labs(title = "Range of age by satisfaction") +
               theme(legend.position = "none"), 
             
             nrow = 1)
# The boxplots show the distribution of age within the different levels of happiness
# and satisfaction. On the left graph it can be seen, that the median age increases, 
# the less satisfied a person is. 
# In the left graph it can be seen, that people who are "not happy at all" tend to be 
# much older by median, than people who are very happy.
# For a less biased picture, we decided to split the level of happiness into male 
# and female. It can be seen, that this does not make so big of a difference. However
# a switch in the median age is observable, the less happy a person gets.


romania %>% 
  group_by(age, sex_fac) %>%
  summarize(n = n()) %>% 
  mutate(age = as.character(age),
         sex = as.character(sex_fac),
         n = as.integer(n)) %>%
  
  ggplot( mapping = aes(x = ifelse(test = sex_fac == "Male", yes = -n, no = n), 
                        y = age, fill = sex)) +
  geom_col() +
  scale_x_symmetric(labels = abs) +
  labs(x = "Population ",
       title = "Age distribution in the sample") +
  scale_y_discrete(breaks = c(19, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 82)) +
  
  theme(
    plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "grey") 
  )

### population pyramid
romania %>% 
  group_by(age, sex_fac) %>%
  summarize(n = n()) %>% 
  mutate(age = as.character(age),
         sex = as.character(sex_fac),
         n = as.integer(n)) %>%
  
  ggplot( mapping = aes(x = ifelse(test = sex_fac == "Male", yes = -n, no = n), 
                        y = age, fill = sex)) +
  geom_col() +
  scale_x_symmetric(labels = abs) +
  labs(x = "Population ",
       title = "Age distribution in the sample") +
  scale_y_discrete(breaks = c(19, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 82)) +
  
  theme(
    plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "grey") 
  )
#######################################################################################

 #The visualisation of age distribution overall and separated by sex supports
 #the insight, that there is no significant difference or trend in age distribution.




### marital status #############################################################
barplot_var(marst_fac) +
  labs(title = "Marriage distribution")

# Since it is not possible, to put the marital status in some kind of hierarchy, 
# it can be considered a nominal scaled variable.
# The graph shows the distribution of people regarding their legal marital status.
# It can be seen, that the wide majority (60%) is married. The second and third highest population
# is formed by people who are single or have never been married (19%), and people who are
# widowed (15%).

# The following mosaic plots are an attempt of a causal link between the marriage status, 
# happiness and life satisfaction 
romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(happy_fac, marst_fac), fill = happy_fac)) +
  
  labs(title ="Comparison of happiness in life and Marital status",
       fill = "Scale of satisfaction") +
  mosaic_theme
# It can be seen, that the widowed people are generally the ones who are rather not, or not happy at all
# The single and married people are generally the biggest peopulation of people who are 
# "very happy". 

romania %>%
  ggplot() +
  geom_mosaic(aes(x =  product(satisfaction_group, marst_fac), fill = satisfaction_group)) +
  
  labs(title ="Comparison of satisfaction in life and income",
       fill = "Scale of satisfaction") +
  mosaic_theme
# The distribution of "very satisified" people is quiet similar to the one seen in the
# previous plot. The same counts for the "not very" or "not satisfied at all", with widowed
# people making up the largest population.




### additional variables #######################################################
bind_rows( activity_fun(member_activity, "Member of an activity"),
           activity_fun(member_religion, "Member of a religion"),
           activity_fun(member_selfhelp, "Member of a selfhelp group"),
           activity_fun(member_charity, "Member of a charity"),
           activity_fun(member_labor_union, "Member of a labor union")) %>%
  
  group_by(member, cat, happy) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n)) %>%
  
  ggplot(aes(x = as_factor(member), y = perc, fill = as_factor(happy))) +
  geom_bar(stat = "identity") +
  labs(fill = "Level of Happiness",
       y = "Distribution (%)") +
  
  theme(
    axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.x = element_text(angle=90),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                      colour = "grey") ,
    plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5) 
    
  ) +
  
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  
  facet_grid( ~ cat) 

# Interpretation
# These additional variables also allow a little bit of insight into the data.
# Starting off with charitable causes we can see that those who mentioned being 
# a member of a charitable organisation reported to be quite happy or very happy 
# more often.
#
# Being a member of a labor union also seems to have a certain connection to
# happiness as union members more often reported to be quite or very happy as 
# opposed to non-members.
#
# Interestingly, being a member of a religious organisation doesn't seem to 
# increase happiness for all participants. It can be observed that those who are
# religious more often reported to be not very happy but at the same time 
# religious respondents more often reported to be very happy. One way of 
# interpreting this would be to assume that being religious could have a diverging
# effect on people and either increase or decrease their happiness according to 
# some other external variable.
#
# Looking at the variable of being a member of a self-help group, the graphs don't
# diverge as much as for other variables. Those who are members of a self-help
# group have a little more frequently reported to be quite happy than those who are
# not, but the effect is very small.
#
# Finally, being a member of an activity. This seems to be connected to the effect
# of happiness as those who have reported to be member of any sort of activity 
# rarely mentioned to be not very happy or not at all happy. Over 90% of respondents
# who mentioned to be member of an activity reported to be either very or at least
# quite happy.
