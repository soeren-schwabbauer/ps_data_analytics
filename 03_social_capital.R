rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 01.11.2022
# Tasks: 
# For the trust dimension of social capital you may use the variable asking whether people can generally be trusted or not, for the weak ties dimension you could use the sum of the variables asking whether one is a member of ....
################################################################################

# load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(webr)
library(patchwork)
library(reshape2)


# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  INPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
  
}

# load dataframe
load(paste0(INPUT, "Romania.rda"))
source("99_functions.R")


##### START REPORT #############################################################

# For the following report, we shall have a look at how the subjective well-being 
# (SWB), namely general happiness and life satisfaction, are linked to Social Capital

# In order to take a closer look at the SWB, we shall take the term of the Social 
# Capital into account. The literature shows that Social Capital is an important 
# factor for increasing the level of SWB. But first of all, what is Social Capital?
#
# Social capital can be seen as "a positive product of human interaction" 
# (investopedia.com). While it can not be owned by somebody, it is the result of 
# connections between individuals within social networks. Social capital can be 
# seen as a base pillar for the effective functioning of social groups. The absence 
# of Social capital can mislead a group of people to not achieve a common goal or purpose.
#
# Despite its undeniable value for any group of individuals, there is no consensus 
# on how social capital can be measured directly. This leads to the conclusion, 
# that social capital can only be expressed through other indicators. The European 
# Value Study (EVS) includes several indicators which can help to determine a 
# countries Social Capital 
#
# In the following, we are going to consider trust as a key dimension for social 
# capital. The EVS sees the trust variable as an indication whether most people 
# can be trusted or not. in particular, the people participating in the survey get 
# asked the following question: Generally speaking, would you say that most people 
# can be trusted or that you can’t be too careful in dealing with people? 
# The answer is then separated into two possible answers (1) "Most people can be trusted" 
# and (2) "Can't be too careful". In the following diagram, we are going to provide 
# an overview of the responses of the qualitative variable.

trust_summary <- romania %>%
  
  group_by(trust_fac) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n/sum(n)*100,2))

trust_plot <- trust_summary %>%
  
  ggplot(aes(x = "", y = prop, fill = trust_fac)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = prop, label = n), color = "black", position = position_stack(vjust = 0.5)) +
  
  labs(title = "Dimensions of trust",
       fill = "Respones to the question") +
  
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "white"),
    plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5)
    )

grid.arrange(trust_plot, tableGrob(trust_summary),  heights = c(2, 0.5))

# As we can see from the pie chart and the table, people in Romania are generally 
# rather careful. The vast majority (approx. 85%) expresses their trust in people 
# on a level of "can't be too careful. This rough overview already gives the 
# impression, that the level of Social Capital is rather low in Romania.

bind_rows(trust_var_summary(happy_fac, "Happiness"),
          trust_var_summary(satisfaction_fac, "Satisfaction")) %>%
  
  group_by(member, cat, trust_fac) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%

  ggplot(aes(x = as_factor(member), y = prop, fill = as_factor(trust_fac))) +
  geom_bar(stat = "identity") +
  labs(fill = "Levels of trust",
       y = "Distribution (%)",
       title = "Impact of the level of trust on happiness and satisfaction") + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle=90),
    #axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                      colour = "grey") ,
    plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5) 
    
  ) +
  
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  
  facet_grid( ~ cat, scales="free_x") 

# Looking at the graph, it is hard to establish a clear trend within the data.
# Starting with the "Happiness" part of the diagram, it can roughly be said, 
# that those who are very or quite happy with their lives tend to trust their
# fellow citizens more on average.
#
# A similar trend cannot really be observed in the case for satisfaction.
# Here, dissatisfied participants seem to trust their fellow citizens more.
# While there are differences in the level of trust, it's not possible to 
# observe a trend.
#
# Furthermore it needs to be stated, that the overall level of trust rarely 
# exceeds 20% with a maximum of 30% being reached only once.


# Besides the trust factor, analyzing "weak ties" can also help to get an 
# impression of the Social Capital among the Romanian people. The term weak 
# ties implies connections among individuals which one does not know very well, 
# but by whom one is surrounded on a regular basis. In the EVS, the interviewers 
# asked the participants weather they are a member of various groups. Let us 
# therefore first have a very general overview on how the membership in any 
# group can impact happiness and or life satisfaction. 
#
# Note: for a better overview, the level of satisfaction has been grouped into 
# 1: 'Not satisfied at all', 
# 2-5: 'Not very satisfied', 
# 6-9: 'Quite satisfied', 
# 10: 'Very satisfied'.

grid.arrange(ties_var_summary(happy_fac, "Happiness") +
               labs(title = "Level of happiness for (non) members",
                    fill = "Level of happiness",
                    subtitle = "of any group"),
             
             ties_var_summary(satisfaction_group, "Satisfaction") +
               labs(title = "Level of satisfaction for (non) members",
                    fill = "Level of satisfaction",
                    subtitle = "of any group"), 
             
             nrow = 1)

# It can be seen, that on average members of any given organisation or institution
# are happier and more satisfied, than individuals who are not.
# This effect doesn't seem to change when looking at general happiness or life
# satisfaction.


bind_rows( fun_happy(happy, member_activity, "an activity"),
           fun_happy(happy, member_religion, "a religion"),
           fun_happy(happy, member_selfhelp, "a selfhelp group"),
           fun_happy(happy, member_charity, "a charity"),
           fun_happy(happy, member_labor_union, "a labor union"),
           fun_happy(happy, member_sports, "a sports group")) %>%
  
  group_by(member, cat, happy) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)) %>%
  
  ggplot(aes(x = as_factor(member), y = perc, fill = as_factor(happy))) +
  geom_bar(stat = "identity") +
  labs(fill = "Level of Happiness",
       y = "Distribution (%)",
       title = "Lebel of happiness, given somebody is a member of ...") +
  
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  
  facet_grid( ~ cat) +
  
  theme_cat

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
# Looking at sports groups, the effect on happiness seems to be surprisingly
# large. Those who reported to be members of sports groups are to a large extent
# either very happy or at least quite happy overall. Those two groups together 
# constitute over 90% of those reporting to be member of a sports group or team.
#
# Finally, being a member of an activity. This seems to be connected to the effect
# of happiness as those who have reported to be member of any sort of activity 
# rarely mentioned to be not very happy or not at all happy. Over 90% of respondents
# who mentioned to be member of an activity reported to be either very or at least
# quite happy. 
# It can be assumed, that this effect is similar to the one observed for sports
# groups, as the graph for overall activities strongly mirrors the one for sports
# groups.

# We assume that weak ties also appear in the neighborhood. Since neighbors are 
# in some way silent, but steady companions, we find it highly important to look 
# at what impact a certain neighborhood can have on the the trust the questioned 
# people have in them.
bind_rows( fun_happy(trust_neighbor, neighbours_diffrace, "have a different race"),
           fun_happy(trust_neighbor, neighbours_drinkers, "are drinkers"),
           fun_happy(trust_neighbor, neighbours_immig, "are immigrants"),
           fun_happy(trust_neighbor, neighbours_drugs, "are drugaddicts"),
           fun_happy(trust_neighbor, neighbours_homosex, "are homosexuals")) %>%
  
  group_by(member, cat, trust_neighbor) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)) %>%
  
  ggplot(aes(x = as_factor(member), y = perc, fill = as_factor(trust_neighbor))) +
  geom_bar(stat = "identity") +
  labs(fill = "Level of trust in neighbours",
       y = "Distribution (%)",
       title = "Level of trust in neighbours, given they ...") +
  
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  
  facet_grid( ~ cat) +
  
  theme_cat
# Starting off with drinking neighbors. To most people it doesn't really seem
# to matter if their neighbors are drinkers or not. The two graphs barely differ
# from each other. There is a small group of respondents that responds with
# "trust somewhat" as opposed to "trust completely" if their neighbors are 
# drinkers, but the effect is too small to be investigated further.
# 
# Being a drug addict however, seems to have a more significant effect. The picture
# is similar to drinking, but a larger percentage of respondents reports to not
# trust their neighbors very much if they are drug addicts.
# 
# The case for homosexuality however is an interesting one. There seems to be 
# a rather large group of respondents that chooses not to trust their neighbors
# very much as opposed to trusting them somewhat, if they happen to be homosexuals. 
# On the other hand, there is also a group of respondents which increase the
# trust in their neighbors from trusting them somewhat to trusting them completely.
# Having homosexual neighbors therefore seems to have a split effect on the 
# Romanian citizens, depending on an external variable.
# 
# Having immigrant neighbors seems to enhance the trust that Romanian citizens
# have in them. This seems to be a uniform effect for all citizens. There is a 
# larger percentage of citizens reporting to trust their neighbors completely
# as well more citizens trusting them at least somewhat if they happen to be 
# immigrants.
#
# Neighbors having a different race seems to polarize the Romanian people to a 
# small but visible extent. There is a larger percentage of respondents reporting
# to trust their neighbors completely as well as a larger percentage of citizens
# not trusting their neighbors at all if they have a different race.

# Furthermore, weak ties also appear within a religion. Individuals which share 
# similar interests, such as the same god, implicitly see each other on a regular 
# basis when attending a church service. The following two mosaic plots shall 
# provide an overview on how the attendance of a church service might affect the 
# level of SWB, as we assume, that weak ties have a positive effect on the Social 
# capital and therefor on the level of SWB.



grid.arrange(romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(happy_fac, relig_service_fac), fill = happy_fac)) +
               labs(title ="Comparison of happiness in life",
                    fill = "Scale of happiness",
                    subtitle = "with church service attendance") +
               mosaic_theme,
             
             romania %>%
               ggplot() +
               geom_mosaic(aes(x =  product(satisfaction_group, relig_service_fac), fill = satisfaction_group)) +
               
               labs(title ="Comparison of satisfaction",
                    fill = "Scale of satisfaction",
                    subtitle = "with church service attendance") +
               mosaic_theme,
             
             nrow = 1)

# There doesn't seem to be a clear connection between church attendance and
# happiness in life or life satisfaction. The mosaic patterns don't differ 
# very much from each other, with one exception. It can be seen, that those
# who practically never attend church report to be not very happy a little 
# more frequently than those who (for instance) attend church more than once
# a week. This effect however is not visible when switching to life 
# satisfaction. Overall, the effect doesn't seem to be strong  enough to 
# speculate a connection between church attendance and happiness or satisfaction.
