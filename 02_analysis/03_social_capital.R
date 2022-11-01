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
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/02_analysis/INPUT/"
  OUTPUT = "G:/Geteilte Ablagen/data_analytics/02_analysis/OUTPUT/"
  CODEBOOK = "G:/Geteilte Ablagen/data_analytics/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  
  INPUT = "G:/Shared drives/data_analytics/02_analysis/INPUT/"
  OUTPUT = "G:/Shared drives/data_analytics/02_analysis/OUTPUT/"
  CODEBOOK = "G:/Shared drives/data_analytics/"
  
}

# load dataframe
load(paste0(INPUT, "Romania.rda"))


##### define themes and create functions for plotting ##########################

### function + plot for general membership
ties_var_summary <- function(var, var_char){
  romania %>% select({{var}},contains("member_")) %>% rename(member = {{var}}) %>% melt() %>%
    group_by(value, member) %>%
    summarise(n = n()) %>%
    mutate(prop = n/sum(n)) %>%
    
    mutate(value = replace(value, value == 1, "member"),
           value = replace(value, value == 0, "no member"),
           value = as_factor(value)) %>%
    
    ggplot(aes(x = (value), y = prop, fill = as_factor(member))) +
    geom_bar(stat = "identity") +
    labs(y = "Distribution (%)") + 
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle=0),
      #axis.title.y = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                        colour = "grey") ,
      plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5) 
      
    ) +
    
    scale_y_continuous(labels = scales::percent,
                       breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) 
}

### function for each membership & type of neighbor + theme for the plots
fun_happy <- function(cat, var, var_char){
  romania %>% select({{cat}}, {{var}}) %>% rename(member = {{var}}) %>% mutate(cat = {{var_char}}) %>%
    
    mutate(member = as.character(as_factor(member)),
           member = replace(member, member == "Mentioned", "yes"),
           member = replace(member, member == "Not mentioned", "no"))
}

theme_cat <-   theme(
  axis.title.x = element_blank(),
  axis.text.x = element_text(angle=0),
  #axis.title.y = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                    colour = "grey") ,
  plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5) 
) 
  



################################################################################




##### START REPORT #############################################################

# For the following report, we shall have a look at how the subjective well-being (SWB), namely general happiness and life satisfaction, are linked to Social Capital

# In order to take a closer look at the SWB, we shall take the term of the Social Capital into account. The literature shows that Social Capital is an important factor for increasing the level of SWB. But first of all, what is Social Capital?
# Social capital can be seen as "a positive product of human interaction" (investopedia.com). While it can not be owned by somebody, it is the result of connections between individuals within social networks. Social capital can be seen as a base pillar for the effective functioning of social groups. The absence of Social capital can mislead a group of people to not achieve a common goal or purpose. 
# Despite its undeniable value for any group of individuals, there is no consensus on how social capital can be measured directly. This leads to the conclusion, that social capitalcan only be expressed through other indicators. The European Value Study (EVS) includes several indicators which can help to determine a countries Socal Capital 

# In the following, we are going to consider trust as a key dimension for social capital. The EVS sees the trust variable as an indication whether most people can be trusted or not. in particular, the people participating in the survey get asked the following question: Generally speaking, would you say that most people can be trusted or that you can’t be too careful in dealing with people? 
# The answer is then separated into two possible answers (1) "Most people can be trusted" and (2) "Can't be too careful". In the following diagram, we are going to provide an overview of the responses of the qualitative variable.

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

# As we can see from the pie chart and the table, people in Romania are generally rather careful. The vast majority (approx. 85%) expresses their trust in people on a level of "can't be too careful. This rough overview already gives the impression, that the level of Social Capital is rather low in Romania.

trust_var_summary <- function(var, var_char){
  romania %>% select(trust_fac, {{var}}) %>% rename(member = {{var}}) %>% mutate(cat = {{var_char}})
}


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

# BITTE INTERPRETIEREN


# Besides the trust factor, analyzing "weak ties" can also help to get an impression of the Social Capital among the Romanian people. The term weak ties implies connections among individuals which one does not know very well, but by whom one is surrounded on a regular basis. In the EVS, the interviewers asked the participants weather they are a member of various groups. Let us therefor first have a very general overview of how the membership in any group can have on happiness and or life satisfaction. 
# Note: for a better overview, the level of satisfaction has been grouped into 1: 'Not satisfied at all', 2-5: 'Not very satisfied', 6-9: 'Quiet satisfied', 10: 'Very satisfied'.
member_happy <- ties_var_summary(happy_fac, "Happiness") +
  labs(title = "Level of happiness for (non) members",
       fill = "Level of happiness")
  
member_satisfaction <- ties_var_summary(satisfaction_group, "Satisfaction") +
  labs(title = "Level of satisfaction for (non) members",
       fill = "Level of satisfaction")

grid.arrange(member_happy, member_satisfaction, nrow = 1)


# It can be seen, that on average members of any given organisation or institution are happier and more satisfied, than individuals who are not.
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
       title = "Member of ...") +
  
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  
  facet_grid( ~ cat) +
  
  theme_cat
# BITTE INTERPRETIEREN (ggfs. kopieren)


# We assume that weak ties also appear in the neighborhood. Since neighbors are in some way silent, but steady companions, we find it highly important to look at what impact a certain neighborhood can have on the the trust the questioned people have in them.
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
       title = "Level of trust, given your neighbours ...") +
  
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  
  facet_grid( ~ cat) +
  
  theme_cat
# Kann auch interpretiert werden
