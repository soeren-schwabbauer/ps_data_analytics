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
library(Hmisc)

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
trust_table <- (trust_summary)

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

  ggplot(aes(x = droplevels(member), y = prop, fill = as_factor(trust_fac))) +
  geom_bar(stat = "identity") +
  labs(fill = "Dimensions of trust",
       y = "Distribution (%)",
       title = "Variables for SWB") + 
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

