rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 01.11.2022
# Tasks: 
# Try to find appropriate components for the political interest and attitude dimension, e.g. run a PCA using the variables “Interested in politics”, “Satisfaction with the political system”, “Having a democratic political system”, “Importance of democracy”, ..
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


# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
  INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
  
} else if (dir.exists("G:/Shared drives/")) {
  
  INPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
  
}

# load dataframe
load(paste0(INPUT, "Romania.rda"))

# The key variable for our analysis is going to be the E023 - Interest in politics (interest_politics) variable. However, the survey offers more questions in regards to the attitude towards politics. The variables in the following graph are going to be key elements in the following principal components analysis.

### select relevant variables
politics_variables <- romania %>% 
  select(intrests_politics, politics_petition, 
         politics_boycott,
         politics_demo,
         politics_strikes,
         
         politics_satisfaction, 
         politics_democracy,
         importance_democracy, 
  ) 
politics_variables <- politics_variables %>% rownames_to_column()

politics_var <- function(varchar) {
  politics_variables %>%
  mutate_all(function(x) droplevels(as_factor(x))) %>% 
  melt(id.vars = "rowname", variable.name = "factor", value.name = "level") %>%
  
  group_by(factor, level) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(freq_cum = revcumsum(freq)) %>%
  filter(factor == {{varchar}}) %>%
  
  ggplot(aes(y = freq, x = factor, fill = level)) +
    geom_bar(postition = "dodge", stat = "identity") +

geom_text(aes(y = freq_cum, label = level), vjust = -1, colour = "white") +
scale_y_continuous(labels = scales::percent) +

theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"),
  plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.1),
  legend.position = "none"
)
}

grid.arrange(politics_var("intrests_politics" ), politics_var("politics_petition"), politics_var("politics_boycott"), politics_var("politics_demo"), ncol = 4)
grid.arrange(politics_var("politics_strikes"), politics_var("politics_satisfaction"), politics_var("politics_democracy"), politics_var("importance_democracy"), ncol = 4)

# In order to avoid misunderstnding, we want to give a quick overview of what the variables mean. 

#  | **id**  | **variable**          | **explaination**                                  |
#  |---------|-----------------------|---------------------------------------------------|
#  | E023    | intrests_politics     | How interested would you say you are in politics? |
#  | E025    | politics_petition     | Signing a petition                                |
#  | E026    | politics_boycott      | Joining in boycotts                               |
#  | E027    | politics_demo         | Attending lawful demonstrations                   |
# | E028    | politics_strikes      | Joining unofficial strikes                        |
#  | E111_01 | politics_satisfaction | Satisfaction with the political system            |
#  | E117    | politics_democracy    | Having a democratic political system              |
#  | E235    | importance_democracy  | Importance of democracy                           |





  
pc.politics <-  politics_variables %>% prcomp( scale = TRUE)

fviz_eig(pc.politics, addlabels = TRUE)

grid.arrange(fviz_pca_ind(pc.politics, axes = c(1,2), label = "var", repel = TRUE),
             fviz_pca_ind(pc.politics, axes = c(1,3), label = "var", repel = TRUE), 
             fviz_pca_ind(pc.politics, axes = c(2,3), label = "var", repel = TRUE),
             fviz_eig(pc.politics, addlabels = TRUE),
             ncol = 2)

grid.arrange(fviz_pca_biplot(pc.politics, axes = c(1,3), label = "var", repel = TRUE),
             fviz_pca_biplot(pc.politics, axes = c(1,2), label = "var", repel = TRUE),
             ncol = 2)
