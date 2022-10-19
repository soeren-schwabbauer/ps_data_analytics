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


##### load files
# load dataframe
load(paste0(INPUT, "Romania.rda"))

###### explore main variables ##################################################

### A170 satisfaction in life
romania$satisfaction_life
summary(romania$satisfaction_life)
# scaled from 1 to ten


### A008 Feeling of happiness
romania$feel_happy
as_factor(feel_happy)
summary(romania$feel_happy)
# scaled from 1 to 4, 1 is best, 4 is worst


###### compare main variables to  income, health, age, marital status ##########
# mosaic tutorial: https://haleyjeppson.github.io/ggmosaic/reference/geom_mosaic.html

### income (Sören) ###########
theme_mosaic <- theme( 
  #legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_text(angle=90),
  panel.background = element_rect(fill = "white"),
  panel.grid.major.y = element_line(size = 0, linetype = 'solid',
                                    colour = "white") ,
    
  plot.title = element_text(color="black", size= 12, face="bold.italic", vjust = 0.5)
) 

# comparing two quantitative variables by mosaic plot
romania$feel_happy <- as_factor(romania$feel_happy) %>%
  droplevels()
romania$satisfaction_life <- as_factor(romania$satisfaction_life) %>%
  droplevels()
romania$income_scale <- as_factor(romania$income_scale) %>%
  droplevels()

summary(romania$feel_happy)
# happiness #####
ggplot(data = romania) +
  geom_mosaic(aes(x =  product(feel_happy, income_scale), fill = feel_happy)) +
  
  labs(title ="Comparison of satisfaction and income",
       fill = "Scale of happiness") +
       
  
  theme_mosaic

# note: it can be seen, that ...

# satisfaction
ggplot(data = romania) +
  geom_mosaic(aes(x =  product(satisfaction_life, income_scale), fill = satisfaction_life))
# note: it can be seen, that

### health ###########



### age ###########



### marital status ###########


### additional variables ###########