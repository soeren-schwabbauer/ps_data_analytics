rm(list = ls())
################################################################################
# Course: Data Analytics - PS
# Date: 13.10.2022
# Purpose: Relabel variables
################################################################################

# load libraries
library(dplyr)
library(readxl)
library(haven)
library(codebook)

# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/INPUT/"
OUTPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
CODEBOOK = "G:/Geteilte Ablagen/data_analytics/"

} else if (dir.exists("G:/Shared drives/")) {
  

INPUT = "G:/Shared drives/data_analytics/01_data_build/INPUT/"
OUTPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
CODEBOOK = "G:/Shared drives/data_analytics/"

}


##### load files
# load dataframe
load(paste0(INPUT, "Romania.rda"))

# load codebook
codebook <- read_excel(paste0(CODEBOOK, "99_codebook.xlsx")) %>%
  na.omit()

##### edits
# pull var_id & variable into vector for relabeling later
var_id <- codebook %>% pull(var_id)
variable <- codebook %>% pull(variable)


# command to relabel variables
romania <- data %>%
  
  rename_at(vars(paste0(var_id, "")), function(x) variable)  # rename variables
# A003 doesn't exist in df



### flip certain variables
# higher number -> better
# lower number -> worse



# mutate new variables to factor
romania <- romania %>%
  
  mutate(satisfaction_fac = as_factor(satisfaction),
         
         happy_fac = as_factor(happy),

         income_scale_fac = as_factor(income_scale),
         
         health_fac = as_factor(health),
         
         marst_fac = as_factor(marst),
         
         sex_fac = as_factor(sex),
         
         trust_fac = as_factor(trust)) %>%
  
  mutate(age = as.numeric(age)) %>%
  

  
  # group satisfaction
  mutate(satisfaction_group = case_when(as.numeric(satisfaction) == 1  ~ "Not satisfied at all",
                                      as.numeric(satisfaction) == 2 |
                                        as.numeric(satisfaction) == 3 |
                                        as.numeric(satisfaction) == 4 |
                                        as.numeric(satisfaction) == 5  ~ "Not very satisfied",
                                      as.numeric(satisfaction) == 6 |
                                        as.numeric(satisfaction) == 7  |
                                        as.numeric(satisfaction) == 8 |
                                        as.numeric(satisfaction) == 9 ~ "Quiet satisfied",
                                      as.numeric(satisfaction) == 10  ~ "Very satisfied")) %>%
  
  mutate(relig_service_fac = case_when(as.numeric(relig_service) == 1  ~ "More than once a week",
                                       as.numeric(relig_service) == 2  ~ "Once a week",
                                       as.numeric(relig_service) == 3  ~ "Once a month",
                                       as.numeric(relig_service) == 4 |
                                         as.numeric(relig_service) == 5  ~ "Special days only",
                                       as.numeric(relig_service) == 6  ~ "Once a year",
                                       as.numeric(relig_service) == 7  ~ "Less often",
                                       as.numeric(relig_service) == 8  ~ "(Practically) Never"),
         relig_service_fac = as_factor(relig_service_fac)) 
                                       



romania$satisfaction_fac <- droplevels(romania$satisfaction_fac)
romania$income_scale_fac <- droplevels(romania$income_scale_fac)
romania$happy_fac <- droplevels(romania$happy_fac)
romania$marst_fac <- droplevels(romania$marst_fac)
romania$health_fac <- droplevels(romania$health_fac)
romania$sex <-  droplevels(romania$sex_fac)
romania$trust_fac <- droplevels(romania$trust_fac)



# check for all variables

# okay for:
# satisfaction 
reverse <- c("intrests_politics")
reverse_fac <- paste0(reverse, "_fac")

reverse <- function(var, var_fac){
  romania$var_fac <- as_factor(romania$var)
  romania$var_fac <- droplevels(romania$var_fac)
  romania$var <- reverse_labelled_values(romania$var_fac)

}

reverse(intrests_politics, intrests_politics_fac)

reverse_labelled_values(happy_fac)
reverse_labelled_values(health_fac)
reverse_labelled_values(trust_fac)

romania$intrests_politics_fac <- as_factor(romania$intrests_politics)
romania$intrests_politics_fac <- droplevels(romania$intrests_politics_fac)
romania$intrests_politics <- reverse_labelled_values(romania$intrests_politics_fac)

romania$politics_petition_fac <- as_factor(romania$politics_petition)
romania$politics_petition_fac <- droplevels(romania$politics_petition_fac)
romania$politics_petition <- reverse_labelled_values(romania$politics_petition_fac)

romania$politics_boycott_fac <- as_factor(romania$politics_boycott)
romania$politics_boycott_fac <- droplevels(romania$politics_boycott_fac)
romania$politics_boycott <- reverse_labelled_values(romania$politics_boycott_fac)


romania$politics_demo_fac <- as_factor(romania$politics_demo)
romania$politics_demo_fac <- droplevels(romania$politics_demo_fac)
romania$politics_demo <- reverse_labelled_values(romania$politics_demo_fac)

romania$politics_demo_fac <- as_factor(romania$politics_demo)
romania$politics_demo_fac <- droplevels(romania$politics_demo_fac)
romania$politics_demo <- reverse_labelled_values(romania$politics_demo_fac)
attach(romania)

intrests_politics
politics_petition
politics_boycott
politics_demo
politics_strikes
politics_satisfaction
politics_democracy
importance_democracy 

##### save file
save(romania, file = paste0(OUTPUT, "romania.rda"))

