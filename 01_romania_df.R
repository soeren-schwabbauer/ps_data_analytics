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
library(labelled)

# load df
if (dir.exists("G:/Geteilte Ablagen/")) {
  
INPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/INPUT/"
OUTPUT = "G:/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
CODEBOOK = "G:/Geteilte Ablagen/data_analytics/"

} else if (dir.exists("G:/Shared drives/")) {
  

INPUT = "G:/Shared drives/data_analytics/01_data_build/INPUT/"
OUTPUT = "G:/Shared drives/data_analytics/01_data_build/OUTPUT/"
CODEBOOK = "G:/Shared drives/data_analytics/"

} else if (dir.exists("~/Google Drive/Geteilte Ablagen/")) {
  
  
INPUT = "~/Google Drive/Geteilte Ablagen/data_analytics/01_data_build/INPUT/"
OUTPUT = "~/Google Drive/Geteilte Ablagen/data_analytics/01_data_build/OUTPUT/"
CODEBOOK = "~/Google Drive/Geteilte Ablagen/data_analytics/"
  
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







# mutate new variables to factor
romania <- romania %>%  mutate(
  
### factorize levels & drop levels
         income_scale_fac = as_factor(income_scale),
         income_scale_fac = droplevels(income_scale_fac),
         
         health_fac = as_factor(health),
         health_fac = droplevels(health_fac),
         
         marst_fac = as_factor(marst),
         marst_fac = droplevels(marst_fac),
         
         sex_fac = as_factor(sex),
         sex_fac = droplevels(sex_fac),
         
         trust_fac = as_factor(trust),
         trust_fac = droplevels(trust_fac),
         
         freedom_fac = as_factor(freedom),
         freedom_fac = droplevels(freedom_fac),
         
         income_eq_fac = as_factor(income_equality),
         income_eq_fac = droplevels(income_eq_fac),
         
         member_religion_fac = as_factor(member_religion),
         member_religion_fac = droplevels(member_religion_fac),
         
         member_activity_fac = as_factor(member_activity),
         member_activity_fac = droplevels(member_activity_fac),
         
         educ_fac = as_factor(educ),
         educ_fac = droplevels(educ_fac),
         
         age = as.numeric(age),
         
### group varibales
         satisfaction_fac = as_factor(satisfaction),
         satisfaction_fac = droplevels(satisfaction_fac),
         satisfaction_group = case_when(as.numeric(satisfaction) == 1  ~ "Not satisfied at all",
                                      as.numeric(satisfaction) == 2 |
                                        as.numeric(satisfaction) == 3 |
                                        as.numeric(satisfaction) == 4 |
                                        as.numeric(satisfaction) == 5  ~ "Not very satisfied",
                                      as.numeric(satisfaction) == 6 |
                                        as.numeric(satisfaction) == 7  |
                                        as.numeric(satisfaction) == 8 |
                                        as.numeric(satisfaction) == 9 ~ "Quiet satisfied",
                                      as.numeric(satisfaction) == 10  ~ "Very satisfied"),
         
         relig_service_fac = case_when(as.numeric(relig_service) == 1  ~ "More than once a week",
                                       as.numeric(relig_service) == 2  ~ "Once a week",
                                       as.numeric(relig_service) == 3  ~ "Once a month",
                                       as.numeric(relig_service) == 4 |
                                         as.numeric(relig_service) == 5  ~ "Special days only",
                                       as.numeric(relig_service) == 6  ~ "Once a year",
                                       as.numeric(relig_service) == 7  ~ "Less often",
                                       as.numeric(relig_service) == 8  ~ "(Practically) Never"),
         relig_service_fac = as_factor(relig_service_fac),
         
         freedom_group = case_when(as.numeric(freedom) == 1 |
                             as.numeric(freedom) == 2 |
                             as.numeric(freedom) == 3 |
                             as.numeric(freedom) == 4 ~ "1) not much",
                           as.numeric(freedom) == 5  | 
                                     as.numeric(freedom) == 6  |
                                     as.numeric(freedom) == 7 ~ "2) a little",
                           as.numeric(freedom) == 8 |
                             as.numeric(freedom) == 9  ~ "3) rather agree",
                           as.numeric(freedom) == 10  ~ "4) a great deal"),

          member_active = case_when((relig_service == 1 | relig_service == 2 & member_religion == 1) ~ 1,
                                          (member_sports == 1) ~ 1,
                                          (member_protest == 1) ~ 1,
                                          (member_charity == 1) ~ 1,
                                          (member_selfhelp == 1) ~ 1),

          member_active = replace_na(member_active, 0),

          member_active_fac = case_when(member_active == 1 ~ "active member",
                              member_active == 0 ~ "inactive member"),

          sex_married = case_when((sex == 1 & marst < 3 ) ~ 'partnership, male',
                        (sex == 1 & marst >= 3 ) ~ 'no partnership, male',
                        (sex == 2 & marst < 3 ) ~ 'partnership, female',
                        (sex == 2 & marst >= 3 ) ~ 'no partnership, female'),

          educ_group = case_when((educ >= 6) ~ 'College Education',
                                 (educ >= 4 | educ <=5) ~ "High School Education",
                                 (educ <= 3) ~ "Less than High School"))
  
  


### flip certain variables
# higher number -> better
# lower number -> worse

romania$happy <- drop_unused_value_labels(romania$happy)
romania$happy <- reverse_labelled_values(romania$happy)
romania$happy_fac <- as_factor(romania$happy)

romania$intrests_politics <- drop_unused_value_labels(romania$intrests_politics)
romania$intrests_politics <- reverse_labelled_values(romania$intrests_politics)
romania$intrests_politics_fac <- as_factor(romania$intrests_politics)

romania$politics_petition <- drop_unused_value_labels(romania$politics_petition)
romania$politics_petition <- reverse_labelled_values(romania$politics_petition)
romania$politics_petition_fac <- as_factor(romania$politics_petition)

romania$politics_boycott <- drop_unused_value_labels(romania$politics_boycott)
romania$politics_boycott <- reverse_labelled_values(romania$politics_boycott)
romania$politics_boycott_fac <- as_factor(romania$politics_boycott)

romania$politics_demo <- drop_unused_value_labels(romania$politics_demo)
romania$politics_demo <- reverse_labelled_values(romania$politics_demo)
romania$politics_demo_fac <- as_factor(romania$politics_demo)

romania$politics_strikes <- drop_unused_value_labels(romania$politics_strikes)
romania$politics_strikes <- reverse_labelled_values(romania$politics_strikes)
romania$politics_strikes_fac <- as_factor(romania$politics_strikes)

romania$politics_democracy <- drop_unused_value_labels(romania$politics_democracy)
romania$politics_democracy <- reverse_labelled_values(romania$politics_democracy)
romania$politics_democracy_fac <- as_factor(romania$politics_democracy)

romania$imp_fam <- drop_unused_value_labels(romania$imp_fam)
romania$imp_fam <- reverse_labelled_values(romania$imp_fam)
romania$imp_fam_fac <- as_factor(romania$imp_fam)

romania$imp_friends <- drop_unused_value_labels(romania$imp_friends)
romania$imp_friends <- reverse_labelled_values(romania$imp_friends)
romania$imp_friends_fac <- as_factor(romania$imp_friends)

romania$trust_neighbor <- drop_unused_value_labels(romania$trust_neighbor)
romania$trust_neighbor <- reverse_labelled_values(romania$trust_neighbor)
romania$trust_neighbor_fac <- as_factor(romania$trust_neighbor)

romania$health <- drop_unused_value_labels(romania$health)
romania$health <- reverse_labelled_values(romania$health)
romania$health_fac <- as_factor(romania$health)

## add variable member_any: are you a member in any organisation
romania <- romania %>% 
  
  rowwise() %>%
  mutate(member_tot = sum(member_religion, member_activity, member_labor_union, member_party, member_association, member_sports, member_consumer, member_other, member_charity, member_selfhelp)) %>%
  ungroup() %>%
  mutate(member_any = ifelse(member_tot >= 1, 1, 0)) %>%
  
  mutate(member_any_fac = factor(member_any),
         member_any_fac = recode_factor(member_any_fac, "0"= "no", "1" = "yes")) %>%
  
  mutate(member_tot_fac = as_factor(member_tot),
         member_tot_fac =fct_relevel(member_tot_fac,c("0","1","2","3","4","5","6","7","10")))





##### save file
save(romania, file = paste0(OUTPUT, "romania.rda"))

