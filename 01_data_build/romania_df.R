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


##### save file
save(romania, file = paste0(OUTPUT, "romania.rda"))

