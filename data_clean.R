#############################
######  Data Cleaning #######
#############################
# Tyler Hutcherosn
# STAT 6021 Final Project
# Clean ACS data, handle missing values, prepare to merge

library(dplyr)
library(purrr)
library(mice)

## Read in the ACS data set
acs.data <- read.csv(file="~/Desktop/Data_Science/Linear_Models/stat_final/acs_data.csv", header=TRUE)
schools <-  #get div names from last file
  
## Check levels of missingness
acs.data %>%
  names %>%
  discard(~ . == "X1") %>%
  discard(function(colName){
    acs.data[[colName]] %>% is.na %>% sum %>% `/`(nrow(acs.data)) -> per
    per > 0.2
  }) -> drop_col_names   

## Drop columns that have a missingness level greater than 20%
acs.data <- acs.data[,-drop_col_names]

## Perform multiple imputation on the rest of the data






