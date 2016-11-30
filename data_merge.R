################################
########## Data Merge ##########
################################
# STAT 6021
# Final Project
# Tyler Hutcherson, Erik Langenborg, Frankie Zeager, Stephen Morris

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
})

## Read in the two data files
acs <- read.csv("acs_wrangling.csv", header=TRUE)
