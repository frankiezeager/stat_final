
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
})

data <- read_csv("acs_data.csv")


data %>% glimpse
data %>%
  mutate(health_insurance_coverage_percent = (B27015_003 + B27015_008 + B27015_013 + B27015_018 + B27015_023) / B27015_001) %>% # Health Insurance
  select(-(B27015_001:B27015_026)) %>%
  mutate(housing_units_with_mortgage_percent = B25081_001 / B25081_002) %>% # housing mortgage
  select(-(B25081_001:B25081_008)) %>%
  mutate(poverty_students_percent = B14006_007 / (B14006_007 + B14006_017)) %>% # Student Poverty
  select(-(B14006_001:B14006_021)) %>%
  select(-(B16009_001:B16009_027)) %>% # Remove extra poverty status
  mutate(income_median = B19013_001) %>% # Median Household income
  select(-B19013_001, -(B19013A_001:B19013I_001)) %>% # remove extra tables
  glimpse

data %>%
  names %>%
  discard(~ . == "X1") %>%
  discard(function(colName){
    data[[colName]] %>% is.na %>% sum %>% `/`(nrow(data)) -> per
    per > 0.2
  }) -> drop_col_names


health_data <- readRDS("temp.rds")

health_data %>% glimpse

(3 + (0:4 * 5)) %>%
  str_pad(., 3, pad = "0") %>%
  paste("B27015_",.,sep='')



str_pad(3, 6, pad = "0")

health_data[1,]
health_data[2,]
