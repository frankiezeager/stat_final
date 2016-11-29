
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
  select(-(B19019_001:B19019_008)) %>% # remove extra tables
  select(-(B19049_001:B19049_005)) %>% # remove extra tables
  select(-(B25099_001:B25099_003)) %>% # remove extra tables
  select(-(B25119_001:B25119_003)) %>% # median Household tenure
  select(-(B10010_001:B10010_003)) %>%
  select(-B19113_001, -(B19113A_001:B19113I_001)) %>% # 	Median Family Income In The Past 12 Months (In 2013 Inflation-Adjusted Dollars)
  select(-(B19119_001:B19119_007)) %>% #
  select(-(B08537_001:B08537_021)) %>% # Missing data
  select(-(B19121_001:B19121_005)) %>% #
  select(-(B19125_001:B19125_003)) %>% # More median family income
  select(-(B11017_001:B11017_003), -(B12007_001:B12007_002), -(B12007A_001:B12007I_002)) %>%
  mutate(family_income_median = B19301_001) %>%
  select(-(B19126_001:B19301I_001)) %>%
  mutate(household_foodstamps_percent = B22003_002 / B22003_001) %>%
  select(-(B22003_001:B22003_007)) %>%
  mutate(household_foodstamps_percent = (B08137_004 + B08137_007) / B08137_001) %>%
  select(-(B08137_001:B08137_021)) %>%
  select(-(C02003_001:C02003_019)) %>% # Detailed Race
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
