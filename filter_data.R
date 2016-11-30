
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
  select(-(B09001_001:B09001_010)) %>%
  mutate(unmarried_partner_present_percent = B09008_002 / B09008_001) %>%
  select(-(B09008_001:B09008_012)) %>%
  select(-(B11001_001:B11001_009)) %>%
  select(-(B16010_001:B16010_053)) %>% # Remove Educational Attainment And Employment Status By Language Spoken At Home For The Population 25 Years And Over
  mutate(house_worth_dollars = B25082_001) %>%
  select(-(B25082_001:B25082_003)) %>%
  mutate(non_contributing_households_percent = (B25102_008 + B25102_015) / B25102_001) %>%
  select(-(B25102_001:B25102_015)) %>%
  select(-(B25103_001:B25103_003)) %>%
  select(-(B992522_001:B992522_007)) %>%
  mutate(school_district = X1) %>%
  select(-X1) %>%
  glimpse
