full_data<-read.csv('data_merge.csv')

options(scipen=999)
#create linear models for each test
#including demographic control data

#left out frc_5 (white) (to avoid perfectly collinearity)
#just education data/ race flags

#reassign family income variable
full_data$family_income_median_adj<-full_data$family_income_median/1000

#reading
#just education data
lm.reading.edu<-lm(Reading.SOL~reading.num_female+reading.num_frc_1+reading.num_frc_2+
                 reading.num_frc_3+reading.num_frc_4+reading.num_frc_6+reading.num_frc_99+reading.disability_flag+
                 reading.lep_flag+reading.disadvantaged_flag,data=full_data)

#just county data
lm.reading.county<-lm(Reading.SOL~health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
#education adn county data included
lm.reading.full<-lm(Reading.SOL~reading.num_female+reading.num_frc_1+reading.num_frc_2+
                      reading.num_frc_3+reading.num_frc_4+reading.num_frc_6+reading.num_frc_99+reading.disability_flag+
                      reading.lep_flag+reading.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
############################################
#writing
#education
lm.writing.edu<-lm(Writing.SOL~writing.num_female+writing.num_frc_1+writing.num_frc_2+
                   writing.num_frc_3+writing.num_frc_4+writing.num_frc_6+writing.num_frc_99+writing.disability_flag+
                   writing.lep_flag+writing.disadvantaged_flag,data=full_data)
#just county data
lm.writing.county<-lm(Writing.SOL~health_insurance_coverage_percent+
                        housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                        household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                      ,data=full_data)
#county data included
lm.writing.full<-lm(Writing.SOL~writing.num_female+writing.num_frc_1+writing.num_frc_2+
                      writing.num_frc_3+writing.num_frc_4+writing.num_frc_6+writing.num_frc_99+writing.disability_flag+
                      writing.lep_flag+writing.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)

######################################
#History
#just education data
lm.history.edu<-lm(History.SOL~history.num_female+history.num_frc_1+history.num_frc_2+
                     history.num_frc_3+history.num_frc_4+history.num_frc_6+history.num_frc_99+history.disability_flag+
                     history.lep_flag+history.disadvantaged_flag,data=full_data)

#just county data
lm.history.county<-lm(History.SOL~health_insurance_coverage_percent+
                        housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                        household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                      ,data=full_data)
#education adn county data included
lm.history.full<-lm(History.SOL~history.num_female+history.num_frc_1+history.num_frc_2+
                      history.num_frc_3+history.num_frc_4+history.num_frc_6+history.num_frc_99+history.disability_flag+
                      history.lep_flag+history.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
######################################
#Math
#just education data
lm.math.edu<-lm(Math.SOL~math.num_female+math.num_frc_1+math.num_frc_2+
                     math.num_frc_3+math.num_frc_4+math.num_frc_6+math.num_frc_99+math.disability_flag+
                     math.lep_flag+math.disadvantaged_flag,data=full_data)

#just county data
lm.math.county<-lm(Math.SOL~health_insurance_coverage_percent+
                        housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                        household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                      ,data=full_data)
#education adn county data included
lm.math.full<-lm(Math.SOL~math.num_female+math.num_frc_1+math.num_frc_2+
                      math.num_frc_3+math.num_frc_4+math.num_frc_6+math.num_frc_99+math.disability_flag+
                      math.lep_flag+math.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
######################################
#Science
#just education data
lm.science.edu<-lm(Science.SOL~science.num_female+science.num_frc_1+science.num_frc_2+
                     science.num_frc_3+science.num_frc_4+science.num_frc_6+science.num_frc_99+science.disability_flag+
                     science.lep_flag+science.disadvantaged_flag,data=full_data)

#just county data
lm.science.county<-lm(Science.SOL~health_insurance_coverage_percent+
                        housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                        household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                      ,data=full_data)
#education adn county data included
lm.science.full<-lm(Science.SOL~science.num_female+science.num_frc_1+science.num_frc_2+
                      science.num_frc_3+science.num_frc_4+science.num_frc_6+science.num_frc_99+science.disability_flag+
                      science.lep_flag+science.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)

#things to do:
#vif
#residual plots
#qq plots
#resolving those 