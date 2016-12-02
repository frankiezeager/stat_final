library(car)
full_data<-read.csv('data_merge.csv')

options(scipen=999)
#create linear models for each test
#including demographic control data

#left out frc_5 (white) (to avoid perfectly collinearity)
#just education data/ race flags

#reassign family income variable
full_data$family_income_median_adj<-full_data$family_income_median/1000
########################################################################
#reading
#just education data
lm.reading.edu<-lm(Reading.SOL~reading.num_female+reading.num_frc_1+reading.num_frc_2+
                 reading.num_frc_3+reading.num_frc_4+reading.num_frc_6+reading.num_frc_99+reading.disability_flag+
                 reading.lep_flag+reading.disadvantaged_flag,data=full_data)
#test for multicollinearity
vif(lm.reading.edu)
#so it appears that frc_4 (hispanic) is extremely multicollinear (6) as with the english learner flag,lep (5.5)

#so lets remove frc_4 and see what happens
lm.reading.edu2<-lm(Reading.SOL~reading.num_female+reading.num_frc_1+reading.num_frc_2+
                     reading.num_frc_3+reading.num_frc_6+reading.num_frc_99+reading.disability_flag+
                     reading.lep_flag+reading.disadvantaged_flag,data=full_data)

#check for multicollinearity
vif(lm.reading.edu2)

#removing frc_4 eliminated the multicollinearity problem (likely because lep captured the same info),
#so no need to do anything about lep

#plot check
plot(lm.reading.edu2)
#residual plot shows a mean of 0, random scattering, no patterns (i.e. it looks good)
#qq plot shows fairly normal data, so we're all good on this front as well
#############################################################

#just county data
lm.reading.county<-lm(Reading.SOL~health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
vif(lm.reading.county)
#none of the county data appears to be too multicollinear (highest vif is 3.7)

#plots
plot(lm.reading.county)
#residual plot: everything looks good, random scattering around 0, etc
#qq plot shows normality as well

#############################################################
#education adn county data included (removed frc_4)
lm.reading.full<-lm(Reading.SOL~reading.num_female+reading.num_frc_1+reading.num_frc_2+
                      reading.num_frc_3+reading.num_frc_6+reading.num_frc_99+reading.disability_flag+
                      reading.lep_flag+reading.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
#check for multicollinearity
vif(lm.reading.full)
#so the family_income_median variable is almost problematic at 4.6, but it
#is a pretty important variable in our analysis, so we'll keep it

#plots
plot(lm.reading.full)
#residual plot:appears to be centered around 0, random scattering and all that
#qq plot also shows normality, so everything looks good here

############################################
#writing
#education
lm.writing.edu<-lm(Writing.SOL~writing.num_female+writing.num_frc_1+writing.num_frc_2+
                   writing.num_frc_3+writing.num_frc_4+writing.num_frc_6+writing.num_frc_99+writing.disability_flag+
                   writing.lep_flag+writing.disadvantaged_flag,data=full_data)
#check for multicollinearity
vif(lm.writing.edu)
#again frc_4 and lep_flag are problematic, so let's remove frc_4, since it has the highest vif
lm.writing.edu2<-lm(Writing.SOL~writing.num_female+writing.num_frc_1+writing.num_frc_2+
                     writing.num_frc_3+writing.num_frc_6+writing.num_frc_99+writing.disability_flag+
                     writing.lep_flag+writing.disadvantaged_flag,data=full_data)
vif(lm.writing.edu2)
#removing frc_4 resolved the multicollinearity problem

#plot
plot(lm.writing.edu2)
#residual plot:looks good, random scattering around 0, no patterns
#qq plot shows some light tails but mostly normal, this shouldn't be too much of an issue

#############################################################
#just county data
lm.writing.county<-lm(Writing.SOL~health_insurance_coverage_percent+
                        housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                        household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                      ,data=full_data)

#test for multicollinearity
vif(lm.writing.county)
#nothing appears to be problematic 

#plots
plot(lm.writing.county)
#residual plot looks good
#qq plot looks good
#############################################################
#education and county data included
lm.writing.full<-lm(Writing.SOL~writing.num_female+writing.num_frc_1+writing.num_frc_2+
                      writing.num_frc_3+writing.num_frc_4+writing.num_frc_6+writing.num_frc_99+writing.disability_flag+
                      writing.lep_flag+writing.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
#check for multicollinearity
vif(lm.writing.full)
#frc_4 has the highest vif at ~7, so let's remove it and see what happens
lm.writing.full2<-lm(Writing.SOL~writing.num_female+writing.num_frc_1+writing.num_frc_2+
                      writing.num_frc_3+writing.num_frc_6+writing.num_frc_99+writing.disability_flag+
                      writing.lep_flag+writing.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
vif(lm.writing.full2)
#so the family_income_median variable is almost problematic at 4.6, but it
#is a pretty important variable in our analysis, so we'll keep it

#plots
plot(lm.writing.full2)
#residual and qq plots look good, no assumption violations

######################################
#History
#just education data
lm.history.edu<-lm(History.SOL~history.num_female+history.num_frc_1+history.num_frc_2+
                     history.num_frc_3+history.num_frc_4+history.num_frc_6+history.num_frc_99+history.disability_flag+
                     history.lep_flag+history.disadvantaged_flag,data=full_data)
#check for multicollinearity
vif(lm.history.edu)
#again frc_4 and lep are problematic, so let's remove frc_4
lm.history.edu2<-lm(History.SOL~history.num_female+history.num_frc_1+history.num_frc_2+
                     history.num_frc_3+history.num_frc_6+history.num_frc_99+history.disability_flag+
                     history.lep_flag+history.disadvantaged_flag,data=full_data)
vif(lm.history.edu2)
#this resolved our multicollinearity problem

#plots
plot(lm.history.edu2)
#residual: there appear to be a few outliers, but these don't seem to problematic
#qq plot: looks good
#############################################################
#just county data
lm.history.county<-lm(History.SOL~health_insurance_coverage_percent+
                        housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                        household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                      ,data=full_data)
#check for multicollinearity
vif(lm.history.county)
#nothing problematic here

plot(lm.history.county)
#residual plot: looks pretty good, there might be some problems with the 0 mean assumption
#but not severely
#qq plot:looks good
#############################################################
#education adn county data included
lm.history.full<-lm(History.SOL~history.num_female+history.num_frc_1+history.num_frc_2+
                      history.num_frc_3+history.num_frc_4+history.num_frc_6+history.num_frc_99+history.disability_flag+
                      history.lep_flag+history.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
vif(lm.history.full)
#frc_4 has the highest (almost a vif of 10), so let's remove it and see what happens
lm.history.full2<-lm(History.SOL~history.num_female+history.num_frc_1+history.num_frc_2+
                      history.num_frc_3+history.num_frc_6+history.num_frc_99+history.disability_flag+
                      history.lep_flag+history.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
vif(lm.history.full2)
#so the family_income_median variable is almost problematic at 4.8, but it
#is a pretty important variable in our analysis, so we'll keep it


#plots
plot(lm.history.full2)
#residual plot: looks pretty good, there might be some problems with the 0 mean assumption
#but not severely
#qq plot:looks good

######################################
#Math
#just education data
lm.math.edu<-lm(Math.SOL~math.num_female+math.num_frc_1+math.num_frc_2+
                     math.num_frc_3+math.num_frc_4+math.num_frc_6+math.num_frc_99+math.disability_flag+
                     math.lep_flag+math.disadvantaged_flag,data=full_data)
#check for multicollinearity
vif(lm.math.edu)
#frc_4 and lep both have super huge vifs, so let's remove frc_4
lm.math.edu2<-lm(Math.SOL~math.num_female+math.num_frc_1+math.num_frc_2+
                  math.num_frc_3+math.num_frc_6+math.num_frc_99+math.disability_flag+
                  math.lep_flag+math.disadvantaged_flag,data=full_data)
vif(lm.math.edu2)
#multicollinearity was resolved by removing frc_4

#plots
plot(lm.math.edu2)
#residual plot: there are definitely some outliers, and the 0 mean assumption may be somewhat violated
#qq plot: looks good
#############################################################
#just county data
lm.math.county<-lm(Math.SOL~health_insurance_coverage_percent+
                        housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                        household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                      ,data=full_data)
vif(lm.math.county)
#no multicollinearity problem

#plots
plot(lm.math.county)
#residual plot: looks okay, with some outliers
#qq plot:may be a slight violation of normality, but it doesnt appear to problematic
#############################################################
#education adn county data included
lm.math.full<-lm(Math.SOL~math.num_female+math.num_frc_1+math.num_frc_2+
                      math.num_frc_3+math.num_frc_4+math.num_frc_6+math.num_frc_99+math.disability_flag+
                      math.lep_flag+math.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)
vif(lm.math.full)
#lets remove frc_4 to see if that helps the multicollinearity situation

lm.math.full2<-lm(Math.SOL~math.num_female+math.num_frc_1+math.num_frc_2+
                   math.num_frc_3+math.num_frc_6+math.num_frc_99+math.disability_flag+
                   math.lep_flag+math.disadvantaged_flag+health_insurance_coverage_percent+
                   housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                   household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                 ,data=full_data)
vif(lm.math.full2)
#theres still a multicollinearity problem, so lets remove the family income variable
lm.math.full3<-lm(Math.SOL~math.num_female+math.num_frc_1+math.num_frc_2+
                  math.num_frc_3+math.num_frc_6+math.num_frc_99+math.disability_flag+
                  math.lep_flag+math.disadvantaged_flag+health_insurance_coverage_percent+
                  housing_units_with_mortgage_percent+poverty_students_percent+
                  household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                ,data=full_data)
vif(lm.math.full3)
#multicollinearity resolved

#plots
plot(lm.math.full3)
#residual plot: definitely some outliers and some possible fanning. it doesnt appear too extreme
#qq plot: looks good

######################################
#Science
#just education data
lm.science.edu<-lm(Science.SOL~science.num_female+science.num_frc_1+science.num_frc_2+
                     science.num_frc_3+science.num_frc_4+science.num_frc_6+science.num_frc_99+science.disability_flag+
                     science.lep_flag+science.disadvantaged_flag,data=full_data)

vif(lm.science.edu)
#again, frc_4 and lep_flag are problematic, lets remove frc_4
lm.science.edu2<-lm(Science.SOL~science.num_female+science.num_frc_1+science.num_frc_2+
                     science.num_frc_3+science.num_frc_6+science.num_frc_99+science.disability_flag+
                     science.lep_flag+science.disadvantaged_flag,data=full_data)
vif(lm.science.edu2)
#multicollinearity resolved

#plots
plot(lm.science.edu2)
#residual plot: some possible fanning, which may indicate that a linear model wasn't the best choice
#qq plot: also appear to be some light tails involved
#########################################################################
#just county data
lm.science.county<-lm(Science.SOL~health_insurance_coverage_percent+
                        housing_units_with_mortgage_percent+poverty_students_percent+family_income_median_adj+
                        household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                      ,data=full_data)
vif(lm.science.county)
#no multicollinearity


#plots
plot(lm.science.county)
#residual plot: some outliers, but nothign too severe
#qq plot: possible light tails, but not too severe

#################################################################
#education and county data included
lm.science.full<-lm(Science.SOL~science.num_female+science.num_frc_1+science.num_frc_2+
                      science.num_frc_3+science.num_frc_4+science.num_frc_6+science.num_frc_99+science.disability_flag+
                      science.lep_flag+science.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)

vif(lm.science.full)
#let's remove frc_4 to see if it helps the multicollinearity

lm.science.full2<-lm(Science.SOL~science.num_female+science.num_frc_1+science.num_frc_2+
                      science.num_frc_3+science.num_frc_6+science.num_frc_99+science.disability_flag+
                      science.lep_flag+science.disadvantaged_flag+health_insurance_coverage_percent+
                      housing_units_with_mortgage_percent+poverty_students_percent+family_income_median+
                      household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                    ,data=full_data)

vif(lm.science.full2)

#lets remove median family income to see if it helps further

lm.science.full3<-lm(Science.SOL~science.num_female+science.num_frc_1+science.num_frc_2+
                       science.num_frc_3+science.num_frc_6+science.num_frc_99+science.disability_flag+
                       science.lep_flag+science.disadvantaged_flag+health_insurance_coverage_percent+
                       housing_units_with_mortgage_percent+poverty_students_percent+
                       household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                     ,data=full_data)

vif(lm.science.full3)

#lets remove disadvantaged now

lm.science.full4<-lm(Science.SOL~science.num_female+science.num_frc_1+science.num_frc_2+
                       science.num_frc_3+science.num_frc_6+science.num_frc_99+science.disability_flag+
                       science.lep_flag+health_insurance_coverage_percent+
                       housing_units_with_mortgage_percent+poverty_students_percent+
                       household_foodstamps_percent+unmarried_partner_present_percent+house_worth_dollars+non_contributing_households_percent
                     ,data=full_data)

vif(lm.science.full4)
#now the multicollinearity has been resolved

#plots
plot(lm.science.full4)
#residual plot: some definite outliers, but nothing too severe
#qq plot:may be indicative of non-normality

######################################################
# Look at model summaries for the full models

summary(lm.reading.full)
summary(lm.writing.full2)
summary(lm.history.full2)
summary(lm.math.full3)
summary(lm.science.full4)








