###### DATA VIZ #########


full_data<-read.csv('data_merge.csv')

#reassign family income variable and house worth
full_data$family_income_median_adj<-full_data$family_income_median/1000
full_data$house_worth_dollars_adj <- full_data$house_worth_dollars/1000000

######################################################
# Some scatter plots
library(ggplot2)
library(gridExtra)

income.read <- ggplot(full_data, aes(x = family_income_median_adj, y = Reading.SOL))
p1 <- income.read +geom_point()
p1

p2 <- income.read + geom_point(aes(size=house_worth_dollars_adj))
p2 #this is neat

partner.read <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Reading.SOL))
p3 <- partner.read + geom_point()
p3

p4 <- partner.read + geom_point(aes(size=family_income_median_adj))
p4


