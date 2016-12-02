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
