###################################
############ DATA VIZ #############
###################################

full_data<-read.csv('data_merge.csv')

# reassign family income variable and house worth
full_data$family_income_median_adj<-full_data$family_income_median/1000
full_data$house_worth_dollars_adj <- full_data$house_worth_dollars/1000000

##########################################################################
# Some scatter plots
library(ggplot2)
library(gridExtra)
library(dplyr)
library(purrr)

income.read <- ggplot(full_data, aes(x = family_income_median_adj, y = Reading.SOL))
# p1 <- income.read +geom_point()
# p1
p2 <- income.read + geom_point(aes(size=house_worth_dollars_adj))
p2 #this is neat

partner.read <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Reading.SOL))
# p3 <- partner.read + geom_point()
# p3
p4 <- partner.read + geom_point(aes(size=family_income_median_adj))
p4

income.write <- ggplot(full_data, aes(x = family_income_median_adj, y = Writing.SOL))
p5 <- income.write + geom_point(aes(size=house_worth_dollars_adj))
p5 

partner.write <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Writing.SOL))
p6 <- partner.write + geom_point(aes(size=family_income_median_adj))
p6

income.history <- ggplot(full_data, aes(x = family_income_median_adj, y = History.SOL))
p7 <- income.history + geom_point(aes(size=house_worth_dollars_adj))
p7 

partner.history <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = History.SOL))
p8 <- partner.history + geom_point(aes(size=family_income_median_adj))
p8

income.math <- ggplot(full_data, aes(x = family_income_median_adj, y = Math.SOL))
p9 <- income.math + geom_point(aes(size=house_worth_dollars_adj))
p9 

partner.math <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Math.SOL))
p10 <- partner.math + geom_point(aes(size=family_income_median_adj))
p10

income.science <- ggplot(full_data, aes(x = family_income_median_adj, y = Science.SOL))
p11 <- income.science + geom_point(aes(size=house_worth_dollars_adj))
p11

partner.science <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Science.SOL))
p12 <- partner.science + geom_point(aes(size=family_income_median_adj))
p12
##########################
read.write <- ggplot(full_data, aes(x=Reading.SOL, y=Writing.SOL))
t1 <- read.write + geom_point()
t1  # problem found

math.science <- ggplot(full_data, aes(x=Math.SOL, y=Science.SOL))
t2 <- math.science + geom_point()
t2 <- t2 + geom_point(aes(size=house_worth_dollars_adj))
t2
#################################################################################################

###### MAPS ########
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
set.seed(8000)

# set directory to the folder where the shapefile is, then input shapefile
setwd("tl_2014_51_unsd/")
virginia.shp <- readShapeSpatial("tl_2014_51_unsd.shp") %>% 
  fortify(region = "NAME") %>% 
  mutate(id = paste(id, ", Virginia", sep=""))

full_data$id <- full_data$school_district
merge.shp <- merge(virginia.shp, full_data, by="id", all.y=TRUE) 
final.plot<-merge.shp[order(merge.shp$order), ] 

va <- ggplot() + 
  geom_polygon(data = final.plot, aes(x = long, y = lat, group = group), color= "black", size = 0.25) + 
  coord_map()

va.reading <- ggplot() + 
  geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = Reading.SOL), color= "black", size = 0.25) + 
  coord_map()
va.writing <- ggplot() + 
  geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = Writing.SOL), color= "black", size = 0.25) + 
  coord_map()
va.history <- ggplot() + 
  geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = History.SOL), color= "black", size = 0.25) + 
  coord_map()
va.math <- ggplot() + 
  geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = Math.SOL), color= "black", size = 0.25) + 
  coord_map()
va.science <- ggplot() + 
  geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = Science.SOL), color= "black", size = 0.25) + 
  coord_map()
grid.arrange(va.reading, va.writing, va.history, va.math, va.science, nrow=2)







