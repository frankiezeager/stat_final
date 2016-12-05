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
income.read <- income.read + geom_point(aes(size=house_worth_dollars_adj)) #this is neat
ggsave(file.path("images", "income.read.png"), income.read)


partner.read <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Reading.SOL))
# p3 <- partner.read + geom_point()
# p3
partner.read <- partner.read + geom_point(aes(size=family_income_median_adj))
ggsave(file.path("images", "partner.read.png"), partner.read)

#income.write <- ggplot(full_data, aes(x = family_income_median_adj, y = Writing.SOL))
#p5 <- income.write + geom_point(aes(size=house_worth_dollars_adj))
#p5

#partner.write <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Writing.SOL))
#p6 <- partner.write + geom_point(aes(size=family_income_median_adj))
#p6

#income.history <- ggplot(full_data, aes(x = family_income_median_adj, y = History.SOL))
#p7 <- income.history + geom_point(aes(size=house_worth_dollars_adj))
#p7

#partner.history <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = History.SOL))
#p8 <- partner.history + geom_point(aes(size=family_income_median_adj))
#p8

income.math <- ggplot(full_data, aes(x = family_income_median_adj, y = Math.SOL))
income.math <- income.math + geom_point(aes(size=house_worth_dollars_adj))
ggsave(file.path("images", "income.math.png"), income.math)


partner.math <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Math.SOL))
partner.math <- partner.math + geom_point(aes(size=family_income_median_adj))
ggsave(file.path("images", "partner.math.png"), partner.math)

#income.science <- ggplot(full_data, aes(x = family_income_median_adj, y = Science.SOL))
#p11 <- income.science + geom_point(aes(size=house_worth_dollars_adj))
#p11

#partner.science <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Science.SOL))
#p12 <- partner.science + geom_point(aes(size=family_income_median_adj))
#p12
##########################
#read.write <- ggplot(full_data, aes(x=Reading.SOL, y=Writing.SOL))
#t1 <- read.write + geom_point()
#t1

#math.science <- ggplot(full_data, aes(x=Math.SOL, y=Science.SOL))
#t2 <- math.science + geom_point()
#t2 <- t2 + geom_point(aes(size=house_worth_dollars_adj))
#t2

read.math <- ggplot(full_data, aes(x=Math.SOL, y=Reading.SOL)) + geom_point(aes(size=house_worth_dollars_adj))
ggsave(file.path("images", "read.math.png"), read.math)
#################################################################################################

###### SOL SCORE MAPS ########
library(maptools)
library(ggmap)
library(RColorBrewer)
library(scales)
set.seed(12345)
gpclibPermit()

full_data$id <- full_data$school_district

# Input shapefile and merge with necessary data
file.path("shapefiles", "tl_2014_51_unsd", "tl_2014_51_unsd.shp") %>%
  readShapeSpatial() %>%
  fortify(region = "NAME") %>%
  mutate(id = paste(id, ", Virginia", sep="")) %>%
  merge(full_data, by="id", all.y=TRUE) -> virginia.shp

# Order the rows
virginia.shp <- virginia.shp[order(virginia.shp$order),]

# Make the maps
va.reading <- ggplot() +
  geom_polygon(data = virginia.shp, aes(x = long, y = lat, group = group, fill = Reading.SOL), color= "black", size = 0.25) +
  coord_map() +
  labs(title="Reading SOL Scores in Virginia") +
  scale_fill_distiller(name="Reading SOL", palette = "YlGn", breaks = pretty_breaks(n = 5), trans="reverse")+
  theme_nothing(legend = TRUE)
va.math <- ggplot() +
  geom_polygon(data = virginia.shp, aes(x = long, y = lat, group = group, fill = Math.SOL), color= "black", size = 0.25) +
  coord_map() +
  labs(title="Math SOL Scores in Virginia") +
  scale_fill_distiller(name="Math SOL", palette = "YlGn", breaks = pretty_breaks(n = 5), trans="reverse")+
  theme_nothing(legend = TRUE)

map_plots <- arrangeGrob(va.reading, va.math, nrow=2)
# NOTE: http://stackoverflow.com/a/17075381/2601448
# TL;DR: grid.arrange draws directly on a device. arrangeGrob draw nothing and returns a grob g

ggsave(file.path("images", "MathSOL.png"), va.math)
ggsave(file.path("images", "ReadingSOL.png"), va.reading)

ggsave(file.path("images", "map_plots.png"), map_plots)
