###################################
############ DATA VIZ #############
###################################

full_data<-read.csv('data_merge.csv')

# Reassign family income variable and house worth
full_data$family_income_median_adj<-full_data$family_income_median/1000  #scale to thousands
full_data$house_worth_dollars_adj <- full_data$house_worth_dollars/1000000  #scale to millions

##########################################################################
# Some scatter plots
library(ggplot2)
library(gridExtra)
library(dplyr)
library(purrr)

theme <- theme(                              
  axis.title.x = element_text(face="bold", color="black", size=10),
  axis.title.y = element_text(face="bold", color="black", size=10),
  plot.title = element_text(face="bold", color = "black", size=12)
)

income.read <- ggplot(full_data, aes(x = family_income_median_adj, y = Reading.SOL)) +
  geom_point(aes(size=house_worth_dollars_adj)) +
  scale_size_continuous(name ="House Worth (millions $)", range=c(0.5,6), labels=waiver()) +
  theme_bw() + theme +
  labs(title="Reading SOL Scores vs Median Income", x="Median Family Income", y="Reading SOL Scores") +
  theme(legend.position=c(1,0),legend.justification=c(1,0)) 
ggsave(file.path("images", "income.read.png"), income.read)

partner.read <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Reading.SOL)) + 
  geom_point(aes(size=family_income_median_adj)) +
  scale_size_continuous(name ="Median Income (thousands $)", range=c(0.2,4)) +
  theme_bw() + theme +
  labs(title="Reading SOL Scores vs Presence of Unmarried Partner", x="% of Homes with Unmarried Partner Present (by county)", y="Reading SOL Scores") +
  theme(legend.position=c(1,1),legend.justification=c(1,1)) 
ggsave(file.path("images", "partner.read.png"), partner.read)

income.math <- ggplot(full_data, aes(x = family_income_median_adj, y = Math.SOL)) +
  geom_point(aes(size=house_worth_dollars_adj)) +
  scale_size_continuous(name ="House Worth (millions $)", range=c(0.5,6), labels=waiver()) +
  theme_bw() + theme +
  labs(title="Math SOL Scores vs Median Income", x="Median Family Income", y="Math SOL Scores") +
  theme(legend.position=c(1,0),legend.justification=c(1,0)) 
ggsave(file.path("images", "income.math.png"), income.math)

partner.math <- ggplot(full_data, aes(x = unmarried_partner_present_percent, y = Math.SOL)) +
  geom_point(aes(size=family_income_median_adj)) +
  scale_size_continuous(name ="Median Income (thousands $)", range=c(0.2,4)) +
  theme_bw() + theme +
  labs(title="Math SOL Scores vs Presence of Unmarried Partner", x="% of Homes with Unmarried Partner Present (by county)", y="Math SOL Scores") +
  theme(legend.position=c(1,1),legend.justification=c(1,1)) 
ggsave(file.path("images", "partner.math.png"), partner.math)


read.math <- ggplot(full_data, aes(x=Math.SOL, y=Reading.SOL)) + 
  geom_point(aes(size=house_worth_dollars_adj)) +
  scale_size_continuous(name ="House Worth (millions $)", range=c(0.5,6), labels=waiver()) +
  theme_bw() + theme +
  labs(title="Math vs Reading SOL Scores", x="Math SOL Scores", y="Reading SOL Scores") +
  theme(legend.position=c(1,0),legend.justification=c(1,0)) 
ggsave(file.path("images", "read.math.png"), read.math)

#################################################################################################

###### SOL SCORE MAPS ########
library(maptools)
library(ggmap)
library(RColorBrewer)
library(scales)
library(Cairo)
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
  scale_fill_distiller(name="Reading SOL", palette = "YlGn", breaks = pretty_breaks(n = 5), trans="reverse")+
  theme_nothing(legend = TRUE) +
  ggtitle("Reading SOL Scores in Virginia") + 
  theme(plot.title = element_text(size = 12, face="bold"))

va.math <- ggplot() + 
  geom_polygon(data = virginia.shp, aes(x = long, y = lat, group = group, fill = Math.SOL), color= "black", size = 0.25) + 
  coord_map() +
  scale_fill_distiller(name="Math SOL", palette = "YlGn", breaks = pretty_breaks(n = 5), trans="reverse")+
  theme_nothing(legend = TRUE) +
  ggtitle("Math SOL Scores in Virginia") + 
  theme(plot.title = element_text(size = 12, face="bold"))

ggsave(file.path("images", "MathSOL.png"), va.math)
ggsave(file.path("images", "ReadingSOL.png"), va.reading)

