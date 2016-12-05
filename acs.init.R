list.of.packages <- c("acs", "dplyr", "purrr", "readr", "mice", "gridExtra", "ggplot2", "car", "maptools", "ggmap", "gpclib")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(acs)
api.key.install(key = "INSERT ACS API KEY http://api.census.gov/data/key_signup.html")
acs.tables.install()
