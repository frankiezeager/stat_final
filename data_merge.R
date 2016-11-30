################################
########## Data Merge ##########
################################
# STAT 6021
# Final Project
# Tyler Hutcherson, Erik Langenborg, Frankie Zeager, Stephen Morris

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(mice)
  library(lattice)
  library(car)
})

## Read in the two data files
acs <- read.csv("acs_wrangling.csv", header=TRUE)
tests <- read.csv("education_data_wrangling.csv", header=TRUE) 
tests <- tests[-c(1),-c(1)]
tests$DIV_NAME <- paste(tests$DIV_NAME, 'Public Schools, Virginia', sep="")           

## The final list of school districts
school.districts <- c("Accomack County Public Schools, Virginia",   
                      "Albemarle County Public Schools, Virginia",
                      "Alexandria City Public Schools, Virginia",
                      "Alleghany County Public Schools, Virginia",
                      "Amelia County Public Schools, Virginia",
                      "Amherst County Public Schools, Virginia",
                      "Appomattox County Public Schools, Virginia",
                      "Arlington County Public Schools, Virginia",
                      "Augusta County Public Schools, Virginia",
                      "Bath County Public Schools, Virginia",
                      "Bedford County Public Schools, Virginia",
                      "Bland County Public Schools, Virginia",
                      "Botetourt County Public Schools, Virginia",
                      "Bristol City Public Schools, Virginia",
                      "Brunswick County Public Schools, Virginia",
                      "Buchanan County Public Schools, Virginia",
                      "Buckingham County Public Schools, Virginia",
                      "Buena Vista City Public Schools, Virginia",
                      "Campbell County Public Schools, Virginia",
                      "Caroline County Public Schools, Virginia",
                      "Carroll County Public Schools, Virginia",
                      "Charles City County Public Schools, Virginia",
                      "Charlotte County Public Schools, Virginia",
                      "Charlottesville City Public Schools, Virginia",
                      "Chesapeake City Public Schools, Virginia",
                      "Chesterfield County Public Schools, Virginia",
                      "Clarke County Public Schools, Virginia",
                      "Colonial Beach Town Public Schools, Virginia",
                      "Colonial Heights City Public Schools, Virginia",
                      "Covington City Public Schools, Virginia",
                      "Craig County Public Schools, Virginia",
                      "Culpeper County Public Schools, Virginia",
                      "Cumberland County Public Schools, Virginia",
                      "Danville City Public Schools, Virginia",
                      "Dickenson County Public Schools, Virginia",
                      "Dinwiddie County Public Schools, Virginia",
                      "Essex County Public Schools, Virginia",
                      "Fairfax County Public Schools, Virginia",
                      "Falls Church City Public Schools, Virginia",
                      "Fauquier County Public Schools, Virginia",
                      "Floyd County Public Schools, Virginia",
                      "Fluvanna County Public Schools, Virginia",
                      "Franklin City Public Schools, Virginia",
                      "Franklin County Public Schools, Virginia",
                      "Frederick County Public Schools, Virginia",
                      "Fredericksburg City Public Schools, Virginia",
                      "Galax City Public Schools, Virginia",
                      "Giles County Public Schools, Virginia",
                      "Gloucester County Public Schools, Virginia",
                      "Goochland County Public Schools, Virginia",
                      "Grayson County Public Schools, Virginia",
                      "Greene County Public Schools, Virginia",
                      "Greensville County Public Schools, Virginia",
                      "Halifax County Public Schools, Virginia",
                      "Hampton City Public Schools, Virginia",
                      "Hanover County Public Schools, Virginia",
                      "Harrisonburg City Public Schools, Virginia",
                      "Henrico County Public Schools, Virginia",
                      "Henry County Public Schools, Virginia",
                      "Highland County Public Schools, Virginia",
                      "Hopewell City Public Schools, Virginia",
                      "Isle of Wight County Public Schools, Virginia",
                      "King and Queen County Public Schools, Virginia",
                      "King George County Public Schools, Virginia",
                      "King William County Public Schools, Virginia",
                      "Lancaster County Public Schools, Virginia",
                      "Lee County Public Schools, Virginia",
                      "Loudoun County Public Schools, Virginia",
                      "Louisa County Public Schools, Virginia",
                      "Lunenburg County Public Schools, Virginia",
                      "Lynchburg City Public Schools, Virginia",
                      "Madison County Public Schools, Virginia",
                      "Manassas City Public Schools, Virginia",
                      "Manassas Park City Public Schools, Virginia",
                      "Martinsville City Public Schools, Virginia",
                      "Mathews County Public Schools, Virginia",
                      "Mecklenburg County Public Schools, Virginia",
                      "Middlesex County Public Schools, Virginia",
                      "Montgomery County Public Schools, Virginia",
                      "Nelson County Public Schools, Virginia",
                      "New Kent County Public Schools, Virginia",
                      "Newport News City Public Schools, Virginia",
                      "Norfolk City Public Schools, Virginia",
                      "Northampton County Public Schools, Virginia",
                      "Northumberland County Public Schools, Virginia",
                      "Norton City Public Schools, Virginia",
                      "Nottoway County Public Schools, Virginia",
                      "Orange County Public Schools, Virginia",
                      "Page County Public Schools, Virginia",
                      "Patrick County Public Schools, Virginia",
                      "Petersburg City Public Schools, Virginia",
                      "Pittsylvania County Public Schools, Virginia",
                      "Poquoson City Public Schools, Virginia",
                      "Portsmouth City Public Schools, Virginia",
                      "Powhatan County Public Schools, Virginia",
                      "Prince Edward County Public Schools, Virginia",
                      "Prince George County Public Schools, Virginia",
                      "Prince William County Public Schools, Virginia",
                      "Pulaski County Public Schools, Virginia",
                      "Radford City Public Schools, Virginia",
                      "Rappahannock County Public Schools, Virginia",
                      "Richmond City Public Schools, Virginia",
                      "Richmond County Public Schools, Virginia",
                      "Roanoke City Public Schools, Virginia",
                      "Roanoke County Public Schools, Virginia",
                      "Rockbridge County Public Schools, Virginia",
                      "Rockingham County Public Schools, Virginia",
                      "Russell County Public Schools, Virginia",
                      "Salem City Public Schools, Virginia",
                      "Scott County Public Schools, Virginia",
                      "Shenandoah County Public Schools, Virginia",
                      "Smyth County Public Schools, Virginia",
                      "Southampton County Public Schools, Virginia",
                      "Spotsylvania County Public Schools, Virginia",
                      "Stafford County Public Schools, Virginia",
                      "Staunton City Public Schools, Virginia",
                      "Suffolk City Public Schools, Virginia",
                      "Surry County Public Schools, Virginia",
                      "Sussex County Public Schools, Virginia",
                      "Tazewell County Public Schools, Virginia",
                      "Virginia Beach City Public Schools, Virginia",
                      "Warren County Public Schools, Virginia",
                      "Washington County Public Schools, Virginia",
                      "Waynesboro City Public Schools, Virginia",
                      "West Point Town Public Schools, Virginia",
                      "Westmoreland County Public Schools, Virginia",
                      "Winchester City Public Schools, Virginia",
                      "Wise County Public Schools, Virginia",
                      "Wythe County Public Schools, Virginia",
                      "York County Public Schools, Virginia") %>% as.list()  

## Fix the county names and double check the list
tests$DIV_NAME[28] <- "Colonial Beach Town Public Schools, Virginia"
tests$DIV_NAME[126] <- "West Point Town Public Schools, Virginia"
tests <- tests[-c(68,128),]
as.list(tests$DIV_NAME) %in% school.districts %>% sum %>% `/`(length(school.districts)) #check to make sure
tests$school_district <- tests$DIV_NAME 
tests <- tests[,-c(1)]

## Merge the data frames
merged <- merge(tests, acs, by = intersect(names(tests),names(acs)), all.x = TRUE, all.y = FALSE)

## Check levels of missingness
check <- function(data){
  data %>%
  names %>%
  discard(function(colName){
    data[[colName]] %>% is.na %>% sum %>% `/`(nrow(data)) -> per
    per < 0.2 })
}

# check(merged) 
# "reading.num_frc_NA" "writing.num_frc_NA" "history.num_frc_NA" "math.num_frc_NA"    "science.num_frc_NA"

## Drop columns that have a missingness level greater than 20%
merged <- subset(merged, select = -reading.num_frc_NA) %>% 
  select(-writing.num_frc_NA) %>% 
  select(-history.num_frc_NA) %>% 
  select(-math.num_frc_NA) %>% 
  select(-science.num_frc_NA)

## Perform Multiple Imputation on the rest of the missing values
subset(merged, select = -c(1,3:17, 19:33, 35:49, 51:65, 67:90)) %>% 
  mice(m=20, method = "pmm", seed = 0123) -> merged.imp 
#subset(merged, select = -c(1,3:17, 19:33, 35:49, 51:65, 67:90)) %>% 
#  mice(m=20, method = "norm.predict", seed = 0123) -> merged.imp2 


## Fully imputed data set
complete(merged.imp,20) %>% 
  as.data.frame() %>% #impute one more time to catch some that got through
  mice(m=10, method = "pmm", seed = 0123) %>% 
  complete(10) %>% 
  as.data.frame() -> final
names(final) <- c("Reading.SOL","Writing.SOL","History.SOL","Math.SOL","Science.SOL")

final <- select(merged, -c(2,18,34,50,66)) %>% 
                  cbind(final,.) %>% 
                  select(c(6,1:5,7:90))
  
## Write to csv
write.csv(final, file="data_merge.csv")


