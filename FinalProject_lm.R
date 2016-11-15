install.packages("acs")
library(acs)
acs.tables.install()
library(dplyr)
library(purrr)

counties <- c("Accomack County",
              "Albemarle County",
              "Alleghany County",
              "Amelia County",
              "Amherst County",
              "Appomattox County",
              "Arlington County",
              "Augusta County",
              "Bath County",
              "Bedford County",
              "Bland County",
              "Botetourt County",
              "Brunswick County",
              "Buchanan County",
              "Buckingham County",
              "Campbell County",
              "Caroline County",
              "Carroll County",
              "Charles City County",
              "Charlotte County",
              "Chesterfield County",
              "Clarke County",
              "Craig County",
              "Culpeper County",
              "Cumberland County",
              "Dickenson County",
              "Dinwiddie County",
              "Essex County",
              "Fairfax County",
              "Fauquier County",
              "Floyd County",
              "Fluvanna County",
              "Franklin County",
              "Frederick County",
              "Giles County",
              "Gloucester County",
              "Goochland County",
              "Grayson County",
              "Greene County",
              "Greensville County",
              "Halifax County",
              "Hanover County",
              "Henrico County",
              "Henry County",
              "Highland County",
              "Isle of Wight County",
              "James City County",
              "King and Queen County",
              "King George County",
              "King William County",
              "Lancaster County",
              "Lee County",
              "Loudoun County",
              "Louisa County",
              "Lunenburg County",
              "Madison County",
              "Mathews County",
              "Mecklenburg County",
              "Middlesex County",
              "Montgomery County",
              "Nelson County",
              "New Kent County",
              "Northampton County",
              "Northumberland County",
              "Nottoway County",
              "Orange County",
              "Page County",
              "Patrick County",
              "Pittsylvania County",
              "Powhatan County",
              "Prince Edward County",
              "Prince George County",
              "Prince William County",
              "Pulaski County",
              "Rappahannock County",
              "Richmond County",
              "Roanoke County",
              "Rockbridge County",
              "Rockingham County",
              "Russell County",
              "Scott County",
              "Shenandoah County",
              "Smyth County",
              "Southampton County",
              "Spotsylvania County",
              "Stafford County",
              "Surry County",
              "Sussex County",
              "Tazewell County",
              "Warren County",
              "Washington County",
              "Westmoreland County",
              "Wise County",
              "Wythe County",
              "York County",
              "Alexandria city",
              "Bristol city",
              "Buena Vista city",
              "Charlottesville city",
              "Chesapeake city",
              "Colonial Heights city",
              "Covington city",
              "Danville city",
              "Emporia city",
              "Fairfax city",
              "Falls Church city",
              "Franklin city",
              "Fredericksburg city",
              "Galax city",
              "Hampton city",
              "Harrisonburg city",
              "Hopewell city",
              "Lexington city",
              "Lynchburg city",
              "Manassas city",
              "Manassas Park city",
              "Martinsville city",
              "Newport News city",
              "Norfolk city",
              "Norton city",
              "Petersburg city",
              "Poquoson city",
              "Portsmouth city",
              "Radford city",
              "Richmond city",
              "Roanoke city",
              "Salem city",
              "Staunton city",
              "Suffolk city",
              "Virginia Beach city",
              "Waynesboro city",
              "Williamsburg city",
              "Winchester city")

geos <- counties %>% map(function(s){
  geo.make(state="VA", county= s)
})

# Grab the interested variables
keywords <- c("income", "household", "education", "age", "population", "crime", "language")

var.codes <- keywords %>% map(function(s){
  acs.lookup(keyword=s, endyear=2014, case.sensitive=F) %>% 
    get("results",.) %>% 
    get("variable.code",.)}) %>% unique()

var.names <- keywords %>% map(function(s){
  acs.lookup(keyword=s, endyear=2014, case.sensitive=F) %>% 
    get("results",.) %>% 
    get("variable.name",.)})

# Get an error
#Error in as.environment(pos) : 
#  S4 object does not extend class "environment" 




