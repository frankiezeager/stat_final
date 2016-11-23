library(acs)
acs.tables.install()
library(dplyr)
library(purrr)
library(data.table)
api.key.install(key = "0faa650effb8df6132c2ce382d5af92b827da336")

div <- c("Accomack County" ,                
 "Albemarle County"               ,"Alexandria City"              ,     
 "Alleghany County"               ,"Amelia County"                , 
 "Amherst County"                 ,"Appomattox County"            , 
 "Arlington County"               ,"Augusta County"               , 
 "Bath County"                    ,"Bedford County"               , 
 "Bland County"                   ,"Botetourt County"             , 
 "Bristol City"                   ,"Brunswick County"             , 
 "Buchanan County"                ,"Buckingham County"            , 
 "Buena Vista City"               ,"Campbell County"              , 
 "Caroline County"                ,"Carroll County"               , 
 "Charles City County"            ,"Charlotte County"             , 
 "Charlottesville City"           ,"Chesapeake City"              , 
 "Chesterfield County"            ,"Clarke County"                , 
 "Colonial Beach"                 ,"Colonial Heights City"        , 
 "Covington City"                 ,"Craig County"                 , 
 "Culpeper County"                ,"Cumberland County"            , 
 "Danville City"                  ,"Dickenson County"             , 
 "Dinwiddie County"               ,"Essex County"                 , 
 "Fairfax County"                 ,"Falls Church City"            , 
 "Fauquier County"                ,"Floyd County"                 , 
 "Fluvanna County"                ,"Franklin City"                , 
 "Franklin County"                ,"Frederick County"             , 
 "Fredericksburg City"            ,"Galax City"                   , 
 "Giles County"                   ,"Gloucester County"            , 
 "Goochland County"               ,"Grayson County"               , 
 "Greene County"                  ,"Greensville County"           , 
 "Halifax County"                 ,"Hampton City"                 , 
 "Hanover County"                 ,"Harrisonburg City"            , 
 "Henrico County"                 ,"Henry County"                 , 
 "Highland County"                ,"Hopewell City"                , 
 "Isle of Wight County"           ,"King and Queen County"        , 
 "King George County"             ,"King William County"          , 
 "Lancaster County"               ,"Lee County"                   , 
 "Lexington City"                 ,"Loudoun County"               , 
 "Louisa County"                  ,"Lunenburg County"             , 
 "Lynchburg City"                 ,"Madison County"               , 
 "Manassas City"                  ,"Manassas Park City"           , 
 "Martinsville City"              ,"Mathews County"               , 
 "Mecklenburg County"             ,"Middlesex County"             , 
 "Montgomery County"              ,"Nelson County"                , 
 "New Kent County"                ,"Newport News City"            , 
 "Norfolk City"                   ,"Northampton County"           , 
 "Northumberland County"          ,"Norton City"                  , 
 "Nottoway County"                ,"Orange County"                , 
 "Page County"                    ,"Patrick County"               , 
 "Petersburg City"                ,"Pittsylvania County"          , 
 "Poquoson City"                  ,"Portsmouth City"              , 
 "Powhatan County"                ,"Prince Edward County"         , 
 "Prince George County"           ,"Prince William County"        , 
 "Pulaski County"                 ,"Radford City"                 , 
 "Rappahannock County"            ,"Richmond City"                , 
 "Richmond County"                ,"Roanoke City"                 , 
 "Roanoke County"                 ,"Rockbridge County"            , 
 "Rockingham County"              ,"Russell County"               , 
 "Salem City"                     ,"Scott County"                 , 
 "Shenandoah County"              ,"Smyth County"                 , 
 "Southampton County"             ,"Spotsylvania County"          , 
 "Stafford County"                ,"Staunton City"                , 
 "Suffolk City"                   ,"Surry County"                 , 
 "Sussex County"                  ,"Tazewell County"              , 
 "Virginia Beach City"            ,"Warren County"                , 
 "Washington County"              ,"Waynesboro City"              , 
 "West Point"                     ,"Westmoreland County"          , 
 "Williamsburg-James City County"  ,"Winchester City"              , 
 "Wise County"                    ,"Wythe County"                 , 
 "York County") %>% as.list()

# Counties and Cities BREAKDOWN
counties <- div[-c(129,128,126,125,122,118,117,110,105,103,101,95,94,92,87,84,83,76,75,74,72,68,61,57,55,47,46,43,39,34,30,29,28,25,24,18,14,3)]
cities <- div[c(129,128,126,125,122,118,117,110,105,103,101,95,94,92,87,84,83,76,75,74,72,68,61,57,55,47,46,43,39,34,30,29,28,25,24,18,14,3)]

# Make geo codes
geos.counties <- counties %>% map(function(s){
  geo.make(state="VA", county= s)})
geos.cities <- cities %>%  map(function(s){
  geo.make(state="VA", msa= s)})


# Grab the interested variables
keywords <- c("income", "household", "education", "age", "population", "language")

# ACS Table Numbers
table.nums <- keywords %>% map(function(s){
  acs.lookup(keyword=s, endyear=2013, case.sensitive=F) %@% 
    "results" %>% 
    get("table.number",.)}) %>% 
  unlist() %>% 
  unique()

# ACS Table Names
table.names <- keywords %>% map(function(s){
  acs.lookup(keyword=s, endyear=2013, case.sensitive=F) %@% 
    "results" %>% 
    get("table.name",.)}) %>% 
  unlist() %>% 
  unique()


# Do a test with geos.cities
test1 <- geos.counties %>% map(function(s){
  acs.fetch(endyear=2013, table.number = "B01003", geography = s, case.sensitive=F) %@%
    "estimate" 
}) %>% 
  do.call("rbind",.) %>% 
  as.data.frame()

# got an error here
test2 <- geos.cities %>% map(function(s){
  acs.fetch(endyear=2013, table.number = "B01003", geography = s, case.sensitive=F) %@%
    "estimate" 
}) %>% 
  do.call("rbind",.) %>% 
  as.data.frame()

test <- rbind(test,test2)






