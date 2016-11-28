# STAT6021 Final Project

# Generating Full Testing Data for each county

Generate full SOL testing data for each county by:

 * Extracting all zip archives to the root directory (on OS X, `unzip \*.zip`)
 * Run `data_wrangling_final_stat.R`

`full_data.csv` will be written to the root directory.

Generate full school district demographi data from the American Community Survey (ACS) by:

* Open `FinalProject_lm.R`, being sure to install all the packages at the start, and retreive an active API
* Go down to the end of the file and change the file="" in the write.scv() command
* Run the code: may take a few minutes

`acs_data.csv` will be written to whatever directory location you assign
