# STAT6021 Final Project

# Initial Setup

All the packages must be installed and an active [Census API key](http://api.census.gov/data/key_signup.html) must be installed.

# Pipeline Usage

The Makefile contains the pipeline. To run the full set, execute `make` or `make clean && make all`.

# Manual Usage

Extract all zip archives to the root directory (on OS X, `unzip -o \*.zip`)

Generate full SOL testing data for each county by `Rscript data_wrangling_final_stat.R`. Or, in RStudio, open `data_wrangling_final_stat.R`, set the working directory to the project root (ie. Session -> Set Working Directory -> To Source File Location) and run the script. `full_data.csv` will be written to the project directory.

Generate full school district demographic data from the American Community Survey (ACS) by running `Rscript FinalProject_lm.R`. Or, in RStudio, open `FinalProject_lm.R`, set the working directory to the project root (ie. Session -> Set Working Directory -> To Source File Location) and run the script. `acs_data.csv` will be written to the project directory.
