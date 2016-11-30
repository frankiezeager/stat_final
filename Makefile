
all: data_merge.csv

data_merge.csv: acs_wrangling.csv education_data_wrangling.csv
	Rscript data_merge.R

acs_wrangling.csv: acs_acquisition.csv
	Rscript acs_wrangling.R

acs_acquisition.csv:
	Rscript acs_acquisition.R

education_data_wrangling.csv: 2012-13_science_3-8.csv
	Rscript education_data_wrangling.R

2012-13_science_3-8.csv:
	unzip -o \*.zip

clean:
	rm *.csv
