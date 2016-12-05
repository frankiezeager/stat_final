
all: MathSOL.png

MathSOL.png: data_merge.csv
	Rscript data_viz.R

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

clean-safe:
	ls *.csv | grep -v acs_acquisition.csv | xargs rm
	rm *.png
	rm *.pdf
