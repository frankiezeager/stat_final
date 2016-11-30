
acs_data_processed.csv: acs_data.csv
	Rscript filter_data.R

acs_data.csv: full_data.csv
	Rscript FinalProject_lm.R

full_data.csv: 2012-13_science_3-8.csv
	Rscript data_wrangling_final_stat.R

2012-13_science_3-8.csv:
	unzip -o \*.zip

clean:
	rm *.csv
