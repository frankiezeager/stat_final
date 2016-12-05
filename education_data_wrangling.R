#loading data
reading<-read.csv('2012-13_english_writing_3-8.csv')
writing<-read.csv('2012-13_english_writing_3-8.csv')
history<-read.csv('2012-13_history_3-8.csv')
math<-read.csv('2012-13_math_3-8.csv')
science<-read.csv('2012-13_science_3-8.csv')

suppressPackageStartupMessages({
  library(dplyr)
})

#convert categorical variables into a series of dummy variables
#reading
for(level in unique(reading$GENDER)){
  reading[paste("gender_dummy", level, sep = "_")] <- ifelse(reading$GENDER == level, 1, 0)
}

for(level in unique(reading$FEDERAL_RACE_CODE)){
  reading[paste("federal_race_code", level, sep = "_")] <- ifelse(reading$FEDERAL_RACE_CODE == level, 1, 0)
}

for(level in unique(reading$DISABILITY_FLAG)){
  reading[paste("disability_flag", level, sep = "_")] <- ifelse(reading$DISABILITY_FLAG == level, 1, 0)
}
for(level in unique(reading$LEP_FLAG)){
  reading[paste("lep_flag", level, sep = "_")] <- ifelse(reading$LEP_FLAG == level, 1, 0)
}

for(level in unique(reading$DISADVANTAGED_FLAG)){
  reading[paste("disadvantaged_flag", level, sep = "_")] <- ifelse(reading$DISADVANTAGED_FLAG == level, 1, 0)
}


#writing
for(level in unique(writing$GENDER)){
  writing[paste("gender_dummy", level, sep = "_")] <- ifelse(writing$GENDER == level, 1, 0)
}

for(level in unique(writing$FEDERAL_RACE_CODE)){
  writing[paste("federal_race_code", level, sep = "_")] <- ifelse(writing$FEDERAL_RACE_CODE == level, 1, 0)
}

for(level in unique(writing$DISABILITY_FLAG)){
  writing[paste("disability_flag", level, sep = "_")] <- ifelse(writing$DISABILITY_FLAG == level, 1, 0)
}
for(level in unique(writing$LEP_FLAG)){
  writing[paste("lep_flag", level, sep = "_")] <- ifelse(writing$LEP_FLAG == level, 1, 0)
}

for(level in unique(writing$DISADVANTAGED_FLAG)){
  writing[paste("disadvantaged_flag", level, sep = "_")] <- ifelse(writing$DISADVANTAGED_FLAG == level, 1, 0)
}

#history
for(level in unique(history$GENDER)){
  history[paste("gender_dummy", level, sep = "_")] <- ifelse(history$GENDER == level, 1, 0)
}

for(level in unique(history$FEDERAL_RACE_CODE)){
  history[paste("federal_race_code", level, sep = "_")] <- ifelse(history$FEDERAL_RACE_CODE == level, 1, 0)
}

for(level in unique(history$DISABILITY_FLAG)){
  history[paste("disability_flag", level, sep = "_")] <- ifelse(history$DISABILITY_FLAG == level, 1, 0)
}
for(level in unique(history$LEP_FLAG)){
  history[paste("lep_flag", level, sep = "_")] <- ifelse(history$LEP_FLAG == level, 1, 0)
}

for(level in unique(history$DISADVANTAGED_FLAG)){
  history[paste("disadvantaged_flag", level, sep = "_")] <- ifelse(history$DISADVANTAGED_FLAG == level, 1, 0)
}

#math
for(level in unique(math$GENDER)){
  math[paste("gender_dummy", level, sep = "_")] <- ifelse(math$GENDER == level, 1, 0)
}

for(level in unique(math$FEDERAL_RACE_CODE)){
  math[paste("federal_race_code", level, sep = "_")] <- ifelse(math$FEDERAL_RACE_CODE == level, 1, 0)
}

for(level in unique(math$DISABILITY_FLAG)){
  math[paste("disability_flag", level, sep = "_")] <- ifelse(math$DISABILITY_FLAG == level, 1, 0)
}
for(level in unique(math$LEP_FLAG)){
  math[paste("lep_flag", level, sep = "_")] <- ifelse(math$LEP_FLAG == level, 1, 0)
}

for(level in unique(math$DISADVANTAGED_FLAG)){
  math[paste("disadvantaged_flag", level, sep = "_")] <- ifelse(math$DISADVANTAGED_FLAG == level, 1, 0)
}

#science
for(level in unique(science$GENDER)){
  science[paste("gender_dummy", level, sep = "_")] <- ifelse(science$GENDER == level, 1, 0)
}

for(level in unique(science$FEDERAL_RACE_CODE)){
  science[paste("federal_race_code", level, sep = "_")] <- ifelse(science$FEDERAL_RACE_CODE == level, 1, 0)
}

for(level in unique(science$DISABILITY_FLAG)){
  science[paste("disability_flag", level, sep = "_")] <- ifelse(science$DISABILITY_FLAG == level, 1, 0)
}
for(level in unique(science$LEP_FLAG)){
  science[paste("lep_flag", level, sep = "_")] <- ifelse(science$LEP_FLAG == level, 1, 0)
}

for(level in unique(science$DISADVANTAGED_FLAG)){
  science[paste("disadvantaged_flag", level, sep = "_")] <- ifelse(science$DISADVANTAGED_FLAG == level, 1, 0)
}

#group by county name (called div_name in the df), and average all of the values for each county
reading %>% group_by(DIV_NAME)%>% summarise(reading.AVG_SOL_SCALE_SCORE=mean(AVG_SOL_SCALE_SCORE),
                                            reading.PASS_ADVANCED_RATE=mean(PASS_ADVANCED_RATE), reading.PASS_PROF_RATE=mean(PASS_PROF_RATE), reading.PASS_RATE=mean(PASS_RATE),
                                            reading.FAIL_RATE=mean(FAIL_RATE), reading.num_female=mean(gender_dummy_F), reading.num_frc_3=mean(federal_race_code_3,na.rm=TRUE), reading.num_frc_4=mean(federal_race_code_4,na.rm=TRUE),
                                            reading.num_frc_5=mean(federal_race_code_5,na.rm=TRUE), reading.num_frc_NA=mean(federal_race_code_NA,na.rm=TRUE), reading.num_frc_99=mean(federal_race_code_99,na.rm=TRUE), reading.num_frc_2=mean(federal_race_code_2,na.rm=TRUE),
                                            reading.num_frc_1=mean(federal_race_code_1,na.rm=TRUE),reading.num_frc_6=mean(federal_race_code_6,na.rm=TRUE), reading.disability_flag=mean(disability_flag_Y),reading.lep_flag=mean(lep_flag_Y),
                                            reading.disadvantaged_flag=mean(disadvantaged_flag_Y))->reading.grouped


writing %>% group_by(DIV_NAME)%>% summarise(writing.AVG_SOL_SCALE_SCORE=mean(AVG_SOL_SCALE_SCORE),
                                            writing.PASS_ADVANCED_RATE=mean(PASS_ADVANCED_RATE), writing.PASS_PROF_RATE=mean(PASS_PROF_RATE), writing.PASS_RATE=mean(PASS_RATE),
                                            writing.FAIL_RATE=mean(FAIL_RATE), writing.num_female=mean(gender_dummy_F), writing.num_frc_3=mean(federal_race_code_3,na.rm=TRUE), writing.num_frc_4=mean(federal_race_code_4,na.rm=TRUE),
                                            writing.num_frc_5=mean(federal_race_code_5,na.rm=TRUE), writing.num_frc_NA=mean(federal_race_code_NA,na.rm=TRUE), writing.num_frc_99=mean(federal_race_code_99,na.rm=TRUE), writing.num_frc_2=mean(federal_race_code_2,na.rm=TRUE),
                                            writing.num_frc_1=mean(federal_race_code_1,na.rm=TRUE),writing.num_frc_6=mean(federal_race_code_6,na.rm=TRUE), writing.disability_flag=mean(disability_flag_Y),writing.lep_flag=mean(lep_flag_Y),
                                            writing.disadvantaged_flag=mean(disadvantaged_flag_Y))->writing.grouped

history %>% group_by(DIV_NAME)%>% summarise(history.AVG_SOL_SCALE_SCORE=mean(AVG_SOL_SCALE_SCORE),
                                            history.PASS_ADVANCED_RATE=mean(PASS_ADVANCED_RATE), history.PASS_PROF_RATE=mean(PASS_PROF_RATE), history.PASS_RATE=mean(PASS_RATE),
                                            history.FAIL_RATE=mean(FAIL_RATE), history.num_female=mean(gender_dummy_F), history.num_frc_3=mean(federal_race_code_3,na.rm=TRUE), history.num_frc_4=mean(federal_race_code_4,na.rm=TRUE),
                                            history.num_frc_5=mean(federal_race_code_5,na.rm=TRUE), history.num_frc_NA=mean(federal_race_code_NA,na.rm=TRUE), history.num_frc_99=mean(federal_race_code_99,na.rm=TRUE), history.num_frc_2=mean(federal_race_code_2,na.rm=TRUE),
                                            history.num_frc_1=mean(federal_race_code_1,na.rm=TRUE),history.num_frc_6=mean(federal_race_code_6,na.rm=TRUE), history.disability_flag=mean(disability_flag_Y),history.lep_flag=mean(lep_flag_Y),
                                            history.disadvantaged_flag=mean(disadvantaged_flag_Y))->history.grouped

math %>% group_by(DIV_NAME)%>% summarise(math.AVG_SOL_SCALE_SCORE=mean(AVG_SOL_SCALE_SCORE),
                                            math.PASS_ADVANCED_RATE=mean(PASS_ADVANCED_RATE), math.PASS_PROF_RATE=mean(PASS_PROF_RATE), math.PASS_RATE=mean(PASS_RATE),
                                            math.FAIL_RATE=mean(FAIL_RATE), math.num_female=mean(gender_dummy_F), math.num_frc_3=mean(federal_race_code_3,na.rm=TRUE), math.num_frc_4=mean(federal_race_code_4,na.rm=TRUE),
                                            math.num_frc_5=mean(federal_race_code_5,na.rm=TRUE), math.num_frc_NA=mean(federal_race_code_NA,na.rm=TRUE), math.num_frc_99=mean(federal_race_code_99,na.rm=TRUE), math.num_frc_2=mean(federal_race_code_2,na.rm=TRUE),
                                            math.num_frc_1=mean(federal_race_code_1,na.rm=TRUE),math.num_frc_6=mean(federal_race_code_6,na.rm=TRUE), math.disability_flag=mean(disability_flag_Y),math.lep_flag=mean(lep_flag_Y),
                                            math.disadvantaged_flag=mean(disadvantaged_flag_Y))->math.grouped

science %>% group_by(DIV_NAME)%>% summarise(science.AVG_SOL_SCALE_SCORE=mean(AVG_SOL_SCALE_SCORE),
                                            science.PASS_ADVANCED_RATE=mean(PASS_ADVANCED_RATE), science.PASS_PROF_RATE=mean(PASS_PROF_RATE), science.PASS_RATE=mean(PASS_RATE),
                                            science.FAIL_RATE=mean(FAIL_RATE), science.num_female=mean(gender_dummy_F), science.num_frc_3=mean(federal_race_code_3,na.rm=TRUE), science.num_frc_4=mean(federal_race_code_4,na.rm=TRUE),
                                            science.num_frc_5=mean(federal_race_code_5,na.rm=TRUE), science.num_frc_NA=mean(federal_race_code_NA,na.rm=TRUE), science.num_frc_99=mean(federal_race_code_99,na.rm=TRUE), science.num_frc_2=mean(federal_race_code_2,na.rm=TRUE),
                                            science.num_frc_1=mean(federal_race_code_1,na.rm=TRUE),science.num_frc_6=mean(federal_race_code_6,na.rm=TRUE), science.disability_flag=mean(disability_flag_Y),science.lep_flag=mean(lep_flag_Y),
                                            science.disadvantaged_flag=mean(disadvantaged_flag_Y))->science.grouped

#merge all of the aggregated datasets into one dataframe
fully_merged<-merge(reading.grouped,writing.grouped, by="DIV_NAME")
fully_merged<-merge(fully_merged,history.grouped,by='DIV_NAME')
fully_merged<-merge(fully_merged,math.grouped, by='DIV_NAME')
fully_merged<-merge(fully_merged,science.grouped,by='DIV_NAME')

write.csv(fully_merged,'education_data_wrangling.csv')

#writing<-merge(writing,writing,by='DIV_NAME')
