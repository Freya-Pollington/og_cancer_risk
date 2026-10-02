rm(list=ls())

#### Loading Packages ####

library(pacman)
pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI,mice,janitor)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)
#-----------------------------------------------------------------------------------------------------
# inner join the alcohol to each of the dataframes for cohort
dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.cohort_dysphagia_alc;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_dysphagia_alc 
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.cohort_dysphagia_cr_flag_nn_flag_sm_fin_elix ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.cohort_dyspepsia_alc;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_dyspepsia_alc 
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.cohort_dyspepsia_cr_flag_nn_flag_sm_fin_elix ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.cohort_vomiting_alc;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_vomiting_alc 
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.cohort_vomiting_cr_flag_nn_flag_sm_fin_elix ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.cohort_abd_pain_alc;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_abd_pain_alc 
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.cohort_abd_pain_cr_flag_nn_flag_sm_fin_elix ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;')

#---------------------------------------------------------------------------------------------
#Calculating BMI and categorising it using height and weight data, filling in with the obesity binary field where possible,
#and imputing the final rows (389 for hernia)

bmi_calc <- function(data,height_data,weight_data){
  #data <- dysphagia
  data_full_join <-left_join(data,weight, by = 'e_patid')%>%
    mutate(eventdate.x = as.Date(eventdate.x)) %>%
    mutate(eventdate.y = as.Date(eventdate.y))
  
  #sense checking weight values
  data_full_join_fil <- data_full_join %>%
    filter(data1 > 30 & data1 < 250) 
  
  #taking only values before the eventdate and then the most recent weight 
  #and one per person with a random selection if more than one on one day
  data_full_join_2 <- data_full_join_fil %>%
    mutate(previous_weight = ifelse(eventdate.y <= eventdate.x,1,0))%>%
    filter(previous_weight == 1) %>%
    group_by(e_patid) %>%
    slice_max(order_by = eventdate.y,n=1)%>%
    distinct()%>%
    sample_n(1) %>%
    select(-previous_weight)
  
  #finding those patients who did not have a valid weight before the eventdate
  data_full_join_anti <- data_full_join %>%
    anti_join(data_full_join_2,by='e_patid') %>%
    group_by(e_patid) %>%
    sample_n(1) 
  
  #joining the patients without a valid weight into the main dataframe
  data_full_join_3 <- data_full_join_2 %>%
    union(data_full_join_anti) %>%
    rename(weight = data1)
  
  #joining most recent height information and calculating bmi
  #creating a bmi category variable for obese (3), overweight (2), underweight (1), and healthy weight (0)
  data_full_join_4 <- data_full_join_3 %>%
    left_join(height_fil,by='e_patid') %>%
    mutate(height = as.numeric(height)) %>%
    mutate(weight = as.numeric(weight)) %>%
    mutate(bmi = (weight/(height*height))) %>%
    mutate(bmi_cat = ifelse(bmi>=30,3,
                            ifelse(bmi<30 & bmi >= 25,2,
                                   ifelse(bmi<25 & bmi>=18.5,0,
                                          ifelse(bmi<18.5,1,NA))))) %>%
    mutate(bmi_cat = ifelse(is.na(bmi_cat) & ever_obesity == 1, 3,ifelse(is.na(bmi_cat) & ever_obesity == 0,NA,bmi_cat))) #%>%
    #select(-c(yob_approx,diagnosisdatebest2,eventdate))
  
  #imputing the remaining bmi values
  data_full_join_prep <- data_full_join_4 %>% ungroup() %>%
    select(-c(symptom_desc,readcode_desc,height,weight,bmi,eventdate.y)) %>%
    mutate(bmi_cat = as.factor(bmi_cat))
  
  set.seed(123)
  data_impute <- mice(data_full_join_prep,method='cart')
  
  data_impute_complete <- complete(data_impute,1)
}

#---------------------------------------------------
#loading height and weight data
height <- readRDS('S://ECHO_IHI_CPRD//Data//Yangfan//pat_basic_data//all_height.rds')

weight <- readRDS('S://ECHO_IHI_CPRD//Data//Yangfan//pat_basic_data//all_weight.rds')

height <- height %>% arrange((data1))

#finding most recent height and taking one per patient
height_fil <- height %>%
  group_by(e_patid) %>%
  filter(data1 > 1.45 & data1 < 2.25) %>% #sense checking height
  mutate(most_recent_height = max(eventdate))%>%
  mutate(most_recent_height_flag = ifelse(most_recent_height == eventdate,1,0)) %>%
  mutate(height = ifelse(most_recent_height_flag == 1, data1, NA)) %>%
  filter(!is.na(height)) %>%
  select(e_patid,eventdate,height) %>%
  distinct() %>%
  sample_n(1)

#loading cohort data
query <- 'SELECT * FROM freya.cohort_abd_pain_cr_flag_nn_flag_sm_fin_elix'
rs <- dbSendQuery(db, query)
abd_pain <- fetch(rs, n=-1)

abd_pain <- clean_names(abd_pain)

query <- 'SELECT * FROM freya.cohort_dyspepsia_cr_flag_nn_flag_sm_fin_elix'
rs <- dbSendQuery(db, query)
dyspepsia <- fetch(rs, n=-1)

dyspepsia <- clean_names(dyspepsia)

query <- 'SELECT * FROM freya.cohort_dysphagia_cr_flag_nn_flag_sm_fin_elix'
rs <- dbSendQuery(db, query)
dysphagia <- fetch(rs, n=-1)

dysphagia <- clean_names(dysphagia)

query <- 'SELECT * FROM freya.cohort_vomiting_cr_flag_nn_flag_sm_fin_elix'
rs <- dbSendQuery(db, query)
vomiting <- fetch(rs, n=-1)

vomiting <- clean_names(vomiting)

#remove the td variables now I have extracted the data, only keeping the flag for if same day
abd_pain <- abd_pain %>%
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc > 0 | is.na(td_oesoph_ulc),0,1),
         same_day_gord = ifelse(td_gord > 0 | is.na(td_gord),0,1),
         same_day_barretts = ifelse(td_barretts > 0 | is.na(td_barretts),0,1),
         same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis > 0 | is.na(td_gastritis_duodenitis),0,1),
         same_day_hernia_abdo = ifelse(td_hernia_abdo > 0 | is.na(td_hernia_abdo),0,1)) %>%
  select(-c(td_dysphagia,td_dyspepsia,td_vomiting,td_oesoph_ulc,td_gord,td_barretts,td_gastritis_duodenitis,td_hernia_abdo))

dyspepsia <- dyspepsia %>%
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc > 0 | is.na(td_oesoph_ulc),0,1),
         same_day_gord = ifelse(td_gord > 0 | is.na(td_gord),0,1),
         same_day_barretts = ifelse(td_barretts > 0 | is.na(td_barretts),0,1),
         same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis > 0 | is.na(td_gastritis_duodenitis),0,1),
         same_day_hernia_abdo = ifelse(td_hernia_abdo > 0 | is.na(td_hernia_abdo),0,1)) %>%
  select(-c(td_dysphagia,td_abd_pain,td_vomiting,td_oesoph_ulc,td_gord,td_barretts,td_gastritis_duodenitis,td_hernia_abdo))

dysphagia <- dysphagia %>%
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc > 0 | is.na(td_oesoph_ulc),0,1),
         same_day_gord = ifelse(td_gord > 0 | is.na(td_gord),0,1),
         same_day_barretts = ifelse(td_barretts > 0 | is.na(td_barretts),0,1),
         same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis > 0 | is.na(td_gastritis_duodenitis),0,1),
         same_day_hernia_abdo = ifelse(td_hernia_abdo > 0 | is.na(td_hernia_abdo),0,1)) %>%
  select(-c(td_abd_pain,td_dyspepsia,td_vomiting,td_oesoph_ulc,td_gord,td_barretts,td_gastritis_duodenitis,td_hernia_abdo))

vomiting <- vomiting %>%
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc > 0 | is.na(td_oesoph_ulc),0,1),
         same_day_gord = ifelse(td_gord > 0 | is.na(td_gord),0,1),
         same_day_barretts = ifelse(td_barretts > 0 | is.na(td_barretts),0,1),
         same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis > 0 | is.na(td_gastritis_duodenitis),0,1),
         same_day_hernia_abdo = ifelse(td_hernia_abdo > 0 | is.na(td_hernia_abdo),0,1)) %>%
  select(-c(td_dysphagia,td_dyspepsia,td_abd_pain,td_oesoph_ulc,td_gord,td_barretts,td_gastritis_duodenitis,td_hernia_abdo))

abd_pain_complete <- bmi_calc(abd_pain,height_fil,weight)
dyspepsia_complete <- bmi_calc(dyspepsia,height_fil,weight)
dysphagia_complete <- bmi_calc(dysphagia,height_fil,weight)
vomiting_complete <- bmi_calc(vomiting,height_fil,weight)

dbWriteTable(db, "cohort_abd_pain_cr_flag_nn_flag_sm_fin_elix_bmi",abd_pain_complete, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_dyspepsia_cr_flag_nn_flag_sm_fin_elix_bmi",dyspepsia_complete, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_dysphagia_cr_flag_nn_flag_sm_fin_elix_bmi",dysphagia_complete, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_vomiting_cr_flag_nn_flag_sm_fin_elix_bmi",vomiting_complete, row.names = FALSE, overwrite = TRUE)

#------------------------------
#function to process alcohol information and make a factor variable for alcohol status

alc_fun <- function(data_alcohol,data_complete){
data_alcohol_fil <- data_alcohol %>%
  group_by(e_patid) %>%
  mutate(alc_eventdate = as.Date(alc_eventdate)) %>%
  mutate(most_recent_date = max(alc_eventdate))%>%
  mutate(most_recent_flag = ifelse(most_recent_date == alc_eventdate,1,0)) %>%
  mutate(alc_status = ifelse(most_recent_flag == 1, data1, NA)) %>%
  filter(!is.na(alc_status))%>%
  select(-c(data1,alc_eventdate,most_recent_date,most_recent_flag))

#majority of people have more than one status on each day
data_n <- data_alcohol_fil %>%
  count(e_patid)

#need to filter by most recent alcohol status for those with more than one
#same method as applied to smoking
data_alcohol_fil2 <- data_alcohol_fil %>%
  group_by(e_patid) %>%
  #re-structuring the alcohol levels to make more sense i.e. 3 is yes, 2 is ex, 1 is no, 0 is NA
  mutate(alc_status = ifelse(alc_status == 1, 3, 
                             ifelse(alc_status == 3, 2,
                                    ifelse(alc_status == 2, 1,0)))) %>%
  slice_max(alc_status)

#join into the main dataframe
data_complete_join <- data_complete %>%
  left_join(data_alcohol_fil2, by='e_patid') %>%
  mutate(alc_status_comb = ifelse(ever_alcohol_problems == 1 & alc_status == 3, 5, #drinker with a history of alcohol problems
                                  ifelse(ever_alcohol_problems == 0 & alc_status == 3, 4, #drinker with no history of alcohol problems
                                         ifelse(ever_alcohol_problems == 1 & alc_status == 2, 3, #ex drinker with history of alcohol problems
                                                ifelse(ever_alcohol_problems == 0 & alc_status == 2, 2, #ex drinker with no history of alcohol problems
                                                       ifelse(ever_alcohol_problems == 1 & alc_status == 1, 3, #non-drinker with history of alcohol problems (setting them as an ex drinker with history)
                                                              ifelse(ever_alcohol_problems == 0 & alc_status == 1, 1,NA)))))))#non-drinker with no history of alcohol problems

  
#imputing the remaining alcohol values
data_complete_join_prep <- data_complete_join %>% ungroup() %>%
  select(-c(eventdate.x.x,eventdate.y,alc_status,eventdate.y)) %>%
  mutate(alc_status_comb = as.factor(alc_status_comb)) %>%
  rename(eventdate = eventdate.x)

set.seed(123)
data_impute <- mice(data_complete_join_prep,method='cart')

data_impute_complete <- complete(data_impute,1)

}

query <- 'SELECT * FROM cohort_dysphagia_alc'
rs <- dbSendQuery(db, query)
dysphagia_alc <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_dyspepsia_alc'
rs <- dbSendQuery(db, query)
dyspepsia_alc <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_vomiting_alc'
rs <- dbSendQuery(db, query)
vomiting_alc <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_abd_pain_alc'
rs <- dbSendQuery(db, query)
abd_pain_alc <- fetch(rs, n=-1)

abd_pain_fin <- alc_fun(abd_pain_alc,abd_pain_complete)
dyspepsia_fin <- alc_fun(dyspepsia_alc,dyspepsia_complete)
dysphagia_fin <- alc_fun(dysphagia_alc,dysphagia_complete)
vomiting_fin <- alc_fun(vomiting_alc,vomiting_complete)

dbWriteTable(db, "cohort_abd_pain_final",abd_pain_fin, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_dyspepsia_final",dyspepsia_fin, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_dysphagia_final",dysphagia_fin, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_vomiting_final",vomiting_fin, row.names = FALSE, overwrite = TRUE)


# Go to R script 2.4