rm(list=ls())

#### Loading Packages ####
library(pacman)

pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI,mice)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#---------------------------------------------------------------------------------------------
#Calculating BMI and categorising it using height and weight data, filling in with the obesity binary field where possible,
#and imputing the final rows (389 for hernia)

bmi_calc <- function(data,height_data,weight_data){

data_full_join <-left_join(data,weight, by = 'e_patid')

#sense checking weight values
data_full_join <- data_full_join %>%
  mutate(eventdate.x = as.Date(eventdate.x)) %>%
  mutate(eventdate.y = as.Date(eventdate.y))

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
  mutate(bmi_cat = ifelse(is.na(bmi_cat) & obesity == 1, 3,ifelse(is.na(bmi_cat) & obesity == 0,NA,bmi_cat))) %>%
  select(-c(code_desc,yob_approx,diagnosisdatebest2,eventdate))

#imputing the remaining bmi values
data_full_join_prep <- data_full_join_4 %>% ungroup() %>%
  select(-c(disease_name,height,bmi,eventdate.y)) %>%
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
query <- 'SELECT * FROM disease_index_hernia_abdo_all_flags_sm_fin_elix'
rs <- dbSendQuery(db, query)
hernia_abdo <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_gord_all_flags_sm_fin_elix'
rs <- dbSendQuery(db, query)
gord <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_gastritis_duodenitis_all_flags_sm_fin_elix'
rs <- dbSendQuery(db, query)
gastritis_duodenitis <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_barretts_all_flags_sm_fin_elix'
rs <- dbSendQuery(db, query)
barretts <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_oesoph_ulc_all_flags_sm_fin_elix'
rs <- dbSendQuery(db, query)
oesoph_ulc <- fetch(rs, n=-1)

hernia_abdo_complete <- bmi_calc(hernia_abdo,height_fil,weight)
gord_complete <- bmi_calc(gord,height_fil,weight)
gastritis_duodenitis_complete <- bmi_calc(gastritis_duodenitis,height_fil,weight)
barretts_complete <- bmi_calc(barretts,height_fil,weight)
oesoph_ulc_complete <- bmi_calc(oesoph_ulc,height_fil,weight)

dbWriteTable(db, "disease_index_gord_all_flags_sm_fin_elix_alc",gord_complete, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "disease_index_hernia_abdo_all_flags_sm_fin_elix_alc",hernia_abdo_complete, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "disease_index_gastritis_duodenitis_all_flags_sm_fin_elix_alc",gastritis_duodenitis_complete, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "disease_index_barretts_all_flags_sm_fin_elix_alc",barretts_complete, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "disease_index_oesoph_ulc_all_flags_sm_fin_elix_alc",oesoph_ulc_complete, row.names = FALSE, overwrite = TRUE)

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
    mutate(alc_status_comb = ifelse(alc_problems == 1 & alc_status == 3, 5, #drinker with a history of alcohol problems
                                    ifelse(alc_problems == 0 & alc_status == 3, 4, #drinker with no history of alcohol problems
                                           ifelse(alc_problems == 1 & alc_status == 2, 3, #ex drinker with history of alcohol problems
                                                  ifelse(alc_problems == 0 & alc_status == 2, 2, #ex drinker with no history of alcohol problems
                                                         ifelse(alc_problems == 1 & alc_status == 1, 3, #non-drinker with history of alcohol problems (setting them as an ex drinker with history)
                                                                ifelse(alc_problems == 0 & alc_status == 1, 1,NA)))))))#non-drinker with no history of alcohol problems
  
  
  #imputing the remaining alcohol values
  data_complete_join_prep <- data_complete_join %>% ungroup() %>%
    select(-c(alc_status,eventdate)) %>%
    mutate(alc_status_comb = as.factor(alc_status_comb)) %>%
    rename(eventdate = eventdate.x)
  
  set.seed(123)
  data_impute <- mice(data_complete_join_prep,method='cart')
  
  data_impute_complete <- complete(data_impute,1)
  
}
#-------------------------------

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.cohort_barretts_alc;")

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.cohort_barretts_alc -- here
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.disease_index_barretts_all_flags_sm_fin_elix ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;")

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.cohort_gord_alc;")

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.cohort_gord_alc -- here
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.disease_index_gord_all_flags_sm_fin_elix ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;")

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.cohort_gastritis_duodenitis_alc;")

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.cohort_gastritis_duodenitis_alc -- here
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.disease_index_gastritis_duodenitis_all_flags_sm_fin_elix ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;")

dbSendQuery(
  conn = db,
  statement = "DROP TABLE IF EXISTS freya.cohort_oesoph_ulc_alc;")

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.cohort_oesoph_ulc_alc -- here
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.disease_index_oesoph_ulc_all_flags_sm_fin_elix ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;")

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.cohort_hernia_abdo_alc;")

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.cohort_hernia_abdo_alc 
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.disease_index_hernia_abdo_all_flags_sm_fin_elix ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;")

#------------------------------------------------------------------------------------
query <- 'SELECT * FROM cohort_hernia_abdo_alc'
rs <- dbSendQuery(db, query)
hernia_abdo_alc <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_gord_alc'
rs <- dbSendQuery(db, query)
gord_alc <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_barretts_alc'
rs <- dbSendQuery(db, query)
barretts_alc <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_oesoph_ulc_alc'
rs <- dbSendQuery(db, query)
oesoph_ulc_alc <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_gastritis_duodenitis_alc'
rs <- dbSendQuery(db, query)
gastritis_duodenitis_alc <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_gord_all_flags_sm_fin_elix_alc'
rs <- dbSendQuery(db, query)
gord_complete <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_gastritis_duodenitis_all_flags_sm_fin_elix_alc'
rs <- dbSendQuery(db, query)
gastritis_duodenitis_complete <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_barretts_all_flags_sm_fin_elix_alc'
rs <- dbSendQuery(db, query)
barretts_complete <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_hernia_abdo_all_flags_sm_fin_elix_alc'
rs <- dbSendQuery(db, query)
hernia_abdo_complete <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_oesoph_ulc_all_flags_sm_fin_elix_alc'
rs <- dbSendQuery(db, query)
oesoph_ulc_complete <- fetch(rs, n=-1)

gord_fin <- alc_fun(gord_alc,gord_complete)
barretts_fin <- alc_fun(barretts_alc,barretts_complete)
oesoph_ulc_fin <- alc_fun(oesoph_ulc_alc,oesoph_ulc_complete)
hernia_abdo_fin <- alc_fun(hernia_abdo_alc,hernia_abdo_complete)
gastritis_duodenitis_fin <- alc_fun(gastritis_duodenitis_alc,gastritis_duodenitis_complete)

dbWriteTable(db, "cohort_gord_final",gord_fin, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_hernia_abdo_final",hernia_abdo_fin, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_gastritis_duodenitis_final",gastritis_duodenitis_fin, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_barretts_final",barretts_fin, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_oesoph_ulc_final",oesoph_ulc_fin, row.names = FALSE, overwrite = TRUE)

#go to sql script 4.0