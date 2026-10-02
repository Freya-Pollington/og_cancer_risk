rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#----------------------------------------------------------------
#creating a table of smoking event codes in cprd and hes in SQL 3.5
##### Joining the non-neo info into each disease cohort ####

#sense checking to ensure that no additional reflux patients are missed from the overall disease
#smoking table
query <- 'SELECT * FROM disease_index_all_pot_elig;'
rs <- dbSendQuery(db, query)
all <- fetch(rs, n=-1)

query <- 'SELECT * FROM freya.disease_index_gord_all_flags;'
rs <- dbSendQuery(db, query)
gord <- fetch(rs, n=-1)

extra <- anti_join(gord,all,by='e_patid')

#GORD
dbSendQuery(
  conn = db,
  statement = '
CREATE INDEX e_patid on freya.disease_index_gord_all_flags (e_patid);
')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.disease_index_gord_all_flags_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.disease_index_gord_all_flags_sm
SELECT v2.*, cl.smokingcat,  cl.`Clinical term`, cl.smok_date 
FROM freya.disease_index_gord_all_flags v2
LEFT JOIN freya.smoking_cohort_gord_extra2 cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year)
AND cl.smok_date <= v2.eventdate;')


#Gastritis_duodenitis
dbSendQuery(
  conn = db,
  statement = '
CREATE INDEX e_patid on freya.disease_index_gastritis_duodenitis_all_flags (e_patid);')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.disease_index_gastritis_duodenitis_all_flags_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.disease_index_gastritis_duodenitis_all_flags_sm
SELECT v2.*, cl.smokingcat,  cl.`Clinical term`, cl.smok_date 
FROM freya.disease_index_gastritis_duodenitis_all_flags v2
LEFT JOIN freya.smoking_cohort_non_neo_all cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year)
AND cl.smok_date <= v2.eventdate;')

#Abdominal hernia
dbSendQuery(
  conn = db,
  statement = '
CREATE INDEX e_patid on freya.disease_index_hernia_abdo_all_flags (e_patid);')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.disease_index_hernia_abdo_all_flags_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.disease_index_hernia_abdo_all_flags_sm
SELECT v2.*, cl.smokingcat,  cl.`Clinical term`, cl.smok_date 
FROM freya.disease_index_hernia_abdo_all_flags v2
LEFT JOIN freya.smoking_cohort_non_neo_all cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year)
AND cl.smok_date <= v2.eventdate;')


#Barretts
#dbSendQuery(
#  conn = db,
#  statement = '
#CREATE INDEX e_patid on freya.disease_index_barretts_all_flags (e_patid);')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.disease_index_barretts_all_flags_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.disease_index_barretts_all_flags_sm
SELECT v2.*, cl.smokingcat,  cl.`Clinical term`, cl.smok_date 
FROM freya.disease_index_barretts_all_flags v2
LEFT JOIN freya.smoking_cohort_non_neo_all cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year)
AND cl.smok_date <= v2.eventdate;')

#Gastritis
#dbSendQuery(
#  conn = db,
#  statement = 'CREATE INDEX e_patid on freya.disease_index_gastritis_duodenitis_all_flags (e_patid);')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.disease_index_gastritis_duodenitis_all_flags_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.disease_index_gastritis_duodenitis_all_flags_sm
SELECT v2.*, cl.smokingcat,  cl.`Clinical term`, cl.smok_date 
FROM freya.disease_index_gastritis_duodenitis_all_flags v2
LEFT JOIN freya.smoking_cohort_non_neo_all cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year)
AND cl.smok_date <= v2.eventdate;')

#Oesophageal ulcer
dbSendQuery(
  conn = db,
  statement = 'CREATE INDEX e_patid on freya.disease_index_oesoph_ulc_all_flags (e_patid);')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.disease_index_oesoph_ulc_all_flags_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.disease_index_oesoph_ulc_all_flags_sm
SELECT v2.*, cl.smokingcat,  cl.`Clinical term`, cl.smok_date 
FROM freya.disease_index_oesoph_ulc_all_flags v2
LEFT JOIN freya.smoking_cohort_non_neo_all cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year)
AND cl.smok_date <= v2.eventdate;')

#--------------------
#Function for changing to being current, ex smoker or never smoker and saving table to sql
smoker_fun <- function(data,tab_name){
  #create a flag for the most recent smoking date, a flag for if the record is the most recent, 
  #change the 'current or ex' to current, create a column of the most recent smoking status
  data <- data %>%
    mutate(smok_date = as.Date(smok_date)) %>%
    group_by(e_patid) %>%
    mutate(smokingcat = ifelse(is.na(smokingcat),1,smokingcat)) %>%
    mutate(most_recent_smok_date = max(smok_date))%>%
    mutate(max_smok_date_flag = ifelse(most_recent_smok_date == smok_date,1,0)) %>%
    mutate(smokingcat = ifelse(smokingcat == 3, 4, ifelse(is.na(smokingcat),1,smokingcat))) %>%
    mutate(recent_smoking_status = ifelse(max_smok_date_flag == 1, smokingcat, NA))
  
  #find the number of different smoking statuses for each patient
  data_2 <- data %>%
    group_by(e_patid) %>%
    summarise(n_distinct(smokingcat))
  names(data_2)[-1] <- "n_dis"
  
  data_jn <- left_join(data,data_2)
  
  #create an updated smoking status field based on the information available for those with 'never' status
  data_jn2 <- data_jn %>%
    group_by(e_patid) %>%
    arrange(e_patid,desc(smok_date)) %>%
    #create a column for the order placement of a smoking status if a patient has more than one smoking status
    mutate(n_rec = row_number()) %>%
    #fill in the most recent value for patients with more than one smoking status over time
    fill(recent_smoking_status, .direction="downup") %>%
    #find those who have a recent never status, but an ex status within the previous 5 records
    mutate(recent_ex_smoker = ifelse(n_dis>1 & n_rec >= 2 & n_rec <= 5 & recent_smoking_status == 1 & smokingcat == 2,1,0)) %>%
    #find those who have a recent never status, but current status within the previous 5 records
    mutate(recent_smoker = ifelse(n_dis>1 & n_rec >= 2 & n_rec <= 5 & recent_smoking_status == 1 & smokingcat == 4,1,0)) %>%
    #if a patient has most recent never smoker status, but also a recent ex status, code ex, if no ex but a current, code current
    #if only never codes for the previous 5 records, but they have an ex or current code before that, code ex, otherwise leave as is for most recent smoking status
    mutate(updated_smoking_status = ifelse(recent_smoking_status == 1 & recent_ex_smoker == 1,2,
                                           ifelse(recent_smoking_status == 1 & recent_smoker == 1,4,
                                                  ifelse(recent_smoking_status == 1 & recent_ex_smoker == 0 & recent_ex_smoker == 0 & max(smokingcat) > 1,2,recent_smoking_status)))) %>%
    #filtering steps to ensure one row per patient
    filter(most_recent_smok_date == smok_date | is.na(most_recent_smok_date)) %>%
    select(-c(most_recent_smok_date,max_smok_date_flag,smokingcat,recent_smoking_status,n_dis,smok_date,`Clinical term`,n_rec,recent_ex_smoker,recent_smoker)) %>%
    mutate(updated_smoking_status = ifelse(is.na(updated_smoking_status), 1, updated_smoking_status)) %>%
    mutate(final_smoking_status = max(updated_smoking_status)) %>%
    select(-updated_smoking_status) %>%
    distinct()

  dbWriteTable(db, tab_name,data_jn2, row.names = FALSE, overwrite = TRUE)
}

#--------------
#Loading the data and running the function
query <- 'SELECT * FROM disease_index_gord_all_flags_sm;'
rs <- dbSendQuery(db, query)
gord <- fetch(rs, n=-1)

tab_name_gord <- "disease_index_gord_all_flags_sm_fin"

smoker_fun(gord,tab_name_gord)

query <- 'SELECT * FROM disease_index_oesoph_ulc_all_flags_sm;'
rs <- dbSendQuery(db, query)
oesoph_ulc <- fetch(rs, n=-1)

tab_name_oesoph_ulc <- "disease_index_oesoph_ulc_all_flags_sm_fin"

smoker_fun(oesoph_ulc,tab_name_oesoph_ulc)
 
query <- 'SELECT * FROM disease_index_barretts_all_flags_sm'
rs <- dbSendQuery(db, query)
barretts <- fetch(rs, n=-1)

tab_name_barretts <- "disease_index_barretts_all_flags_sm_fin"

smoker_fun(barretts,tab_name_barretts)

query <- 'SELECT * FROM disease_index_hernia_abdo_all_flags_sm;'
rs <- dbSendQuery(db, query)
hernia_abdo <- fetch(rs, n=-1)

tab_name_hernia_abdo <- "disease_index_hernia_abdo_all_flags_sm_fin"

smoker_fun(hernia_abdo,tab_name_hernia_abdo)

query <- 'SELECT * FROM disease_index_gastritis_duodenitis_all_flags_sm;'
rs <- dbSendQuery(db, query)
gastritis_duodenitis <- fetch(rs, n=-1)

tab_name_gastritis_duodenitis <- "disease_index_gastritis_duodenitis_all_flags_sm_fin"

smoker_fun(gastritis_duodenitis,tab_name_gastritis_duodenitis)


#go to sql script 3.6 for elixhauser extraction
