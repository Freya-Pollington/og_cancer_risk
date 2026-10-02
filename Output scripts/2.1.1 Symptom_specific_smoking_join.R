rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(tidyverse, tidyr,lubridate, 
               data.table, RMySQL, DBI)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#--------------------------------------------------------------------
## 2. Joining in smoking status
#----------------------------------------------------------------------

#In SQL script 2.1, smoking status data is extracted for each patient, 
#now needs to be joined into each symptom cohort

#-----------------
#### DYSPEPSIA ####
#----------------

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.cohort_dyspepsia_cr_flag_nn_flag_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_dyspepsia_cr_flag_nn_flag_sm
SELECT v2.*, cl.smokingcat, cl.smok_date
FROM freya.cohort_dyspepsia_cr_flag_nn_flag v2
LEFT JOIN freya.smoking_pot_elig_dist cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year) 
AND cl.smok_date <= v2.eventdate;')

# dbSendQuery(
#   conn = db,
#   statement = '
# DROP TABLE IF EXISTS freya.cohort_dyspepsia_cr_flag_nn_flag_sm_dist;')
# 
# dbSendQuery(
#   conn = db,
#   statement = '
# CREATE TABLE freya.cohort_dyspepsia_cr_flag_nn_flag_sm_dist
# SELECT DISTINCT *
#   FROM freya.cohort_dyspepsia_cr_flag_nn_flag_sm
# ;')

#-----------------
#### dysphagia ####
#----------------

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.cohort_dysphagia_cr_flag_nn_flag_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_dysphagia_cr_flag_nn_flag_sm
SELECT v2.*, cl.smokingcat, cl.smok_date
FROM freya.cohort_dysphagia_cr_flag_nn_flag v2
LEFT JOIN freya.smoking_pot_elig_dist cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year) 
AND cl.smok_date <= v2.eventdate;')

# dbSendQuery(
#   conn = db,
#   statement = '
# DROP TABLE IF EXISTS freya.cohort_dysphagia_cr_flag_nn_flag_sm_dist;')
# 
# dbSendQuery(
#   conn = db,
#   statement = '
# CREATE TABLE freya.cohort_dysphagia_cr_flag_nn_flag_sm_dist
# SELECT DISTINCT *
#   FROM freya.cohort_dysphagia_cr_flag_nn_flag_sm
# ;')


#-----------------
#### vomiting ####
#----------------

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.cohort_vomiting_cr_flag_nn_flag_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_vomiting_cr_flag_nn_flag_sm
SELECT v2.*, cl.smokingcat, cl.smok_date
FROM freya.cohort_vomiting_cr_flag_nn_flag v2
LEFT JOIN freya.smoking_pot_elig_dist cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year) 
AND cl.smok_date <= v2.eventdate;')

# dbSendQuery(
#   conn = db,
#   statement = '
# DROP TABLE IF EXISTS freya.cohort_vomiting_cr_flag_nn_flag_sm_dist;')
# 
# dbSendQuery(
#   conn = db,
#   statement = '
# CREATE TABLE freya.cohort_vomiting_cr_flag_nn_flag_sm_dist
# SELECT DISTINCT *
#   FROM freya.cohort_vomiting_cr_flag_nn_flag_sm
# ;')


#-----------------
#### abd_pain ####
#----------------

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.cohort_abd_pain_cr_flag_nn_flag_sm;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_abd_pain_cr_flag_nn_flag_sm
SELECT v2.*, cl.smokingcat, cl.smok_date
FROM freya.cohort_abd_pain_cr_flag_nn_flag v2
LEFT JOIN freya.smoking_pot_elig_dist cl ON cl.e_patid = v2.e_patid
AND cl.smok_date >= date_sub(v2.eventdate, interval 20 year) 
AND cl.smok_date <= v2.eventdate;')

# dbSendQuery(
#   conn = db,
#   statement = '
# DROP TABLE IF EXISTS freya.cohort_abd_pain_cr_flag_nn_flag_sm_dist;')
# 
# dbSendQuery(
#   conn = db,
#   statement = '
# CREATE TABLE freya.cohort_abd_pain_cr_flag_nn_flag_sm_dist
# SELECT DISTINCT *
#   FROM freya.cohort_abd_pain_cr_flag_nn_flag_sm
# ;')

#--------------------
#Function for changing to being current, ex smoker or never smoker and saving table to sql
smoker_fun <- function(data,tab_name){
  #create a flag for the most recent smoking date, a flag for if the record is the most recent, 
  #change the 'current or ex' to current, create a column of the most recent smoking status
  data <- dyspepsia
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
    select(-c(most_recent_smok_date,max_smok_date_flag,smokingcat,recent_smoking_status,n_dis,smok_date,n_rec,recent_ex_smoker,recent_smoker)) %>%
    mutate(updated_smoking_status = ifelse(is.na(updated_smoking_status), 1, updated_smoking_status)) %>%
    mutate(final_smoking_status = max(updated_smoking_status)) %>%
    select(-updated_smoking_status) %>%
    distinct()
  
  dbWriteTable(db, tab_name,data_jn2, row.names = FALSE, overwrite = TRUE)
}

#--------------
#Loading the data and running the function
query <- 'SELECT * FROM freya.cohort_dyspepsia_cr_flag_nn_flag_sm;'
rs <- dbSendQuery(db, query)
dyspepsia <- fetch(rs, n=-1)

tab_name_dyspepsia <- "cohort_dyspepsia_cr_flag_nn_flag_sm_fin"

smoker_fun(dyspepsia,tab_name_dyspepsia)

query <- 'SELECT * FROM freya.cohort_dysphagia_cr_flag_nn_flag_sm;'
rs <- dbSendQuery(db, query)
dysphagia <- fetch(rs, n=-1)

tab_name_dysphagia <- "cohort_dysphagia_cr_flag_nn_flag_sm_fin"

smoker_fun(dysphagia,tab_name_dysphagia)

query <- 'SELECT * FROM freya.cohort_vomiting_cr_flag_nn_flag_sm;'
rs <- dbSendQuery(db, query)
vomiting <- fetch(rs, n=-1)

tab_name_vomiting <- "cohort_vomiting_cr_flag_nn_flag_sm_fin"

smoker_fun(vomiting,tab_name_vomiting)

query <- 'SELECT * FROM freya.cohort_abd_pain_cr_flag_nn_flag_sm;'
rs <- dbSendQuery(db, query)
abd_pain <- fetch(rs, n=-1)

tab_name_abd_pain <- "cohort_abd_pain_cr_flag_nn_flag_sm_fin"

smoker_fun(abd_pain,tab_name_abd_pain)

#Go to R script 2.2
#1241+331+647+3631
