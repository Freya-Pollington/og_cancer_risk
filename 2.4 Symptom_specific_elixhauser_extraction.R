rm(list=ls())

#### Loading Packages ####
library(pacman)

pacman::p_load(tidyverse, tidyr,lubridate, 
               data.table, RMySQL, DBI, comorbidity)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#-------------------------------------------------------
## Need to calculate Elixhauser score

#Have taken patient history 20 years before index date with every ICD10 code where need a column
#of patient ID and column of codes and is joined for each patients event date

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.elixhauser_codes_dyspepsia;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.elixhauser_codes_dyspepsia
SELECT a.e_patid, b.icd
FROM freya.cohort_dyspepsia_cr_flag_nn_flag_sm_fin a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_epi b on b.e_patid = a.e_patid
WHERE b.epistart >= date_sub(a.eventdate,interval 20 year)
AND b.epistart <= a.eventdate
;')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.elixhauser_codes_dysphagia;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.elixhauser_codes_dysphagia
SELECT a.e_patid, b.icd
FROM freya.cohort_dysphagia_cr_flag_nn_flag_sm_fin a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_epi b on b.e_patid = a.e_patid
WHERE b.epistart >= date_sub(a.eventdate,interval 20 year)
AND b.epistart <= a.eventdate
;')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.elixhauser_codes_vomiting;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.elixhauser_codes_vomiting
SELECT a.e_patid, b.icd
FROM freya.cohort_vomiting_cr_flag_nn_flag_sm_fin a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_epi b on b.e_patid = a.e_patid
WHERE b.epistart >= date_sub(a.eventdate,interval 20 year)
AND b.epistart <= a.eventdate
;')


dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.elixhauser_codes_abd_pain;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.elixhauser_codes_abd_pain
SELECT a.e_patid, b.icd
FROM freya.cohort_abd_pain_cr_flag_nn_flag_sm_fin a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_epi b on b.e_patid = a.e_patid
WHERE b.epistart >= date_sub(a.eventdate,interval 20 year)
AND b.epistart <= a.eventdate
;')


#------------------------------------------------------------------------------------------
## creating a function to add in elix info

elix_add <- function(elixhauser, data_full, tab_name){
  com <- comorbidity(x=elixhauser, id="e_patid", code= "icd",map = "elixhauser_icd10_quan",assign0 = FALSE)
  
  com <- com %>% select(- c(metacanc,solidtum))
  
  unw_score <- rowSums(com[,2:30])
  
  elixhauser_score <- cbind(com$e_patid, unw_score)
  
  elixhauser_score <- as.data.frame(elixhauser_score)
  
  names(elixhauser_score)[1] <- "e_patid"
  
  # Joining the elixhauser scores back into the main dataframe with smoking
  data_all <- left_join(data_full, elixhauser_score)
  
  data_all$unw_score <- ifelse(is.na(data_all$unw_score),0,data_all$unw_score)

  dbWriteTable(db, tab_name,data_all, row.names = FALSE, overwrite = TRUE)
}

#------------------------------------------------------------------------
##Loading the data for each cohort and applying the function

query <- 'SELECT * FROM freya.elixhauser_codes_dyspepsia;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

query <- 'SELECT * FROM freya.cohort_dyspepsia_cr_flag_nn_flag_sm_fin'
rs <- dbSendQuery(db, query)
data_full <- fetch(rs, n=-1)

tab_name <- "cohort_dyspepsia_cr_flag_nn_flag_sm_fin_elix"

elix_add(elixhauser,data_full,tab_name)

#----

query <- 'SELECT * FROM freya.elixhauser_codes_dysphagia;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

query <- 'SELECT * FROM freya.cohort_dysphagia_cr_flag_nn_flag_sm_fin'
rs <- dbSendQuery(db, query)
data_full <- fetch(rs, n=-1)

tab_name <- "cohort_dysphagia_cr_flag_nn_flag_sm_fin_elix"

elix_add(elixhauser,data_full,tab_name)

#----

query <- 'SELECT * FROM freya.elixhauser_codes_vomiting;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

query <- 'SELECT * FROM freya.cohort_vomiting_cr_flag_nn_flag_sm_fin'
rs <- dbSendQuery(db, query)
data_full <- fetch(rs, n=-1)

tab_name <- "cohort_vomiting_cr_flag_nn_flag_sm_fin_elix"

elix_add(elixhauser,data_full,tab_name)

#----

query <- 'SELECT * FROM freya.elixhauser_codes_abd_pain;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

query <- 'SELECT * FROM freya.cohort_abd_pain_cr_flag_nn_flag_sm_fin'
rs <- dbSendQuery(db, query)
data_full <- fetch(rs, n=-1)

tab_name <- "cohort_abd_pain_cr_flag_nn_flag_sm_fin_elix"

elix_add(elixhauser,data_full,tab_name)

#go to sql script 3.0

