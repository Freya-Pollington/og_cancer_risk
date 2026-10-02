rm(list=ls())

#### Loading Packages ####
library(pacman)

pacman::p_load(tidyverse,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI, comorbidity)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#----------------------------
## Need to calculate and join in Elixhauser scores
# In SQL for the CPRD and HES tables (before filtering for oes ulc), have created the ICD vectors based on
# event date for the potentially eligible events

#Oesophageal ulcer
dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.elixhauser_codes_oesoph_ulc;')

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.elixhauser_codes_oesoph_ulc
SELECT a.e_patid, a.disease_name,a.eventdate,b.icd
FROM freya.disease_index_oesoph_ulc_elig_flag_cr a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_hosp b on b.e_patid = a.e_patid"
)

#Barretts
dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.elixhauser_codes_barretts;')

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.elixhauser_codes_barretts
SELECT a.e_patid, a.disease_name,a.eventdate,b.icd
FROM freya.disease_index_barretts_elig_flag_cr a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_hosp b on b.e_patid = a.e_patid"
)

#GORD
dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.elixhauser_codes_gord;')

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.elixhauser_codes_gord
SELECT a.e_patid, a.disease_name,a.eventdate,b.icd
FROM freya.disease_index_gord_elig_flag_cr a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_hosp b on b.e_patid = a.e_patid"
)

#Abdo hernia
dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.elixhauser_codes_hernia_abdo;')

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.elixhauser_codes_hernia_abdo
SELECT a.e_patid, a.disease_name,a.eventdate,b.icd
FROM freya.disease_index_hernia_abdo_elig_flag_cr a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_hosp b on b.e_patid = a.e_patid"
)

#Gastritis
dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.elixhauser_codes_gastritis_duodenitis;')

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.elixhauser_codes_gastritis_duodenitis
SELECT a.e_patid, a.disease_name,a.eventdate,b.icd
FROM freya.disease_index_gastritis_duodenitis_elig_flag_cr a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_hosp b on b.e_patid = a.e_patid"
)


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

dbWriteTable(db, tab_name,data_all, row.names = FALSE, overwrite = TRUE)
}

#------------------------------------------------------------------------
##Loading the data for each cohort and applying the function

query <- 'SELECT * FROM freya.elixhauser_codes_oesoph_ulc;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_oesoph_ulc_all_flags_sm_fin'
rs <- dbSendQuery(db, query)
data_full <- fetch(rs, n=-1)

tab_name <- "disease_index_oesoph_ulc_all_flags_sm_fin_elix"

elix_add(elixhauser,data_full,tab_name)

#----
query <- 'SELECT * FROM freya.elixhauser_codes_hernia_abdo;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_hernia_abdo_all_flags_sm_fin'
rs <- dbSendQuery(db, query)
data_full <- fetch(rs, n=-1)

tab_name <- "disease_index_hernia_abdo_all_flags_sm_fin_elix"

elix_add(elixhauser,data_full,tab_name)

#-----
query <- 'SELECT * FROM freya.elixhauser_codes_gord;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_gord_all_flags_sm_fin'
rs <- dbSendQuery(db, query)
data_full <- fetch(rs, n=-1)

tab_name <- "disease_index_gord_all_flags_sm_fin_elix"

elix_add(elixhauser,data_full,tab_name)

#-----
query <- 'SELECT * FROM freya.elixhauser_codes_barretts;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_barretts_all_flags_sm_fin'
rs <- dbSendQuery(db, query)
data_full <- fetch(rs, n=-1)

tab_name <- "disease_index_barretts_all_flags_sm_fin_elix"

elix_add(elixhauser,data_full,tab_name)

#-----
query <- 'SELECT * FROM freya.elixhauser_codes_gastritis_duodenitis;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_gastritis_duodenitis_all_flags_sm_fin'
rs <- dbSendQuery(db, query)
data_full <- fetch(rs, n=-1)

tab_name <- "disease_index_gastritis_duodenitis_all_flags_sm_fin_elix"

elix_add(elixhauser,data_full,tab_name)

#go to r script 3.7