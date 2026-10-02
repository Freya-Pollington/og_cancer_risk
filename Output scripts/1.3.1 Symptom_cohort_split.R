rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(tidyverse,here,skimr,binom, tidyr,lubridate, 
                RMySQL, DBI)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#--------------------------------------------------
query <- 'SELECT * FROM freya.cohortv1_8;'
rs <- dbSendQuery(db, query)
main <- fetch(rs, n=-1)

#as the vague and alarm symptoms were pulled in as dates at the end of the last script via sql, 
#changing them to 1s and 0s
main_all_3 <- main %>%
  mutate(weight_loss = ifelse(is.na(weight_loss),0,1)) %>%
  mutate(cough = ifelse(is.na(cough),0,1)) %>%
  mutate(fatigue = ifelse(is.na(fatigue),0,1)) %>%
  mutate(appetite_loss = ifelse(is.na(appetite_loss),0,1))%>%
  mutate(ever_obesity = ifelse(is.na(ever_obesity),0,1)) %>%
  mutate(ever_alcohol_problems = ifelse(is.na(ever_alcohol_problems),0,1)) %>%
  mutate(eventdate = as.Date(eventdate))
  

main_all_3 <- distinct(main_all_3)
#-----------------------------------
##Splitting into symptom cohorts

#Dyspepsia#
main_dyspepsia <- main_all_3 %>% filter(Dyspepsia == 1) %>%
  select(-c(Dyspepsia, Dysphagia, Vomiting, Abdominal_pain))

#Dysphagia#
main_dysphagia <- main_all_3 %>% filter(Dysphagia == 1) %>%
  select(-c(Dyspepsia, Dysphagia, Vomiting, Abdominal_pain))

#Vomiting#
main_vomiting <- main_all_3 %>% filter(Vomiting == 1) %>%
  select(-c(Dyspepsia, Dysphagia, Vomiting, Abdominal_pain))

#Abdominal pain#
main_abd_pain <- main_all_3 %>% filter(Abdominal_pain == 1) %>% 
  select(-c(Dyspepsia, Dysphagia, Vomiting, Abdominal_pain))


dbWriteTable(db, "cohort_dyspepsia",main_dyspepsia,row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_dysphagia",main_dysphagia,row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_vomiting",main_vomiting,row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_abd_pain",main_abd_pain,row.names = FALSE, overwrite = TRUE)

## 1. Joining cancer registry data -----------------
dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.cohort_dyspepsia_cr;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE cohort_dyspepsia_cr
SELECT v2.*, crt.diagnosisdatebest, icd.cancer_site_desc
FROM freya.cohort_dyspepsia v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.cohort_abd_pain_cr;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.cohort_abd_pain_cr
SELECT v2.*, crt.diagnosisdatebest, icd.cancer_site_desc
FROM freya.cohort_abd_pain v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.cohort_dysphagia_cr;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.cohort_dysphagia_cr
SELECT v2.*, crt.diagnosisdatebest, icd.cancer_site_desc
FROM freya.cohort_dysphagia v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)


dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.cohort_vomiting_cr;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.cohort_vomiting_cr
SELECT v2.*, crt.diagnosisdatebest, icd.cancer_site_desc
FROM freya.cohort_vomiting v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)

#go to script sql 1.4
