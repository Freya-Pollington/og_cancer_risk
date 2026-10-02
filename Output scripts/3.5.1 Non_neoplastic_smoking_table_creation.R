rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(tidyverse,here,skimr,binom, tidyr,lubridate, 
               RMySQL, DBI, data.table)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#------------------------------------------------------------------------------------------------------
#Extracting smoking information for the non-neoplastic cohorts

dbSendQuery(
  conn = db,
  statement = 'CREATE INDEX e_patid on freya.disease_index_all_pot_elig (e_patid);')

  
dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.smoking_cohort_non_neo_all;')

dbSendQuery(
    conn = db,
    statement = 'CREATE TABLE freya.smoking_cohort_non_neo_all(
  SELECT b.e_patid, a.eventdate AS smok_date, m.smokingcat, m.`Clinical term`
  FROM freya.disease_index_all_pot_elig b
  INNER JOIN 18_299_Lyratzopoulos_e2.cprd_clinical a ON a.e_patid=b.e_patid
  INNER JOIN freya.smoking_codelist m ON a.medcode=m.medcode
);')

      
dbSendQuery(
    conn = db,
    statement = 'DROP TABLE IF EXISTS freya.smoking_cohort_gord_extra;')

    
dbSendQuery(
    conn = db,
    statement = 'CREATE TABLE freya.smoking_cohort_gord_extra(
  SELECT b.e_patid, a.eventdate AS smok_date, m.smokingcat, m.`Clinical term`
  FROM freya.disease_index_gord_all_flags b
  INNER JOIN 18_299_Lyratzopoulos_e2.cprd_clinical a ON a.e_patid=b.e_patid
  INNER JOIN freya.smoking_codelist m ON a.medcode=m.medcode
);')

dbSendQuery(
    conn = db,
    statement ='DROP TABLE IF EXISTS freya.smoking_cohort_non_neo_all_dist;')

dbSendQuery(
    conn = db,
    statement ='CREATE TABLE freya.smoking_cohort_non_neo_all_dist
  SELECT DISTINCT *
  FROM freya.smoking_cohort_non_neo_all
;')

#Go to R script 3.5.2