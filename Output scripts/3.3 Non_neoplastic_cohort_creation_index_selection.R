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

#-------------------------------------------------------------
#### Filtering to each cohort ####

elig_fil <- function(data,var_name,tab_name){
  data_all <- data %>% 
    filter(disease_name == {var_name}) %>%
    mutate(eventdate = as.Date(eventdate))
  
  #Creating an age band variable for age at index symptom for every cohort other tha GORD
  if (var_name != "gord"){
    
    data_all <- data_all %>%
      mutate(age = round(time_length(difftime(eventdate,yob_approx),"years"),0)) %>%
      ungroup() %>%
      mutate(age_band = cut(age,breaks = c(30,40,50,60,70,80,90,100),include.lowest = TRUE, right = FALSE)) %>%
      distinct() %>%
      mutate(age_band = gsub("([,])|[[:punct:]]","\\1",age_band))
  }
    
  #Sort chronologically by patient
  data_sort <- data_all[
    with(data_all, order(e_patid, eventdate)),
  ]
  
  data_sort <- data_sort %>%
    group_by(e_patid) %>%
    mutate(eventdate_lag1 = lag(eventdate)) %>%
    fill(eventdate_lag1) %>%
    ungroup()
  
  # Date 2 year before upper GI symptom
  data_sort <- data_sort %>%
    mutate(eventdate = as.Date(eventdate)) %>%
    mutate(eventdate_pre_2yr = data_sort$eventdate %m-% years(2))
  
  # Flagging eligible events if potentially eligible, no previous disease cases in the last 2 years
  data_sort <- data_sort %>%
    mutate(data_sort, elig = ifelse (
        (eventdate_lag1 < eventdate_pre_2yr | is.na(eventdate_lag1))
      ,
      1, 0
    ))
  
  dbWriteTable(db, {tab_name},data_sort,row.names = FALSE, overwrite = TRUE)
}

#------------------------------
#loading data and passing through the function

#gord
query <- 'SELECT * FROM disease_index_all_pot_elig_tests_hpylori;'
rs <- dbSendQuery(db, query)
data_gord <- fetch(rs, n=-1)
data_gord <- data_gord %>%
  mutate(eventdate = as.Date(eventdate))

var_gord <- "gord"
tab_gord <- "disease_index_gord_elig_flag"

#Creating an age band variable for age at index symptom
data_gord <- data_gord %>%
  mutate(age = round(time_length(difftime(eventdate,yob_approx),"years"),0)) %>%
  ungroup() %>%
  mutate(age_band = cut(age,breaks = c(30,40,50,60,70,80,90,100),include.lowest = TRUE, right = FALSE)) %>%
  distinct() %>%
  mutate(age_band = gsub("([,])|[[:punct:]]","\\1",age_band))

#add in reflux patients
query <- 'SELECT * FROM freya.disease_index_reflux;'
rs <- dbSendQuery(db, query)
reflux <- fetch(rs, n=-1)
reflux <- reflux %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(diagnosisdatebest2 = as.Date(diagnosisdatebest2)) 

#find patients in the reflux cohort who are not in the gord cohort
anti_join <- anti_join(reflux,data_gord%>% filter(disease_name=="gord"),by='e_patid')

#processing the dataframe to enable union with the main gord dataframe, including creating the og_cancer variable, 
#and choosing a random index for those with more than one eligible index date
anti_join <- anti_join %>%
  mutate(og_cancer = ifelse(s_or_o_cancer == 1 & diagnosisdatebest2 >= eventdate & diagnosisdatebest2 <= eventdate %m+% months(12),1,0)) %>%
  select(-c(cancer_group_desc_v2,cancer_site_desc_v2,medcode,symptom_desc)) %>%
  rename(code_desc = readcode_desc) %>%
  mutate(disease_name = "gord") %>%
  mutate(yob_approx = NA) %>%
  group_by(e_patid) %>%
  sample_n(1) %>%
  mutate(data_source = "cprd")

#join the new reflux patients into main df - removing the precreated cancer variables from reflux data for now to ensure continuity
data_elig <- union(anti_join %>% select(-stomach_c,-s_or_o_cancer,-oesophageal_c,-og_cancer,-diagnosisdatebest2), 
                   data_gord %>% select(-lcd, -uts, -deathdate,-yob, -mob, -crd, -tod, -code_number,-age30date,-age100date,-fu_start,-fu_end,-pot_elig))
  

elig_fil(data_elig,var_gord,tab_gord)

#barretts
query <- 'SELECT * FROM disease_index_all_pot_elig_tests_hpylori;'
rs <- dbSendQuery(db, query)
data_barretts <- fetch(rs, n=-1)
var_barretts <- "barretts"
tab_barretts <- "disease_index_barretts_elig_flag"

elig_fil(data_barretts,var_barretts,tab_barretts)

#oesoph_ulc
query <- 'SELECT * FROM disease_index_all_pot_elig_tests_hpylori;'
rs <- dbSendQuery(db, query)
data_oesoph_ulc <- fetch(rs, n=-1)
var_oesoph_ulc <- "oesoph_ulc"
tab_oesoph_ulc <- "disease_index_oesoph_ulc_elig_flag"

elig_fil(data_oesoph_ulc,var_oesoph_ulc,tab_oesoph_ulc)

#hernia_abdo
query <- 'SELECT * FROM disease_index_all_pot_elig_tests_hpylori;'
rs <- dbSendQuery(db, query)
data_hernia_abdo <- fetch(rs, n=-1)
var_hernia_abdo <- "hernia_abdo"
tab_hernia_abdo <- "disease_index_hernia_abdo_elig_flag"

elig_fil(data_hernia_abdo,var_hernia_abdo,tab_hernia_abdo)

#gastritis_duodenitis
query <- 'SELECT * FROM disease_index_all_pot_elig_tests_hpylori;'
rs <- dbSendQuery(db, query)
data_gastritis_duodenitis <- fetch(rs, n=-1)
var_gastritis_duodenitis <- "gastritis_duodenitis"
tab_gastritis_duodenitis <- "disease_index_gastritis_duodenitis_elig_flag"

elig_fil(data_gastritis_duodenitis,var_gastritis_duodenitis,tab_gastritis_duodenitis)

#------------------------------------------------------------------------------------------------------------------
#### Joining in cancer information to each cohort ####

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.disease_index_gord_elig_flag_cr;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.disease_index_gord_elig_flag_cr
SELECT v2.*, crt.diagnosisdatebest, icd.cancer_site_desc, icd.cancer_group_desc
FROM freya.disease_index_gord_elig_flag v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.disease_index_gastritis_duodenitis_elig_flag_cr;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.disease_index_gastritis_duodenitis_elig_flag_cr
SELECT v2.*, crt.diagnosisdatebest, icd.cancer_site_desc, icd.cancer_group_desc
FROM freya.disease_index_gastritis_duodenitis_elig_flag v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.disease_index_hernia_abdo_elig_flag_cr;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.disease_index_hernia_abdo_elig_flag_cr
SELECT v2.*, crt.diagnosisdatebest, icd.cancer_site_desc, icd.cancer_group_desc
FROM freya.disease_index_hernia_abdo_elig_flag v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)

dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.disease_index_barretts_elig_flag_cr;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.disease_index_barretts_elig_flag_cr
SELECT v2.*, crt.diagnosisdatebest, icd.cancer_site_desc, icd.cancer_group_desc
FROM freya.disease_index_barretts_elig_flag v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)


dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.disease_index_oesoph_ulc_elig_flag_cr;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.disease_index_oesoph_ulc_elig_flag_cr
SELECT v2.*, crt.diagnosisdatebest, icd.cancer_site_desc, icd.cancer_group_desc
FROM freya.disease_index_oesoph_ulc_elig_flag v2
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v2.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)

#------------------------------------------------------------------------------------------------------------
#a function to sort out the cancer codes, flag those with cancer in the last five years, filter to eligible indexes and take the earliest

can_flag_fil <- function(data,tab_name){
    data <- oesoph_ulc
    data_cr <- {data} %>%
      mutate(cancer_site_desc_v2 = ifelse(cancer_site_desc == "Non cancerous ICD10",NA,cancer_site_desc))%>%
      select(-"cancer_site_desc") %>%
      mutate(eventdate = as.Date(eventdate)) %>%
        mutate(cancer_group_desc_v2 = ifelse(cancer_group_desc == "Non cancerous ICD10",NA,cancer_group_desc))

    data_cr$diagnosisdatebest2 <- ifelse(is.na(data_cr$cancer_site_desc_v2)==TRUE,NA,data_cr$diagnosisdatebest)
    data_cr<- data_cr %>% mutate(diagnosisdatebest2 = as.Date(diagnosisdatebest2)) %>%
      select(-"diagnosisdatebest")
    
    # flagging cancer in 5 years
    data_cr <- data_cr %>%
      mutate(prev_can_flag = ifelse(
        cancer_group_desc_v2 == "Upper GI" &
        (eventdate > diagnosisdatebest2) &
          (eventdate < diagnosisdatebest2 %m+% years(5)) &
          (is.na(cancer_site_desc_v2) == FALSE),1,0
      ))

## ------------------------------------------------
#### Filtering to only eligible indexes and taking earliest index for those without cancer in the last 5 years ####

    data_cr_fil <- data_cr %>%
      filter(elig == 1) %>%
      filter(prev_can_flag != 1)
    
    ## Creating flag for oesophageal or stomach cancer, a flag for either
    ## removing the column with text description and taking distinct rows before selecting index dates
    
    data_cr_fil <- data_cr_fil %>%
      mutate(stomach_c =ifelse(cancer_site_desc_v2 == "Stomach" & !is.na(cancer_site_desc_v2),1,0)) %>%
      mutate(oesophageal_c = ifelse(cancer_site_desc_v2 == "Oesophagus"& !is.na(cancer_site_desc_v2),1,0)) %>%
      mutate(s_or_o_cancer = ifelse(stomach_c == 1 | oesophageal_c == 1,1,0)) %>%
      select(e_patid,eventdate,disease_name,code_desc,yob_approx,data_source,gender,stomach_c,oesophageal_c,s_or_o_cancer,diagnosisdatebest2,low_hb,raise_platelets,raise_wbc,h_pylori_flag)
    
    data_cr_fil <- distinct(data_cr_fil)
    
    data_cr_fil <- data_cr_fil %>%
      mutate(diagnosisdatebest2 = ifelse(s_or_o_cancer == 1, as.Date(diagnosisdatebest2), NA)) %>%
      mutate(diagnosisdatebest2 = as.Date(diagnosisdatebest2))
    
    data_cr_fil <- distinct(data_cr_fil)
    
    set.seed(2431)
    data_sort_fil <- data_cr_fil %>%
      group_by(e_patid) %>%
      sample_n(1)
    
    #-----------------------------------------------------------------
    #Creating a flag for valid diagnosis of Oesophago-gastric cancer within 1 year of index
    data_sort_fil <- data_sort_fil %>%
      mutate(og_cancer = ifelse(
        (is.na(diagnosisdatebest2) == FALSE) &
          (s_or_o_cancer == 1) &
          (diagnosisdatebest2 >= eventdate) &
          (diagnosisdatebest2 <= eventdate %m+% years(1)),
        1,0
      )) 
    
    ## Saving the table of valid oesophageal ulcer indexes to SQL to join in other disease and symptom information ##
   dbWriteTable(db, {tab_name},data_sort_fil,row.names = FALSE, overwrite = TRUE)

}

#--------------------------------
#loading the data and applying it to the function

#gord
query <- 'SELECT * FROM freya.disease_index_gord_elig_flag_cr;'
rs <- dbSendQuery(db, query)
gord <- fetch(rs, n=-1)
gord_tab <- "disease_index_gord_elig"
can_flag_fil(gord,gord_tab)


#barretts
query <- 'SELECT * FROM freya.disease_index_barretts_elig_flag_cr;'
rs <- dbSendQuery(db, query)
barretts <- fetch(rs, n=-1)
barretts_tab <- "disease_index_barretts_elig"
can_flag_fil(barretts,barretts_tab)

#hernia_abdo
query <- 'SELECT * FROM freya.disease_index_hernia_abdo_elig_flag_cr;'
rs <- dbSendQuery(db, query)
hernia_abdo <- fetch(rs, n=-1)
hernia_abdo_tab <- "disease_index_hernia_abdo_elig"
can_flag_fil(hernia_abdo,hernia_abdo_tab)

#gastritis_duodenitis
query <- 'SELECT * FROM freya.disease_index_gastritis_duodenitis_elig_flag_cr;'
rs <- dbSendQuery(db, query)
gastritis_duodenitis <- fetch(rs, n=-1)
gastritis_duodenitis_tab <- "disease_index_gastritis_duodenitis_elig"
can_flag_fil(gastritis_duodenitis,gastritis_duodenitis_tab)

#oesoph_ulc
query <- 'SELECT * FROM freya.disease_index_oesoph_ulc_elig_flag_cr;'
rs <- dbSendQuery(db, query)
oesoph_ulc <- fetch(rs, n=-1)
oesoph_ulc_tab <- "disease_index_oesoph_ulc_elig"
can_flag_fil(oesoph_ulc,oesoph_ulc_tab)


# Go to R script 3.4