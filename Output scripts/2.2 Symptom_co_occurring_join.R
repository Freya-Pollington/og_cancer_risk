rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI,janitor)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)


query <- 'SELECT * FROM cohort_dyspepsia_cr_flag;'
rs <- dbSendQuery(db, query)
dyspepsia <- fetch(rs, n=-1)

dyspepsia <- dyspepsia %>% select(-c(cancer_site_desc_v2,diagnosisdatebest2))

query <- 'SELECT * FROM cohort_dysphagia_cr_flag;'
rs <- dbSendQuery(db, query)
dysphagia <- fetch(rs, n=-1)

dysphagia <- dysphagia %>% select(-c(cancer_site_desc_v2,diagnosisdatebest2))

query <- 'SELECT * FROM cohort_vomiting_cr_flag;'
rs <- dbSendQuery(db, query)
vomiting <- fetch(rs, n=-1)

vomiting <- vomiting %>% select(-c(cancer_site_desc_v2,diagnosisdatebest2))

query <- 'SELECT * FROM cohort_abd_pain_cr_flag;'
rs <- dbSendQuery(db, query)
abd_pain <- fetch(rs, n=-1)

abd_pain <- abd_pain %>% select(-c(cancer_site_desc_v2,diagnosisdatebest2))

##----------------------------------------------------------
#### Joining in symptom/lifestyle information ####
#---------------
## To get tables for the main symptoms need to extract from this table of all the main symptoms after filtering for potential eligibility but before index selection 
#from script 1.3.1
query <- 'SELECT * FROM freya.cohortv1_2;'
rs <- dbSendQuery(db, query)
main <- fetch(rs, n=-1)

main <- main %>%
  mutate(eventdate = as.Date(eventdate))

cohort_dyspepsia <- main %>%
  filter(Dyspepsia == 1) %>%
  select(e_patid, eventdate)

cohort_dysphagia <- main %>%
  filter(Dysphagia == 1) %>%
  select(e_patid, eventdate)

cohort_vomiting <- main %>%
  filter(Vomiting == 1) %>%
  select(e_patid, eventdate)

cohort_abd_pain <- main %>%
  filter(Abdominal_pain == 1) %>%
  select(e_patid, eventdate)

names(cohort_dyspepsia)[2] <- "dyspepsia"
names(cohort_dysphagia)[2] <- "dysphagia"
names(cohort_vomiting)[2] <- "vomiting"
names(cohort_abd_pain)[2] <- "abd_pain"

cohort_dyspepsia_anti <- anti_join(cohort_dyspepsia,dyspepsia %>% mutate(eventdate=as.Date(eventdate)),by=c('e_patid','dyspepsia'='eventdate'))
cohort_dysphagia_anti <- anti_join(cohort_dysphagia,dysphagia %>% mutate(eventdate=as.Date(eventdate)),by=c('e_patid','dysphagia'='eventdate'))
cohort_abd_pain_anti <- anti_join(cohort_abd_pain,abd_pain %>% mutate(eventdate=as.Date(eventdate)),by=c('e_patid','abd_pain'='eventdate'))
cohort_vomiting_anti <- anti_join(cohort_vomiting,vomiting %>% mutate(eventdate=as.Date(eventdate)),by=c('e_patid','vomiting'='eventdate'))

#Function to join in each other non-neoplastic disease to each main disease dataframe
disease_join <- function(df_main, df_disease, disease_name) {
  df_main %>%
    mutate(eventdate = as.Date(eventdate)) %>%
    left_join(df_disease, by = "e_patid") %>%
    mutate(!!sym(disease_name) := as.Date(as.character(!!sym(disease_name)),format="%Y-%m-%d"),
      !!paste0("td_",as.character(disease_name)) := ifelse(as.numeric(eventdate - !!sym(as.character(disease_name)))>=0 & 
                                                            as.numeric(eventdate - !!sym(as.character(disease_name))) <= 183,
                                                          as.numeric(eventdate - !!sym(as.character(disease_name))),NA),
      # This allows for dynamic naming of column names, where the symptom from the passed df name
      # -- is extracted as a symbol and converted to a string and assigned to the column name.
      # -- a flag is added if it meets the conditions if the additional symptom occurs within 6 months (otherwise NA == 0).
      "{disease_name}" := ifelse(eventdate >= !!sym(as.character(disease_name)) &
                                   eventdate %m-% months(6) <= !!sym(as.character(disease_name)), 1, 0),
      "{disease_name}" := replace_na(!!sym(disease_name), 0)
    ) %>%
    select(-matches("eventdate.y")) %>%
    clean_names()
}

# Apply function to each symptom

## Dysphagia

# Add each of the symptom patient/date data frames to a list
symptoms_vec <- list(dyspepsia = cohort_dyspepsia, vomiting = cohort_vomiting,
                     abd_pain = cohort_abd_pain,dysphagia =  cohort_dysphagia_anti)
main_dysphagia_1 <- dysphagia

for(symptom_name in names(symptoms_vec)) {
  main_dysphagia_1 <- disease_join(main_dysphagia_1, symptoms_vec[[symptom_name]], symptom_name) %>%
    distinct()
}

## dyspepsia
# Add each of the symptom patient/date data frames to a list
symptoms_vec <- list(dysphagia =  cohort_dysphagia, vomiting =  cohort_vomiting,
                     abd_pain =  cohort_abd_pain, dyspepsia = cohort_dyspepsia_anti)
main_dyspepsia_1 <- dyspepsia

for(symptom_name in names(symptoms_vec)) {
  main_dyspepsia_1 <- disease_join(main_dyspepsia_1, symptoms_vec[[symptom_name]], symptom_name) %>%
    distinct()
}

## abd_pain
# Add each of the symptom patient/date data frames to a list
symptoms_vec <- list(dysphagia =  cohort_dysphagia, vomiting =  cohort_vomiting,
                     dyspepsia =  cohort_dyspepsia, abd_pain = cohort_abd_pain_anti)
main_abd_pain_1 <- abd_pain

for(symptom_name in names(symptoms_vec)) {
  main_abd_pain_1 <- disease_join(main_abd_pain_1, symptoms_vec[[symptom_name]], symptom_name) %>%
    distinct()
}

## vomiting
# # Add each of the symptom patient/date data frames to a list
symptoms_vec <- list(dysphagia =  cohort_dysphagia, abd_pain =  cohort_abd_pain,
                     dyspepsia =  cohort_dyspepsia, vomiting=cohort_vomiting_anti)
main_vomiting_1 <- vomiting

for(symptom_name in names(symptoms_vec)) {
  main_vomiting_1 <- disease_join(main_vomiting_1, symptoms_vec[[symptom_name]], symptom_name) %>%
    distinct()
}

## Function to keep only the positive test result or symptom recording
keep_pos <- function(data,variable,new_variable){
  #data <- out
  sub <- data %>%
    select(e_patid,UQ(variable)) %>%
    arrange(e_patid,UQ(variable)) %>%
    distinct()
  
  n <- sub %>%
    group_by(e_patid) %>%
    count(e_patid)
  
  sub_jn <- left_join(sub,n)
  
  sub_jn <- sub_jn %>%
    mutate("{new_variable}" := ifelse(n>1,1,!!sym(new_variable))) %>%
    select(-n) %>%
    distinct()
  
  data <- data %>%
    select(-c(UQ(variable))) %>%
    distinct()
  
  out <- left_join(data,sub_jn)
  #out <- out %>% select(-c('cancer_site_desc_v2','diagnosisdatebest2'))
  return(out)
}

# Applying the function
#data_list <- list(main_vomiting_1,main_dyspepsia_1,main_dysphagia_1)

#variable = quo('dysphagia') 
#new_variable = as.name('dysphagia')
symp_vec <- c('abd_pain','dyspepsia','dysphagia','vomiting'
              )

main_vomiting_12 <- main_vomiting_1
for(symp_name in (symp_vec)) {
  main_vomiting_12  <- keep_pos(main_vomiting_12 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_abd_pain_12 <- main_abd_pain_1
for(symp_name in (symp_vec)) {
  main_abd_pain_12  <- keep_pos(main_abd_pain_12 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_dysphagia_12 <- main_dysphagia_1
for(symp_name in (symp_vec)) {
  main_dysphagia_12  <- keep_pos(main_dysphagia_12 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_dyspepsia_12 <- main_dyspepsia_1
for(symp_name in (symp_vec)) {
  main_dyspepsia_12  <- keep_pos(main_dyspepsia_12 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

#-- Now sorting the td variables having more than one, need to slightly edit the function
## Function to keep only the positive test result or symptom recording
keep_pos2 <- function(data,variable,new_variable){
  sub <- data %>%
    select(e_patid,UQ(variable)) %>%
    arrange(e_patid,UQ(variable)) %>%
    distinct()
  
  n <- sub %>%
    group_by(e_patid) %>%
    count(e_patid)
  
  sub_jn <- left_join(sub,n)
  
  sub_jn <- sub_jn %>%
    group_by(e_patid) %>%
    mutate(!!sym(new_variable) := ifelse(n>1,min(!!sym(new_variable), na.rm=TRUE),!!sym(new_variable))) %>%
    select(-n) %>%
    distinct()
  
  data <- data %>%
    select(-c(UQ(variable))) %>%
    distinct()
  
  out <- left_join(data,sub_jn)
  return(out)
}

symp_vec <- c("td_dyspepsia","td_dysphagia","td_vomiting","td_abd_pain")  

main_abd_pain_122 <- main_abd_pain_12
for(symp_name in (symp_vec)) {
  main_abd_pain_122  <- keep_pos2(main_abd_pain_122 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_dyspepsia_122 <- main_dyspepsia_12
for(symp_name in (symp_vec)) {
  main_dyspepsia_122  <- keep_pos2(main_dyspepsia_122 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_dysphagia_122 <- main_dysphagia_12
for(symp_name in (symp_vec)) {
  main_dysphagia_122  <- keep_pos2(main_dysphagia_122 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_vomiting_122 <- main_vomiting_12
for(symp_name in (symp_vec)) {
  main_vomiting_122  <- keep_pos2(main_vomiting_122 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

dbWriteTable(db, "cohort_dyspepsia_cr_flag_symp",main_dyspepsia_122,row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_dysphagia_cr_flag_symp",main_dysphagia_122,row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_vomiting_cr_flag_symp",main_vomiting_122,row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_abd_pain_cr_flag_symp",main_abd_pain_122,row.names = FALSE, overwrite = TRUE)

#------------------------------------------------------------------
## 3. Joining non-neoplastic data

dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.cohort_dyspepsia_cr_flag_nn;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_dyspepsia_cr_flag_nn
SELECT v2.e_patid,cast(v2.eventdate as date) as eventdate,v2.readcode_desc,v2.symptom_desc,v2.gender,v2.ever_obesity,v2.ever_alcohol_problems,v2.low_hb,v2.raise_platelets,v2.raise_wbc,v2.h_pylori_flag,v2.weight_loss,v2.cough,v2.appetite_loss,v2.fatigue,v2.abd_pain,v2.vomiting,v2.dysphagia,v2.`td_dysphagia`,v2.`td_vomiting`,v2.`td_abd_pain`,v2.dyspepsia,v2.age_band,v2.age,v2.og_cancer, cast(cl.eventdate as date) as diagdate, cl.disease_name, cl.code_desc, cl.pot_elig, cl.data_source -- , cast(hes.eventdate as date) as discharged, hes.disease_name as disease_name_hes, hes.disease_number as disease_number_hes, cl.pot_elig as cprd_pot_elig, hes.pot_elig as hes_pot_elig
FROM cohort_dyspepsia_cr_flag_symp v2
LEFT JOIN freya.disease_history_all_pot_elig cl ON cl.e_patid = v2.e_patid
-- LEFT JOIN freya.disease_index_cprd_pot_elig_flag hes ON v2.e_patid = hes.e_patid
;'
)

dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.cohort_dysphagia_cr_flag_nn;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_dysphagia_cr_flag_nn
SELECT v2.e_patid,cast(v2.eventdate as date) as eventdate,v2.readcode_desc,v2.symptom_desc,v2.gender,v2.ever_obesity,v2.ever_alcohol_problems,v2.low_hb,v2.raise_platelets,v2.raise_wbc,v2.h_pylori_flag,v2.weight_loss,v2.cough,v2.appetite_loss,v2.fatigue,v2.abd_pain, v2.vomiting,v2.dyspepsia,v2.`td_dyspepsia`,v2.`td_vomiting`,v2.`td_abd_pain`,v2.dysphagia,v2.age_band,v2.age,v2.og_cancer, cast(cl.eventdate as date) as diagdate, cl.disease_name, cl.code_desc, cl.pot_elig, cl.data_source -- , cast(hes.eventdate as date) as discharged, hes.disease_name as disease_name_hes, hes.disease_number as disease_number_hes, cl.pot_elig as cprd_pot_elig, hes.pot_elig as hes_pot_elig
FROM cohort_dysphagia_cr_flag_symp v2
LEFT JOIN freya.disease_history_all_pot_elig cl ON cl.e_patid = v2.e_patid
-- LEFT JOIN freya.disease_index_cprd_pot_elig_flag hes ON v2.e_patid = hes.e_patid
;'
)

dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.cohort_vomiting_cr_flag_nn;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_vomiting_cr_flag_nn
SELECT v2.e_patid,cast(v2.eventdate as date) as eventdate,v2.readcode_desc,v2.symptom_desc,v2.gender,v2.ever_obesity,v2.ever_alcohol_problems,v2.low_hb,v2.raise_platelets,v2.raise_wbc,v2.h_pylori_flag,v2.weight_loss,v2.cough,v2.appetite_loss,v2.fatigue,v2.abd_pain,v2.dysphagia,v2.dyspepsia,v2.`td_dysphagia`,v2.`td_dyspepsia`,v2.`td_abd_pain`,v2.vomiting,v2.age_band,v2.age,v2.og_cancer, cast(cl.eventdate as date) as diagdate,cl.disease_name,cl.code_desc, cl.pot_elig, cl.data_source -- , cast(hes.eventdate as date) as discharged, hes.disease_name as disease_name_hes, hes.disease_number as disease_number_hes, cl.pot_elig as cprd_pot_elig, hes.pot_elig as hes_pot_elig
FROM cohort_vomiting_cr_flag_symp v2
LEFT JOIN freya.disease_history_all_pot_elig cl ON cl.e_patid = v2.e_patid
-- LEFT JOIN freya.disease_index_cprd_pot_elig_flag hes ON v2.e_patid = hes.e_patid
;'
)


dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.cohort_abd_pain_cr_flag_nn;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.cohort_abd_pain_cr_flag_nn
SELECT v2.e_patid,cast(v2.eventdate as date) as eventdate,v2.readcode_desc,v2.symptom_desc,v2.gender,v2.ever_obesity,v2.ever_alcohol_problems,v2.low_hb,v2.raise_platelets,v2.raise_wbc,v2.h_pylori_flag,v2.weight_loss,v2.cough,v2.appetite_loss,v2.fatigue,v2.dysphagia,v2.dyspepsia,v2.vomiting,v2.`td_dysphagia`,v2.`td_vomiting`,v2.`td_dyspepsia`,v2.abd_pain,v2.age_band,v2.age,v2.og_cancer, cast(cl.eventdate as date) as diagdate,cl.disease_name,cl.code_desc, cl.pot_elig, cl.data_source -- , cast(hes.eventdate as date) as discharged, hes.disease_name as disease_name_hes, hes.disease_number as disease_number_hes, cl.pot_elig as cprd_pot_elig, hes.pot_elig as hes_pot_elig
FROM cohort_abd_pain_cr_flag_symp v2
LEFT JOIN freya.disease_history_all_pot_elig cl ON cl.e_patid = v2.e_patid
-- LEFT JOIN freya.disease_index_cprd_pot_elig_flag hes ON v2.e_patid = hes.e_patid
;'
)

query <- 'SELECT * FROM cohort_dyspepsia_cr_flag_nn;'
rs <- dbSendQuery(db, query)
dyspepsia <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_dysphagia_cr_flag_nn;'
rs <- dbSendQuery(db, query)
dysphagia <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_vomiting_cr_flag_nn;'
rs <- dbSendQuery(db, query)
vomiting <- fetch(rs, n=-1)

query <- 'SELECT * FROM cohort_abd_pain_cr_flag_nn;'
rs <- dbSendQuery(db, query)
abd_pain <- fetch(rs, n=-1)

#-------------------------------------------------------------------
#Creating a flag for each non-neoplastic disease of interest 

flag <- function(x,td_var,same_day_var){
  x <- x %>% 
    mutate(eventdate = as.Date(eventdate)) %>%
    mutate(diagdate = as.Date(diagdate))
  
  #flagging a valid diagnosis within 6 months
  data <- x %>%
    mutate(diag_1_yr = ifelse(
      (is.na(diagdate) == FALSE) &
        (disease_name %in% c("oesoph_ulc","gord","barretts","gastritis_duodenitis","hernia_abdo"))&
        (diagdate >= eventdate %m-% months(6)) &
        (diagdate <= eventdate),
      1,0
    )) 
  
  
  #filter records to those with a valid hes or cprd diagnosis and remove unnecessary columns
  data_all <- data %>% 
    filter(diag_1_yr == 1) %>%
    select(-c(diag_1_yr,pot_elig)) 
  
  data_all <- distinct(data_all)
  
  #flag the non-neoplastic diseases of interest individually 
  data_all <- data_all %>%
    mutate(oesoph_ulc = ifelse(disease_name == "oesoph_ulc",1,0)) %>%
    mutate(gord = ifelse(disease_name == "gord",1,0)) %>%
    mutate(barretts = ifelse(disease_name == "barretts",1,0)) %>%
    mutate(gastritis_duodenitis = ifelse(disease_name == "gastritis_duodenitis",1,0)) %>%
    mutate(hernia_abdo = ifelse(disease_name == "hernia_abdo",1,0))%>%
    mutate(td_oesoph_ulc = ifelse(oesoph_ulc == 1,as.numeric(eventdate - diagdate),NA)) %>%
    mutate(td_gord = ifelse(gord == 1,as.numeric(eventdate - diagdate),NA)) %>%
    mutate(td_barretts = ifelse(barretts == 1,as.numeric(eventdate - diagdate),NA)) %>%
    mutate(td_gastritis_duodenitis = ifelse(gastritis_duodenitis == 1,as.numeric(eventdate - diagdate),NA)) %>%
    mutate(td_hernia_abdo = ifelse(hernia_abdo == 1,as.numeric(eventdate - diagdate),NA)) 
  
  #count the number of non-neoplastic outcomes
  data_all_fil <- data_all %>%
    select(e_patid,oesoph_ulc,gord,barretts,gastritis_duodenitis,hernia_abdo)
  
  oesoph_ulc <- aggregate(data_all_fil$oesoph_ulc, by = list(e_patid = data_all_fil$e_patid), FUN=sum)
  gord <- aggregate(data_all_fil$gord, by = list(e_patid = data_all_fil$e_patid), FUN=sum)
  barretts <- aggregate(data_all_fil$barretts, by = list(e_patid = data_all_fil$e_patid), FUN=sum)
  gastritis_duodenitis <- aggregate(data_all_fil$gastritis_duodenitis, by = list(e_patid = data_all_fil$e_patid), FUN=sum)
  hernia_abdo <- aggregate(data_all_fil$hernia_abdo, by = list(e_patid = data_all_fil$e_patid), FUN=sum)
  
  names(oesoph_ulc)[2] <- "oesoph_ulc"
  names(gord)[2] <- "gord"
  names(barretts)[2] <- "barretts"
  names(gastritis_duodenitis)[2] <- "gastritis_duodenitis"
  names(hernia_abdo)[2] <- "hernia_abdo"
  
  data_all_fil_non_neo <- cbind(oesoph_ulc,gord$gord,barretts$barretts,gastritis_duodenitis$gastritis_duodenitis,hernia_abdo$hernia_abdo)
  
  data_all_fil_non_neo[is.na(data_all_fil_non_neo)] <- 0
  
  names(data_all_fil_non_neo)[3] <- "gord"
  names(data_all_fil_non_neo)[4] <- "barretts"
  names(data_all_fil_non_neo)[5] <- "gastritis_duodenitis"
  names(data_all_fil_non_neo)[6] <- "hernia_abdo"
  
  data_all_fil_non_neo <- data_all_fil_non_neo %>%
    mutate(count_non_neo=rowSums(across(c(oesoph_ulc,gord,barretts,gastritis_duodenitis,hernia_abdo)))) 
  
  ###Joining the non-neoplastic diagnoses back into main dataframe of patients with potentially eligible disease diagnoses
  data_all_trunc <- data_all[,-30:-34]
  data_all_trunc <- data_all_trunc %>%
    select(-c(disease_name,code_desc,data_source))
  
  #joining into one dataframe with one row per patient
  data_join <- data_all_trunc %>%
    inner_join(data_all_fil_non_neo, by = 'e_patid') %>%
    group_by(e_patid) %>%
    distinct() %>%
    select(-diagdate)

  ## Joining in those who didn't have a valid diagnosis within a year
  data_no_diag <- data %>% 
    filter(diag_1_yr == 0) %>%
    select(-c(diagdate,diag_1_yr,pot_elig,disease_name,code_desc,data_source))
  
  data_join_patid <- data_join$e_patid
  
  data_no_diag_fil <- data_no_diag %>%
    filter(!e_patid %in% data_join_patid)
  
  data_no_diag_fil <- data_no_diag_fil %>%
    mutate(oesoph_ulc = 0) %>%
    mutate(gord = 0) %>%
    mutate(barretts = 0) %>%
    mutate(gastritis_duodenitis = 0) %>%
    mutate(hernia_abdo = 0) %>%
    mutate(count_non_neo = 0) %>%
    mutate(td_oesoph_ulc = NA) %>%
    mutate(td_gord= NA) %>%
    mutate(td_gastritis_duodenitis = NA) %>%
    mutate(td_barretts = NA) %>%
    mutate(td_hernia_abdo = NA)
  
  data_union_fin <- union(data_join,data_no_diag_fil)
  
  data_union_fin <- data_union_fin %>%
    mutate(same_day_barretts = ifelse(td_barretts == 0,1,0),
           same_day_gord = ifelse(td_gord == 0,1,0),
           same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis == 0,1,0),
           same_day_oesoph_ulc = ifelse(td_oesoph_ulc == 0,1,0),
           same_day_hernia_abdo = ifelse(td_hernia_abdo == 0,1,0))
  
}

dyspepsia_join <- flag(dyspepsia) #177391
dysphagia_join <- flag(dysphagia) #46242
vomiting_join <- flag(vomiting) #73333
abd_pain_join <- flag(abd_pain) #449135

#--------------------------------------------
#removing repeats with the same function as before
symp_vec <- c('barretts','same_day_barretts','gord','same_day_gord','oesoph_ulc','same_day_oesoph_ulc',
              'hernia_abdo'
              )

dyspepsia_join2 <- dyspepsia_join
for(symp_name in (symp_vec)) {
  dyspepsia_join2  <- keep_pos(dyspepsia_join2 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

vomiting_join2 <- vomiting_join
for(symp_name in (symp_vec)) {
  vomiting_join2  <- keep_pos(vomiting_join2 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

dysphagia_join2 <- dysphagia_join
for(symp_name in (symp_vec)) {
  dysphagia_join2  <- keep_pos(dysphagia_join2 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

abd_pain_join2 <- abd_pain_join
for(symp_name in (symp_vec)) {
  abd_pain_join2  <- keep_pos(abd_pain_join2 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

symp_vec <- c("td_barretts","td_gord","td_oesoph_ulc","td_hernia_abdo","td_gastritis_duodenitis")  

abd_pain_join22 <- abd_pain_join2
for(symp_name in (symp_vec)) {
  abd_pain_join22  <- keep_pos2(abd_pain_join22 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

dyspepsia_join22 <- dyspepsia_join2
for(symp_name in (symp_vec)) {
  dyspepsia_join22  <- keep_pos2(dyspepsia_join22 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

dysphagia_join22 <- dysphagia_join2
for(symp_name in (symp_vec)) {
  dysphagia_join22  <- keep_pos2(dysphagia_join22 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

vomiting_join22 <- vomiting_join2
for(symp_name in (symp_vec)) {
  vomiting_join22  <- keep_pos2(vomiting_join22 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

#----------------------------------------------------------------------
#creating a same day diagnosis column for all the main features
vomiting_join22 <- vomiting_join22 %>% 
  mutate(same_day_gord = ifelse(td_gord !=0 | is.na(td_gord),0,1))%>% 
  mutate(same_day_barretts = ifelse(td_barretts !=0 | is.na(td_barretts),0,1))%>% 
  mutate(same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis !=0 | is.na(td_gastritis_duodenitis),0,1))%>% 
  mutate(same_day_dyspepsia = ifelse(td_dyspepsia !=0 | is.na(td_dyspepsia),0,1))%>% 
  mutate(same_day_dysphagia = ifelse(td_dysphagia !=0 | is.na(td_dysphagia),0,1))%>% 
  mutate(same_day_abd_pain = ifelse(td_abd_pain !=0 | is.na(td_abd_pain),0,1))%>% 
  mutate(same_day_hernia_abdo = ifelse(td_hernia_abdo !=0 | is.na(td_hernia_abdo),0,1))%>% 
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc !=0 | is.na(td_oesoph_ulc),0,1))

dyspepsia_join22 <- dyspepsia_join22 %>% 
  mutate(same_day_gord = ifelse(td_gord !=0 | is.na(td_gord),0,1))%>% 
  mutate(same_day_barretts = ifelse(td_barretts !=0 | is.na(td_barretts),0,1))%>% 
  mutate(same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis !=0 | is.na(td_gastritis_duodenitis),0,1))%>% 
  mutate(same_day_dysphagia = ifelse(td_dysphagia !=0 | is.na(td_dysphagia),0,1))%>% 
  mutate(same_day_abd_pain = ifelse(td_abd_pain !=0 | is.na(td_abd_pain),0,1))%>% 
  mutate(same_day_vomiting = ifelse(td_vomiting !=0 | is.na(td_vomiting),0,1))%>% 
  mutate(same_day_hernia_abdo = ifelse(td_hernia_abdo !=0 | is.na(td_hernia_abdo),0,1))%>% 
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc !=0 | is.na(td_oesoph_ulc),0,1))

dysphagia_join22 <- dysphagia_join22 %>% 
  mutate(same_day_gord = ifelse(td_gord !=0 | is.na(td_gord),0,1))%>% 
  mutate(same_day_barretts = ifelse(td_barretts !=0 | is.na(td_barretts),0,1))%>% 
  mutate(same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis !=0 | is.na(td_gastritis_duodenitis),0,1))%>% 
  mutate(same_day_dyspepsia = ifelse(td_dyspepsia !=0 | is.na(td_dyspepsia),0,1))%>% 
  mutate(same_day_abd_pain = ifelse(td_abd_pain !=0 | is.na(td_abd_pain),0,1))%>% 
  mutate(same_day_vomiting = ifelse(td_vomiting !=0 | is.na(td_vomiting),0,1))%>% 
  mutate(same_day_hernia_abdo = ifelse(td_hernia_abdo !=0 | is.na(td_hernia_abdo),0,1))%>% 
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc !=0 | is.na(td_oesoph_ulc),0,1))

abd_pain_join22 <- abd_pain_join22 %>% 
  mutate(same_day_gord = ifelse(td_gord !=0 | is.na(td_gord),0,1))%>% 
  mutate(same_day_barretts = ifelse(td_barretts !=0 | is.na(td_barretts),0,1))%>% 
  mutate(same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis !=0 | is.na(td_gastritis_duodenitis),0,1))%>% 
  mutate(same_day_dyspepsia = ifelse(td_dyspepsia !=0 | is.na(td_dyspepsia),0,1))%>% 
  mutate(same_day_dysphagia = ifelse(td_dysphagia !=0 | is.na(td_dysphagia),0,1))%>% 
  mutate(same_day_vomiting = ifelse(td_vomiting !=0 | is.na(td_vomiting),0,1))%>% 
  mutate(same_day_hernia_abdo = ifelse(td_hernia_abdo !=0 | is.na(td_hernia_abdo),0,1))%>% 
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc !=0 | is.na(td_oesoph_ulc),0,1))

#--------------------------------------------------------------------------------------------------------------------
#getting same day diagnoses into excel
#------------------------------------------------------------------------------------------------------
hernia_abdo <- vomiting_join22 %>% filter(hernia_abdo == 1)
hernia_abdo <- table(hernia_abdo$same_day_hernia_abdo)

gastritis_duodenitis <- vomiting_join22 %>% filter(gastritis_duodenitis == 1)
gastritis_duodenitis <- table(gastritis_duodenitis$same_day_gastritis_duodenitis)

oesoph_ulc <- vomiting_join22 %>% filter(oesoph_ulc == 1)
oesoph_ulc <- table(oesoph_ulc$same_day_oesoph_ulc)

gord <- vomiting_join22 %>% filter(gord == 1)
gord <- table(gord$same_day_gord)

dyspepsia <- vomiting_join22 %>% filter(dyspepsia == 1)
dyspepsia <- table(dyspepsia$same_day_dyspepsia)

dysphagia <- vomiting_join22 %>% filter(dysphagia == 1)
dysphagia <- table(dysphagia$same_day_dysphagia)

abd_pain <- vomiting_join22 %>% filter(abd_pain == 1)
abd_pain <- table(abd_pain$same_day_abd_pain)

barretts <- vomiting_join22 %>% filter(barretts == 1)
barretts <- table(barretts$same_day_barretts)

same_day_vomiting <- rbind(hernia_abdo,dyspepsia,dysphagia,gastritis_duodenitis,gord,oesoph_ulc,abd_pain,barretts)

same_day_vomiting <- same_day_vomiting %>%
  as.data.frame() %>%
  mutate(prop = round(`1`/(`1`+`0`),3))

xlsx::write.xlsx(same_day_vomiting,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "Vomiting",append =TRUE)

#-------------------------------------------------------------------------------------------
hernia_abdo <- dyspepsia_join22 %>% filter(hernia_abdo == 1)
hernia_abdo <- table(hernia_abdo$same_day_hernia_abdo)

gastritis_duodenitis <- dyspepsia_join22 %>% filter(gastritis_duodenitis == 1)
gastritis_duodenitis <- table(gastritis_duodenitis$same_day_gastritis_duodenitis)

oesoph_ulc <- dyspepsia_join22 %>% filter(oesoph_ulc == 1)
oesoph_ulc <- table(oesoph_ulc$same_day_oesoph_ulc)

gord <- dyspepsia_join22 %>% filter(gord == 1)
gord <- table(gord$same_day_gord)

dysphagia <- dyspepsia_join22 %>% filter(dysphagia == 1)
dysphagia <- table(dysphagia$same_day_dysphagia)

abd_pain <- dyspepsia_join22 %>% filter(abd_pain == 1)
abd_pain <- table(abd_pain$same_day_abd_pain)

vomiting <- dyspepsia_join22 %>% filter(vomiting == 1)
vomiting <- table(vomiting$same_day_vomiting)

barretts <- dyspepsia_join22 %>% filter(barretts == 1)
barretts <- table(barretts$same_day_barretts)

same_day_dyspepsia <- rbind(hernia_abdo,vomiting,dysphagia,gastritis_duodenitis,gord,oesoph_ulc,abd_pain,barretts)

same_day_dyspepsia <- same_day_dyspepsia %>%
  as.data.frame() %>%
  mutate(prop = round(`1`/(`1`+`0`),3))

xlsx::write.xlsx(same_day_dyspepsia,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "dyspepsia",append =TRUE)

#-------------------------------------------------------------------------------------------
hernia_abdo <- dysphagia_join22 %>% filter(hernia_abdo == 1)
hernia_abdo <- table(hernia_abdo$same_day_hernia_abdo)

gastritis_duodenitis <- dysphagia_join22 %>% filter(gastritis_duodenitis == 1)
gastritis_duodenitis <- table(gastritis_duodenitis$same_day_gastritis_duodenitis)

oesoph_ulc <- dysphagia_join22 %>% filter(oesoph_ulc == 1)
oesoph_ulc <- table(oesoph_ulc$same_day_oesoph_ulc)

gord <- dysphagia_join22 %>% filter(gord == 1)
gord <- table(gord$same_day_gord)

dyspepsia <- dysphagia_join22 %>% filter(dyspepsia == 1)
dyspepsia <- table(dyspepsia$same_day_dyspepsia)

abd_pain <- dysphagia_join22 %>% filter(abd_pain == 1)
abd_pain <- table(abd_pain$same_day_abd_pain)

vomiting <- dysphagia_join22 %>% filter(vomiting == 1)
vomiting <- table(vomiting$same_day_vomiting)

barretts <- dysphagia_join22 %>% filter(barretts == 1)
barretts <- table(barretts$same_day_barretts)

same_day_dysphagia <- rbind(hernia_abdo,dyspepsia,vomiting,gastritis_duodenitis,gord,oesoph_ulc,abd_pain,barretts)

same_day_dysphagia <- same_day_dysphagia %>%
  as.data.frame() %>%
  mutate(prop = round(`1`/(`1`+`0`),3))

xlsx::write.xlsx(same_day_dysphagia,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "dysphagia",append =TRUE)

#-------------------------------------------------------------------------------------------
hernia_abdo <- abd_pain_join22 %>% filter(hernia_abdo == 1)
hernia_abdo <- table(hernia_abdo$same_day_hernia_abdo)

gastritis_duodenitis <- abd_pain_join22 %>% filter(gastritis_duodenitis == 1)
gastritis_duodenitis <- table(gastritis_duodenitis$same_day_gastritis_duodenitis)

oesoph_ulc <- abd_pain_join22 %>% filter(oesoph_ulc == 1)
oesoph_ulc <- table(oesoph_ulc$same_day_oesoph_ulc)

gord <- abd_pain_join22 %>% filter(gord == 1)
gord <- table(gord$same_day_gord)

dyspepsia <- abd_pain_join22 %>% filter(dyspepsia == 1)
dyspepsia <- table(dyspepsia$same_day_dyspepsia)

dysphagia <- abd_pain_join22 %>% filter(dysphagia == 1)
dysphagia <- table(dysphagia$same_day_dysphagia)

vomiting <- abd_pain_join22 %>% filter(vomiting == 1)
vomiting <- table(vomiting$same_day_vomiting)

barretts <- abd_pain_join22 %>% filter(barretts == 1)
barretts <- table(barretts$same_day_barretts)

same_day_abd_pain <- rbind(hernia_abdo,dysphagia,vomiting,gastritis_duodenitis,gord,oesoph_ulc,dyspepsia,barretts)

same_day_abd_pain <- same_day_abd_pain %>%
  as.data.frame() %>%
  mutate(prop = round(`1`/(`1`+`0`),3))

xlsx::write.xlsx(same_day_abd_pain,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "abd_pain",append =TRUE)
#-----------------------------------------------------------------------------------
#getting time to diagnosis summary
#------------------------------------------------------------------------------------
hernia_abdo <- summary(vomiting_join22$td_hernia_abdo)
gastritis_duodenitis <- summary(vomiting_join22$td_gastritis_duodenitis)
oesoph_ulc <- summary(vomiting_join22$td_oesoph_ulc)
gord <- summary(vomiting_join22$td_gord)
dyspepsia <- summary(vomiting_join22$td_dyspepsia)
dysphagia <- summary(vomiting_join22$td_dysphagia)
abd_pain <- summary(vomiting_join22$td_abd_pain)
barretts <- summary(vomiting_join22$td_barretts)

td_vomiting <- rbind(hernia_abdo,dyspepsia,dysphagia,gastritis_duodenitis,gord,oesoph_ulc,abd_pain,barretts)

xlsx::write.xlsx(td_vomiting,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "Vomiting_ttd",append =TRUE)

hernia_abdo <- summary(dyspepsia_join22$td_hernia_abdo)
gastritis_duodenitis <- summary(dyspepsia_join22$td_gastritis_duodenitis)
oesoph_ulc <- summary(dyspepsia_join22$td_oesoph_ulc)
gord <- summary(dyspepsia_join22$td_gord)
dysphagia <- summary(dyspepsia_join22$td_dysphagia)
abd_pain <- summary(dyspepsia_join22$td_abd_pain)
vomiting <- summary(dyspepsia_join22$td_vomiting)
barretts <- summary(dyspepsia_join22$td_barretts)

td_dyspepsia <- rbind(hernia_abdo,vomiting,dysphagia,gastritis_duodenitis,gord,oesoph_ulc,abd_pain,barretts)

xlsx::write.xlsx(td_dyspepsia,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "dyspepsia_ttd",append =TRUE)


hernia_abdo <- summary(dysphagia_join22$td_hernia_abdo)
gastritis_duodenitis <- summary(dysphagia_join22$td_gastritis_duodenitis)
oesoph_ulc <- summary(dysphagia_join22$td_oesoph_ulc)
gord <- summary(dysphagia_join22$td_gord)
dyspepsia <- summary(dysphagia_join22$td_dyspepsia)
abd_pain <- summary(dysphagia_join22$td_abd_pain)
vomiting <- summary(dysphagia_join22$td_vomiting)
barretts <- summary(dysphagia_join22$td_barretts)

td_dysphagia <- rbind(hernia_abdo,dyspepsia,vomiting,gastritis_duodenitis,gord,oesoph_ulc,abd_pain,barretts)

xlsx::write.xlsx(td_dysphagia,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "dysphagia_ttd",append =TRUE)

hernia_abdo <- summary(abd_pain_join22$td_hernia_abdo)
gastritis_duodenitis <- summary(abd_pain_join22$td_gastritis_duodenitis)
oesoph_ulc <- summary(abd_pain_join22$td_oesoph_ulc)
gord <- summary(abd_pain_join22$td_gord)
dyspepsia <- summary(abd_pain_join22$td_dyspepsia)
dysphagia <- summary(abd_pain_join22$td_dysphagia)
vomiting <- summary(abd_pain_join22$td_vomiting)
barretts <- summary(abd_pain_join22$td_barretts)

td_abd_pain <- rbind(hernia_abdo,dysphagia,vomiting,gastritis_duodenitis,gord,oesoph_ulc,dyspepsia,barretts)

xlsx::write.xlsx(td_abd_pain,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "abd_pain_ttd",append =TRUE)

#--------------------------------------------------------------------
# saving the dataframe to sql to calculate elixhauser codes
vomiting_join22 <- distinct(vomiting_join22)
dyspepsia_join22 <- distinct(dyspepsia_join22)
dysphagia_join22 <- distinct(dysphagia_join22)
abd_pain_join22 <- distinct(abd_pain_join22)

dbWriteTable(db, "cohort_dyspepsia_cr_flag_nn_flag",dyspepsia_join22,row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_dysphagia_cr_flag_nn_flag",dysphagia_join22,row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_vomiting_cr_flag_nn_flag",vomiting_join22,row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_abd_pain_cr_flag_nn_flag",abd_pain_join22,row.names = FALSE, overwrite = TRUE)
