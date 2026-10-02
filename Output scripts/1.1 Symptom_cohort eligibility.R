rm(list=ls())

#### Loading Packages ####
library(pacman)
pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

query <- 'SELECT * FROM freya.cohortv0_3;'
rs <- dbSendQuery(db, query)
data <- fetch(rs, n=-1)
#------------------------------------------------------------------------------------------
#### Define potentially eligible symptom events ####

# Year of birth as date variable
data <- data %>%
  mutate(yob_approx = as.Date(ISOdate(data$yob,8,1)))

# Year turned 30
data <- data %>%
  mutate(age30date = data$yob_approx %m+% years(30))

# Year turned 100
data <- data %>%
  mutate(age100date = data$yob_approx %m+% years(100))

# Format dates as date
data <- data %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(crd = as.Date(crd)) %>%
  mutate(uts = as.Date(uts)) %>%
  mutate(lcd = as.Date(lcd)) %>%
  mutate(tod = as.Date(tod)) %>%
  mutate(deathdate = as.Date(deathdate)) |>
  mutate(age30date = as.Date(age30date, format = "%Y-%m-%d")) |>
  mutate(age100date = as.Date(age100date, format = "%Y-%m-%d"))


#-----------------
# Study start & end values

# Study_start: Cohort from CPRD is from start 2007
study_start <- as.Date("2007-01-01")

# Study end: NCRAS data will be until end of 2018, 
# so I should include my up to end of 2017
study_end <- as.Date("2017-12-31")

# Follow up start
# After: study start, 1 year After UTS and CRD date, after age 30
data <- data |> 
  mutate(fu_start = pmax((crd %m+% years(1)),
                         (uts %m+% years(1)),
                         age30date,
                         study_start
                         , na.rm = TRUE))

# Follow up end
# Before: study end, before lcd or tod, death date, age 100, or study end
data <- data |> 
  mutate(fu_end = pmin(lcd,
                       tod,
                       deathdate,
                       age100date,
                       study_end
                       , na.rm = TRUE))

# Potentially eligible flag
data <- data |>
  mutate(pot_elig = ifelse (
    eventdate >= (crd %m+% years(1)) &
      eventdate >= (uts %m+% years(1))  &
      eventdate >= age30date &
      eventdate >= study_start &
      eventdate <= lcd & 
      (eventdate <= tod | is.na(tod))   & 
      (eventdate <= deathdate |  is.na(deathdate)) &
      eventdate <= age100date &
      eventdate <= study_end
    ,
    1, 0
  ))

dbWriteTable(db, "cohortv0_5",data,row.names= FALSE, overwrite = TRUE)

#--------------------------------------------------------------------------------
## 1. Joining cancer registry data -----------------
dbSendQuery(
  conn = db,
  statement = "
DROP TABLE IF EXISTS freya.cohortv0_6;"
)

dbSendQuery(
  conn = db,
  statement = "
CREATE TABLE freya.cohortv0_6
SELECT v5.*, crt.diagnosisdatebest, crt.site_icd10_o2, crt.stage_best, icd.cancer_site_desc, icd.cancer_group_desc
FROM cohortv0_5 v5
LEFT JOIN 18_299_Lyratzopoulos_e2.cancer_registration_tumour crt on crt.e_patid = v5.e_patid
LEFT JOIN lookup_tables.lookup_cancersite icd on crt.site_icd10_o2 = icd.icd10_4dig
;
"
)

query <- 'SELECT * FROM freya.cohortv0_6;'
rs <- dbSendQuery(db, query)
cohort_join <- fetch(rs, n=-1)

#--------------------------------------------------------
#Creating an age band variable for age at index symptom

cohort_join <- cohort_join %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(yob_approx = as.Date(yob_approx)) %>%
  mutate(diagnosisdatebest = as.Date(diagnosisdatebest))
  
cohort_join <- cohort_join %>%
  mutate(age = round(time_length(difftime(eventdate,yob_approx),"years"),0))

cohort_join <- cohort_join %>%
  mutate(age_band = cut(age, breaks = c(30,40,50,60,70,80,90,100),include.lowest = TRUE, right = FALSE)) %>%
  select(-c(yob_approx)) #%>%
  
cohort_join$age_band <- gsub("([,])|[[:punct:]]","\\1",cohort_join$age_band)

cohort_join$age_band = ifelse(cohort_join$age_band == '80,90'|cohort_join$age_band == '90,100','80+',cohort_join$age_band)

 #-------------------------------------------------------------------------------------------
#### Flagging patients with a Cancer diagnosis in the 5 years before eventdate ####

#need to filter out those with non cancerous icd 10 codes before excluding those with cancer
cohort_join <- cohort_join %>%
  mutate(cancer_group_desc_v2 = ifelse(cancer_group_desc == "Non cancerous ICD10",NA,cancer_group_desc))

cohort_join$diagnosisdatebest2 <- ifelse(is.na(cohort_join$cancer_group_desc_v2)==TRUE,NA,cohort_join$diagnosisdatebest)
cohort_join<- cohort_join %>% mutate(diagnosisdatebest2 = as.Date(diagnosisdatebest2))

# flagging cancer in 5 years
cohort_join1 <- cohort_join %>%
  mutate(prev_can_flag = ifelse(
    cancer_group_desc_v2 == "Upper GI" &
    (eventdate > diagnosisdatebest2) &
      (eventdate < diagnosisdatebest2 %m+% years(5)) &
      (is.na(cancer_group_desc_v2) == FALSE),1,0
  ))

#filtering out non cancerous icd 10 in site specific column also
cohort_join1 <- cohort_join1 %>%
  mutate(cancer_site_desc_v2 = ifelse(cancer_site_desc == "Non cancerous ICD10",NA,cancer_site_desc)) %>%
  select(-"cancer_site_desc") %>%
  mutate(stomach_c =ifelse(cancer_site_desc_v2 == "Stomach" & !is.na(cancer_site_desc_v2),1,0)) %>%
  mutate(oesophageal_c = ifelse(cancer_site_desc_v2 == "Oesophagus"& !is.na(cancer_site_desc_v2),1,0)) %>%
  mutate(s_or_o_cancer = ifelse(stomach_c == 1 | oesophageal_c == 1,1,0))

#Saving the table before filtering for eligibility
dbWriteTable(db, "cohortv0_7",cohort_join1, row.names = FALSE, overwrite = TRUE)

cohort_fil <- cohort_join1 %>%
  filter(pot_elig == 1) %>%
  filter(prev_can_flag == 0) %>%
  select(e_patid, eventdate,medcode, readcode_desc,symptom_desc,age_band,age,gender,diagnosisdatebest2, cancer_group_desc_v2, cancer_site_desc_v2,stomach_c,oesophageal_c,s_or_o_cancer) 

#Saving the table after filtering for eligibility
dbWriteTable(db, "cohortv1",cohort_fil,row.names = FALSE, overwrite = TRUE)

##Go to SQL script 1.2