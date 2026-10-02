rm(list=ls())

#### Loading Packages ####
library(pacman)

pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI, xlsx)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)


query <- 'SELECT * FROM freya.random_sample_2_v2;'
rs <- dbSendQuery(db, query)
data <- fetch(rs, n=-1)

#------------------------------------------------------------------------------------------
#### Define potentially eligible symptom events ####

# Year of birth as date variable
data_join_chron <- data %>%
  mutate(yob_approx = as.Date(ISOdate(data$yob,8,1)))

# Year turned 30
data_join_chron <- data_join_chron %>%
  mutate(age30date = data_join_chron$yob_approx %m+% years(30))

# Year turned 100
data_join_chron <- data_join_chron %>%
  mutate(age100date = data_join_chron$yob_approx %m+% years(100))

# Format dates as date
data_join_chron <- data_join_chron %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(crd = as.Date(crd)) %>%
  mutate(uts = as.Date(uts)) %>%
  mutate(lcd = as.Date(lcd)) %>%
  mutate(tod = as.Date(tod)) %>%
  mutate(deathdate = as.Date(deathdate)) |>
  mutate(age30date = as.Date(age30date, format = "%Y-%m-%d")) |>
  mutate(age100date = as.Date(age100date, format = "%Y-%m-%d")) #%>%

#-----------------

# Study start & end values

# Study_start: Cohort from CPRD is from start 2007
study_start <- as.Date("2007-01-01")

# Study end: NCRAS data will be until end of 2018, 
# so I should include my up to end of 2017
study_end <- as.Date("2017-12-31")

# Follow up start
# After: study start, 1 year After UTS and CRD date, after age 30
data_join_chron <- data_join_chron |> 
  mutate(fu_start = pmax((crd %m+% years(1)),
                         (uts %m+% years(1)),
                         age30date,
                         study_start
                         , na.rm = TRUE))

# Follow up end
# Before: study end, before lcd or tod, death date, age 100, or study end
data_join_chron <- data_join_chron |> 
  mutate(fu_end = pmin(lcd,
                       tod,
                       deathdate,
                       age100date,
                       study_end
                       , na.rm = TRUE))

# Potentially eligible flag
data_join_chron <- data_join_chron |>
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


#Removing unnecessary columns
data_join_chron <- data_join_chron %>% select(e_patid, eventdate,gender,diagnosisdatebest,site_icd10_o2,cancer_site_desc,yob_approx,pot_elig)

#------------------------------------------------------
## Adding in the OG cancer flag

cohort_join <- data_join_chron %>%
  mutate(diagnosisdatebest = as.Date(diagnosisdatebest))

#need to filter out those with non cancerous icd 10 codes before excluding those with cancer
cohort_join <- cohort_join %>%
  mutate(cancer_site_desc_v2 = ifelse(cancer_site_desc == "Non cancerous ICD10",NA,cancer_site_desc))

cohort_join$diagnosisdatebest2 <- ifelse(is.na(cohort_join$cancer_site_desc_v2)==TRUE,NA,cohort_join$diagnosisdatebest)
cohort_join<- cohort_join %>% mutate(diagnosisdatebest2 = as.Date(diagnosisdatebest2))

# flagging cancer in 5 years
cohort_join <- cohort_join %>%
  mutate(prev_can_flag = ifelse(
    cancer_site_desc_v2 != "Stomach" &
      cancer_site_desc_v2 != "Oesophagus" &
      cancer_site_desc_v2 != "Pancreas" &
      cancer_site_desc_v2 != "Liver" &
      cancer_site_desc_v2 != "Bladder" &
    (eventdate > diagnosisdatebest2) &
      (eventdate < diagnosisdatebest2 %m+% years(5)) &
      (is.na(cancer_site_desc_v2) == FALSE),1,0
  ))

## taking ids for those with stomach or oesophageal cancer
n <- cohort_join %>%
  group_by(e_patid, eventdate) %>%
  filter(cancer_site_desc_v2 == "Stomach" | cancer_site_desc_v2 == "Oesophagus") %>%
  ungroup() %>%
  select(e_patid)

#joining into dataframe to see other diagnoses
data_elig_jn <- left_join(n,cohort_join)

#taking only the oesophageal and stomach diagnoses for those with more than one diagnosis on an eventdate
jn_f <- data_elig_jn %>%
  group_by(e_patid,eventdate) %>%
  summarise(count=n()) %>%
  mutate(xtra = ifelse(count >1,1,0)) %>%
  filter(xtra>0)

#taking a vector of patient IDs  with more than one diagnosis per data to filter out of the previous dataframe
jn_fe <- jn_f %>% select(e_patid,eventdate)
anti_pre <- anti_join(data_elig_jn,jn_fe)

#joining in to the same dataframe for those who did have more than one eligible index
#then filtering those who had more than one index to be stomach or oesophageal
joinpre <- left_join(jn_fe,data_elig_jn)

joinpre_fil <- joinpre %>%
  filter(cancer_site_desc_v2 == "Stomach" | cancer_site_desc_v2 == "Oesophagus")

union_pre <- union(anti_pre,joinpre)

#anti-joining the original dataframe with the patient IDs for those with stomach or oesophageal diagnoses
anti <- anti_join(cohort_join,n)

#union to add the patients with those diagnoses back in without the extra diagnoses/blanks 
union_post <- union(anti,union_pre)

#-----------------------------------------------------------------------------------------------------
#### Flagging records where patient has a oesophago-gastric cancer diagnosis within 1 year of index date ####
#edit on original back to upper GI cancer

union_post <- union_post %>%
  mutate(diag_1_yr = ifelse(
    (is.na(diagnosisdatebest2) == FALSE) &
      (cancer_site_desc_v2 == "Stomach" | cancer_site_desc_v2 == "Oesophagus") &
      (diagnosisdatebest2 >= eventdate) &
      (diagnosisdatebest2 <= eventdate %m+% years(1)),
    1,0
  ))

#------------------------------------------------------------------------------
#Filtering potentially eligible patients and those without cancer in the last 5 years, then taking a random index per patient
set.seed(3214)
elig <- union_post %>%
  group_by(e_patid) %>%
  filter(prev_can_flag == 0) %>%
  filter(pot_elig == 1) %>%
  sample_n(1) %>%
  ungroup()

#----------------------------------
#Creating an age band variable for age at index symptom
elig <- elig %>%
  mutate(age = round(time_length(difftime(eventdate,yob_approx),"years"),0))

elig <- elig %>%
  mutate(age_band = cut(age,breaks = c(30,40,50,60,70,80,90,100),include.lowest = TRUE, right = FALSE)) %>%
  select(-c(yob_approx))

elig <- distinct(elig)

elig$age_band <- gsub("([,])|[[:punct:]]","\\1",elig$age_band)
length(unique(elig$e_patid))

elig$age_band <- ifelse(elig$age_band == "80,90"|elig$age_band == "90,100","80+",elig$age_band)

elig <- elig %>% select(e_patid,eventdate,gender,age,age_band,diag_1_yr)

#-------------------------------

dbWriteTable(db, "random_sample_elig_v2",elig, row.names = FALSE, overwrite = TRUE)

#Go to R script 4.2
