rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI,writexl)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)


query <- 'SELECT * FROM freya.cohortv1_1_2;'
rs <- dbSendQuery(db, query)
cohort_join <- fetch(rs, n=-1)

cohort_join <- cohort_join %>%
  mutate(eventdate=as.Date(eventdate))

#-------------------------------------------------------
#first taking symptom indexes of all main symptoms of interest based on a symptom flag

#Creating flag for each symptom
cohort_union_all <- cohort_join %>%
  mutate(Appetite_loss=ifelse(
    grepl("appetite",readcode_desc,ignore.case = T),1,0)) %>%
  mutate(Vomiting=ifelse(
    (grepl("vomit",readcode_desc,ignore.case = T)),1,0)) %>%
  mutate(Dysphagia=ifelse(
    symptom_desc == "Dysphagia ",1,0)) %>%
  mutate(Weight_loss=ifelse(
    symptom_desc == "Weight loss",1,0)) %>%
  mutate(Dyspepsia=ifelse(
    (symptom_desc == "Dyspepsia ") ,1,0)) %>%
  mutate(Abdominal_pain=ifelse(
    symptom_desc == "Abdominal pain",1,0)) %>%
  mutate(Cough =ifelse(
    symptom_desc == "Cough ",1,0)) %>%
  mutate(Fatigue = ifelse(symptom_desc == "Fatigue",1,0))

#getting the count of symptom events per patient
data_agg <- cohort_union_all %>%
  group_by(e_patid) %>%
  summarise(dyspepsia = sum(Dyspepsia),
                            dysphagia = sum(Dysphagia),
                            abd_pain = sum(Abdominal_pain),
                            vomiting = sum(Vomiting))

data_out <- cohort_union_all %>%
  arrange(e_patid, Dyspepsia, eventdate) %>%
  group_by(e_patid, Dyspepsia,gender) %>%
  mutate(days_since_last = ifelse(Dyspepsia == 1, as.numeric(difftime(eventdate, lag(eventdate),units="days")),0),
         is_new_event = ifelse(is.na(days_since_last) | days_since_last >= 7,1,0)) %>%
  filter(is_new_event == 1) %>%
           summarise(event_count = n(),
                     .groups="drop")
dyspepsia <- data_out %>%
  group_by(gender) %>%
  summarise(across(event_count,.fns = list(
    min=min,
    q25=~quantile(.,0.25),
    median=median,
    mean=mean,
    q75=~quantile(.,0.75),
    max=max
  )))

data_out <- cohort_union_all %>%
  arrange(e_patid, Dysphagia, eventdate) %>%
  group_by(e_patid, Dysphagia,gender) %>%
  mutate(days_since_last = ifelse(Dysphagia == 1, as.numeric(difftime(eventdate, lag(eventdate),units="days")),0),
         is_new_event = ifelse(is.na(days_since_last) | days_since_last >= 7,1,0)) %>%
  filter(is_new_event == 1) %>%
  summarise(event_count = n(),
            .groups="drop")
Dysphagia <- data_out %>%
  group_by(gender) %>%
  summarise(across(event_count,.fns = list(
    min=min,
    q25=~quantile(.,0.25),
    median=median,
    mean=mean,
    q75=~quantile(.,0.75),
    max=max
  )))

data_out <- cohort_union_all %>%
  arrange(e_patid, Vomiting, eventdate) %>%
  group_by(e_patid, Vomiting,gender) %>%
  mutate(days_since_last = ifelse(Vomiting == 1, as.numeric(difftime(eventdate, lag(eventdate),units="days")),0),
         is_new_event = ifelse(is.na(days_since_last) | days_since_last >= 7,1,0)) %>%
  filter(is_new_event == 1) %>%
  summarise(event_count = n(),
            .groups="drop")
Vomiting <- data_out %>%
  group_by(gender) %>%
  summarise(across(event_count,.fns = list(
    min=min,
    q25=~quantile(.,0.25),
    median=median,
    mean=mean,
    q75=~quantile(.,0.75),
    max=max
  )))

data_out <- cohort_union_all %>%
  arrange(e_patid, Abdominal_pain, eventdate) %>%
  group_by(e_patid, Abdominal_pain,gender) %>%
  mutate(days_since_last = ifelse(Abdominal_pain == 1, as.numeric(difftime(eventdate, lag(eventdate),units="days")),0),
         is_new_event = ifelse(is.na(days_since_last) | days_since_last >= 7,1,0)) %>%
  filter(is_new_event == 1) %>%
  summarise(event_count = n(),
            .groups="drop")
Abdominal_pain <- data_out %>%
  group_by(gender) %>%
  summarise(across(event_count,.fns = list(
    min=min,
    q25=~quantile(.,0.25),
    median=median,
    mean=mean,
    q75=~quantile(.,0.75),
    max=max
  )))

data_out <- cohort_union_all %>%
  arrange(e_patid, Cough, eventdate) %>%
  group_by(e_patid, Cough,gender) %>%
  mutate(days_since_last = ifelse(Cough == 1, as.numeric(difftime(eventdate, lag(eventdate),units="days")),0),
         is_new_event = ifelse(is.na(days_since_last) | days_since_last >= 7,1,0)) %>%
  filter(is_new_event == 1) %>%
  summarise(event_count = n(),
            .groups="drop")
Cough <- data_out %>%
  group_by(gender) %>%
  summarise(across(event_count,.fns = list(
    min=min,
    q25=~quantile(.,0.25),
    median=median,
    mean=mean,
    q75=~quantile(.,0.75),
    max=max
  )))

data_out <- cohort_union_all %>%
  arrange(e_patid, Abdominal_pain, eventdate) %>%
  group_by(e_patid, Abdominal_pain,gender) %>%
  mutate(days_since_last = ifelse(Abdominal_pain == 1, as.numeric(difftime(eventdate, lag(eventdate),units="days")),0),
         is_new_event = ifelse(is.na(days_since_last) | days_since_last >= 7,1,0)) %>%
  filter(is_new_event == 1) %>%
  summarise(event_count = n(),
            .groups="drop")
Abdominal_pain <- data_out %>%
  group_by(gender) %>%
  summarise(across(event_count,.fns = list(
    min=min,
    q25=~quantile(.,0.25),
    median=median,
    mean=mean,
    q75=~quantile(.,0.75),
    max=max
  )))

all <- rbind(dyspepsia,Dysphagia,Abdominal_pain,Vomiting)

write_xlsx(all,'.\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\event_count_men_women_comp.xlsx')

cohort_main_symp <- cohort_union_all %>%
  filter(Dysphagia == 1 | Dyspepsia == 1 |  Vomiting == 1 | Abdominal_pain == 1) %>%  #Abdominal_mass == 1 |  | Reflux == 1 
  distinct()

dbWriteTable(db, "cohortv1_2",cohort_main_symp,row.names= FALSE, overwrite = TRUE)

#Creating the co-occurring symptom cohorts
cohort_weight_loss <- cohort_union_all %>%
  filter(Weight_loss == 1) %>%
  select(e_patid, eventdate)

cohort_cough <- cohort_union_all %>%
  filter(Cough == 1) %>%
  select(e_patid, eventdate)

cohort_appetite_loss <- cohort_union_all %>%
  filter(Appetite_loss == 1) %>%
  select(e_patid, eventdate)

cohort_fatigue <- cohort_union_all %>%
  filter(Fatigue == 1) %>%
  select(e_patid, eventdate)

#Moving to SQL
dbWriteTable(db, "cohort_weight_loss",cohort_weight_loss,row.names= FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_cough",cohort_cough,row.names= FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_appetite_loss",cohort_appetite_loss,row.names= FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_fatigue",cohort_fatigue,row.names= FALSE, overwrite = TRUE)

dbSendQuery(
  conn = db,
  statement = "
drop table if exists  freya.cohortv1_3;")

dbSendQuery(
  conn = db,
  statement = "
create table freya.cohortv1_3
(
  select distinct l.e_patid,l.eventdate,l.medcode,l.readcode_desc,l.symptom_desc,l.age_band,l.age,l.gender,l.Dyspepsia,l.Dysphagia,l.Vomiting,l.Abdominal_pain, l.low_hb, l.raise_platelets, l.raise_wbc, l.h_pylori_flag,d.eventdate as weight_loss
  from freya.cohortv1_2 l
  left join freya.cohort_weight_loss d on d.e_patid = l.e_patid 
  and d.eventdate <= l.eventdate
  and d.eventdate >= date_sub(l.eventdate, interval 6 month)
)
;")

dbSendQuery(
  conn = db,
  statement = "
drop table if exists freya.cohortv1_4;")

dbSendQuery(
  conn = db,
  statement = "
create table freya.cohortv1_4
(
  select distinct l.e_patid,l.eventdate,l.medcode,l.readcode_desc,l.symptom_desc,l.age_band,l.age,l.gender,l.Dyspepsia,l.Dysphagia,l.Vomiting,l.Abdominal_pain, l.low_hb,l.raise_platelets, l.raise_wbc,l.h_pylori_flag,l.weight_loss, d.eventdate as cough
  from freya.cohortv1_3 l
  left join freya.cohort_cough d on d.e_patid = l.e_patid 
  and d.eventdate <= l.eventdate 
  and d.eventdate >= date_sub(l.eventdate, interval 6 month)
)
;")

dbSendQuery(
  conn = db,
  statement = "
drop table if exists  freya.cohortv1_5;")

dbSendQuery(
  conn = db,
  statement = "
create table freya.cohortv1_5
(
  select distinct l.e_patid,l.eventdate,l.medcode,l.readcode_desc,l.symptom_desc,l.age_band,l.age,l.gender,l.Dyspepsia,l.Dysphagia,l.Vomiting,l.Abdominal_pain, l.low_hb,l.raise_platelets, l.raise_wbc,l.h_pylori_flag,l.weight_loss, l.cough,d.eventdate as appetite_loss
  from freya.cohortv1_4 l
  left join freya.cohort_appetite_loss d on d.e_patid = l.e_patid 
  and d.eventdate <= l.eventdate
  and d.eventdate >= date_sub(l.eventdate, interval 6 month)
)
;")

dbSendQuery(
  conn = db,
  statement = "
drop table if exists  freya.cohortv1_6;")

dbSendQuery(
  conn = db,
  statement = "
create table freya.cohortv1_6
(
  select distinct l.e_patid,l.eventdate,l.medcode,l.readcode_desc,l.symptom_desc,l.age_band,l.age,l.gender,l.Dyspepsia,l.Dysphagia,l.Vomiting,l.Abdominal_pain, l.low_hb,l.raise_platelets, l.raise_wbc,l.h_pylori_flag,l.weight_loss,l.cough,l.appetite_loss, d.eventdate as fatigue
  from freya.cohortv1_5 l
  left join freya.cohort_fatigue d on d.e_patid = l.e_patid 
  and d.eventdate <= l.eventdate
  and d.eventdate >= date_sub(l.eventdate, interval 6 month)
)
;")


dbSendQuery(
  conn = db,
  statement = "
drop table if exists  freya.cohortv1_7;")

dbSendQuery(
  conn = db,
  statement = "
create table freya.cohortv1_7
(
  select distinct l.e_patid,l.eventdate,l.medcode,l.readcode_desc,l.symptom_desc,l.age_band,l.age,l.gender,l.Dyspepsia,l.Dysphagia,l.Vomiting,l.Abdominal_pain, l.low_hb, l.raise_platelets, l.raise_wbc,l.h_pylori_flag,l.weight_loss,l.cough,l.appetite_loss, l.fatigue, d.eventdate as ever_obesity
  from freya.cohortv1_6 l
  left join freya.obesity d on d.e_patid = l.e_patid 
  and d.eventdate <= l.eventdate
  and d.eventdate >= date_sub(l.eventdate, interval 240 month)
)
;")


dbSendQuery(
  conn = db,
  statement = "
drop table if exists  freya.cohortv1_8;")

dbSendQuery(
  conn = db,
  statement = "
create table freya.cohortv1_8
(
  select distinct l.e_patid,l.eventdate,l.medcode,l.readcode_desc,l.symptom_desc,l.age_band,l.age,l.gender,l.Dyspepsia,l.Dysphagia,l.Vomiting,l.Abdominal_pain, l.low_hb, l.raise_platelets, l.raise_wbc,l.h_pylori_flag,l.weight_loss,l.cough,l.appetite_loss, l.fatigue, l.ever_obesity, d.eventdate as ever_alcohol_problems
  from freya.cohortv1_7 l
  left join freya.alc_problems d on d.e_patid = l.e_patid 
  and d.eventdate <= l.eventdate
  and d.eventdate >= date_sub(l.eventdate, interval 240 month)
)
;")

#Go to R script 1.3.1