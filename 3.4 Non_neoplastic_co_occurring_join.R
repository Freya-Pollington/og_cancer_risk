rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same 
pacman::p_load(tidyverse, tidyr, 
               data.table, RMySQL, DBI,janitor)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

##----------------------------------------------------------
#### Joining in the other non-neoplastic diseases ####

#taking the records for each of the main disease groups before 
#previous symptom and cancer filtering took place
query <- 'SELECT * FROM freya.disease_index_all_pot_elig_tests_1;'
rs <- dbSendQuery(db, query)
non_neo <- fetch(rs, n=-1)

non_neo <- non_neo %>%
  mutate(eventdate = as.Date(eventdate))

non_neo_gord <- non_neo %>%
  filter(disease_name == "gord")%>%
  select(e_patid, eventdate) %>%
  rename(gord=eventdate)

non_neo_gastritis_duodenitis <- non_neo %>%
  filter(disease_name == "gastritis_duodenitis")%>%
  select(e_patid, eventdate)%>%
  rename(gastritis_duodenitis=eventdate)

non_neo_barretts <- non_neo %>%
  filter(disease_name == "barretts")%>%
  select(e_patid, eventdate)%>%
  rename(barretts=eventdate)

non_neo_hernia_abdo <- non_neo %>%
  filter(disease_name == "hernia_abdo")%>%
  select(e_patid, eventdate)%>%
  rename(hernia_abdo=eventdate)

non_neo_oesoph_ulc <- non_neo %>%
  filter(disease_name == "oesoph_ulc")%>%
  select(e_patid, eventdate)%>%
  rename(oesoph_ulc=eventdate)

#---
#loading the main disease dataframes

#GORD, adding in reflux indexes
query <- 'SELECT * FROM freya.disease_index_gord_elig;'
rs <- dbSendQuery(db, query)
main_gord <- fetch(rs, n=-1)

###checks###
query <- 'SELECT * FROM freya.disease_index_all_pot_elig_flag;'
rs <- dbSendQuery(db, query)
initial_gord <- fetch(rs, n=-1)
initial_gord = initial_gord %>% filter(disease_name=='gord')

#find patients in the reflux cohort who are not in the gord cohort
anti_join <- anti_join(reflux,initial_gord,by='e_patid')
initial_gord_combined <- union(initial_gord%>%select(e_patid),anti_join%>%select(e_patid))
#get number of patients wihtout prev cancer check
can_check <- union(gord,anti_join)
####

#Sort chronologically by patient
data_sort <- reflux[
  with(reflux, order(e_patid, eventdate)),
]

data_sort <- data_sort %>%
  group_by(e_patid) %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(eventdate_lag1 = lag(eventdate)) %>%
  fill(eventdate_lag1) %>%
  ungroup()

# Date 2 year before upper GI symptom
data_sort <- data_sort %>%
  #mutate(eventdate = as.Date(eventdate)) %>%
  mutate(eventdate_pre_2yr = data_sort$eventdate %m-% years(2))

# Flagging eligible events if potentially eligible, no previous disease cases in the last 2 years
data_sort <- data_sort %>%
  mutate(data_sort, elig = ifelse (
    (eventdate_lag1 < eventdate_pre_2yr | is.na(eventdate_lag1))
    ,
    1, 0)) %>%
  filter(elig == 1)

#find patients in the reflux cohort who are not in the gord cohort
anti_join <- anti_join(data_sort,main_gord,by='e_patid')

#processing the dataframe to enable union with the main gord dataframe, including creating the og_cancer variable, 
#and choosing a random index for those with more than one eligible index date
anti_join <- anti_join %>%
  mutate(og_cancer = ifelse(s_or_o_cancer == 1 & diagnosisdatebest2 >= eventdate & diagnosisdatebest2 <= eventdate %m+% months(12),1,0)) %>%
  select(-c(cancer_group_desc_v2,cancer_site_desc_v2,eventdate_lag1,eventdate_pre_2yr,medcode,elig,symptom_desc)) %>%
  rename(code_desc = readcode_desc) %>%
  mutate(disease_name = "gord") %>%
  mutate(yob_approx = NA) %>%
  group_by(e_patid) %>%
  sample_n(1) %>%
  mutate(data_source = "cprd")

main_gord <- main_gord %>% mutate(eventdate = as.Date(eventdate))
gord_combined <- union(main_gord,anti_join)

#oesophageal ulcer
query <- 'SELECT * FROM freya.disease_index_oesoph_ulc_elig;'
rs <- dbSendQuery(db, query)
main_oesoph_ulc <- fetch(rs, n=-1)

#diaphragmatic hernia
query <- 'SELECT * FROM freya.disease_index_hernia_abdo_elig;'
rs <- dbSendQuery(db, query)
main_hernia_abdo <- fetch(rs, n=-1)

#barretts
query <- 'SELECT * FROM freya.disease_index_barretts_elig;'
rs <- dbSendQuery(db, query)
main_barretts <- fetch(rs, n=-1)

#gastritis/duodenitis
query <- 'SELECT * FROM freya.disease_index_gastritis_duodenitis_elig;'
rs <- dbSendQuery(db, query)
main_gastritis_duodenitis <- fetch(rs, n=-1)

#---
#Function to join in each other non-neoplastic disease to each main disease dataframe

disease_join <- function(df_main, df_disease, disease_name) {
  df_main %>%
    mutate(eventdate = as.Date(eventdate)) %>%
    left_join(df_disease, by = "e_patid") %>%
    mutate(
      
      !!paste("td_",as.character(disease_name)) := ifelse(as.numeric(eventdate - !!sym(as.character(disease_name)))>=0 & 
                                                            as.numeric(eventdate - !!sym(as.character(disease_name))) <= 183,
                                                          as.numeric(eventdate - !!sym(as.character(disease_name))),NA),
      # This allows for dynamic naming of column names, where the symptom from the passed df name
      # -- is extracted as a symbol and converted to a string and assigned to the column name.
      # -- a flag is added if it meets the conditions if the additional symptom occurs withiin 6 months (otherwise NA == 0).
      "{disease_name}" := ifelse(eventdate >= !!sym(as.character(disease_name)) &
                                   eventdate %m-% months(6) <= !!sym(as.character(disease_name)), 1, 0),
      "{disease_name}" := replace_na(!!sym(disease_name), 0)
    ) %>%
    select(-matches("eventdate.y")) %>%
    clean_names()
}

#---
# Apply function to each disease

#Gord

# Add each of the disease patient/date data frames to a list, omitting the current main dataframe
diseases_vec <- list(barretts = non_neo_barretts, oesoph_ulc = non_neo_oesoph_ulc,
                     gastritis_duodenitis = non_neo_gastritis_duodenitis,
                     hernia_abdo = non_neo_hernia_abdo)

#run the function on each co-occurring disease
main_gord_1 <- gord_combined
for(disease_name in names(diseases_vec)) {
  main_gord_1 <- disease_join(main_gord_1, diseases_vec[[disease_name]], disease_name) %>%
    distinct()
}

#oesoph_ulc
diseases_vec <- list(barretts = non_neo_barretts, gord = non_neo_gord,
                     gastritis_duodenitis = non_neo_gastritis_duodenitis,
                     hernia_abdo = non_neo_hernia_abdo)

main_oesoph_ulc_1 <- main_oesoph_ulc
for(disease_name in names(diseases_vec)) {
  main_oesoph_ulc_1 <- disease_join(main_oesoph_ulc_1, diseases_vec[[disease_name]], disease_name) %>%
    distinct()
}

#barretts
diseases_vec <- list(oesoph_ulc = non_neo_oesoph_ulc, gord = non_neo_gord,
                     gastritis_duodenitis = non_neo_gastritis_duodenitis,
                     hernia_abdo = non_neo_hernia_abdo)

main_barretts_1 <- main_barretts
for(disease_name in names(diseases_vec)) {
  main_barretts_1 <- disease_join(main_barretts_1, diseases_vec[[disease_name]], disease_name) %>%
    distinct()
}

#hernia_abdo
diseases_vec <- list(barretts = non_neo_barretts, gord = non_neo_gord,
                     gastritis_duodenitis = non_neo_gastritis_duodenitis,
                     oesoph_ulc = non_neo_oesoph_ulc)

main_hernia_abdo_1 <- main_hernia_abdo
for(disease_name in names(diseases_vec)) {
  main_hernia_abdo_1 <- disease_join(main_hernia_abdo_1, diseases_vec[[disease_name]], disease_name) %>%
    distinct()
}

#gastritis_duodenitis
diseases_vec <- list(barretts = non_neo_barretts, gord = non_neo_gord,
                     hernia_abdo = non_neo_hernia_abdo,
                     oesoph_ulc = non_neo_oesoph_ulc)

main_gastritis_duodenitis_1 <- main_gastritis_duodenitis
for(disease_name in names(diseases_vec)) {
  main_gastritis_duodenitis_1 <- disease_join(main_gastritis_duodenitis_1, diseases_vec[[disease_name]], disease_name) %>%
    distinct()
}

#---------------------------------------
## patients could have both a valid previous disease and invalid for the same index date
## to combat this, ensuring only the positive value (1) is kept in instances where there are both 1s and 0s

## Function to keep only the positive test result or symptom recording
keep_pos <- function(data,variable,new_variable){
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
  return(out)
}

#---
#for each co-occurring disease, running the function on each dataframe

disease_vec <- c('barretts','oesoph_ulc','gord','gastritis_duodenitis'#,
                 )  

main_hernia_abdo_1_2 <- main_hernia_abdo_1
for(symp_name in (disease_vec)) {
  main_hernia_abdo_1_2  <- keep_pos(main_hernia_abdo_1_2 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

disease_vec <- c('barretts','oesoph_ulc','gord','hernia_abdo'#,
                ) 

main_gastritis_duodenitis_1_2 <- main_gastritis_duodenitis_1
for(symp_name in (disease_vec)) {
  main_gastritis_duodenitis_1_2  <- keep_pos(main_gastritis_duodenitis_1_2 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

disease_vec <- c('barretts','oesoph_ulc','hernia_abdo','gastritis_duodenitis'#,
                 ) 

main_gord_1_2 <- main_gord_1
for(symp_name in (disease_vec)) {
  main_gord_1_2  <- keep_pos(main_gord_1_2 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

disease_vec <- c('gord','oesoph_ulc','hernia_abdo','gastritis_duodenitis'#,
                 )

main_barretts_1_2 <- main_barretts_1
for(symp_name in (disease_vec)) {
  main_barretts_1_2  <- keep_pos(main_barretts_1_2 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

disease_vec <- c('gord','barretts','hernia_abdo','gastritis_duodenitis'#,
                 ) 

main_oesoph_ulc_1_2 <- main_oesoph_ulc_1
for(symp_name in (disease_vec)) {
  main_oesoph_ulc_1_2  <- keep_pos(main_oesoph_ulc_1_2 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

#-- Now sorting the td variables having more than one
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

disease_vec <- c('td_barretts','td_oesoph_ulc','td_gord','td_gastritis_duodenitis')  

main_hernia_abdo_1_22 <- main_hernia_abdo_1_2
for(symp_name in (disease_vec)) {
  main_hernia_abdo_1_22  <- keep_pos2(main_hernia_abdo_1_22 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

disease_vec <- c('td_barretts','td_oesoph_ulc','td_hernia_abdo','td_gastritis_duodenitis')  

main_gord_1_22 <- main_gord_1_2
for(symp_name in (disease_vec)) {
  main_gord_1_22  <- keep_pos2(main_gord_1_22 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

disease_vec <- c('td_gord','td_oesoph_ulc','td_hernia_abdo','td_gastritis_duodenitis')  

main_barretts_1_22 <- main_barretts_1_2
for(symp_name in (disease_vec)) {
  main_barretts_1_22  <- keep_pos2(main_barretts_1_22 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

disease_vec <- c('td_gord','td_oesoph_ulc','td_hernia_abdo','td_barretts')  

main_gastritis_duodenitis_1_22 <- main_gastritis_duodenitis_1_2
for(symp_name in (disease_vec)) {
  main_gastritis_duodenitis_1_22  <- keep_pos2(main_gastritis_duodenitis_1_22 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

disease_vec <- c('td_gord','td_barretts','td_hernia_abdo','td_gastritis_duodenitis')  

main_oesoph_ulc_1_22 <- main_oesoph_ulc_1_2
for(symp_name in (disease_vec)) {
  main_oesoph_ulc_1_22  <- keep_pos2(main_oesoph_ulc_1_22 , quo(symp_name), as.name(symp_name)) %>%
    distinct()
}


##----------------------------------------------------------
#### Joining in symptom/lifestyle information ####
#---------------
#Loding sql dataframes
query <- 'SELECT * FROM freya.cohort_weight_loss;'
rs <- dbSendQuery(db, query)
weight_loss <- fetch(rs, n=-1)

names(weight_loss)[2] <- "weight_loss"

weight_loss$weight_loss <- as.Date(weight_loss$weight_loss)

query <- 'SELECT * FROM freya.cohort_cough;'
rs <- dbSendQuery(db, query)
cough <- fetch(rs, n=-1)

names(cough)[2] <- "cough"

cough$cough <- as.Date(cough$cough)

query <- 'SELECT * FROM freya.cohort_appetite_loss;'
rs <- dbSendQuery(db, query)
appetite_loss <- fetch(rs, n=-1)

names(appetite_loss)[2] <- "appetite_loss"

appetite_loss$appetite_loss <- as.Date(appetite_loss$appetite_loss)

query <- 'SELECT * FROM freya.cohort_fatigue;'
rs <- dbSendQuery(db, query)
fatigue <- fetch(rs, n=-1)

names(fatigue)[2] <- "fatigue"

fatigue$fatigue <- as.Date(fatigue$fatigue)

query <- 'SELECT * FROM freya.alc_problems;'
rs <- dbSendQuery(db, query)
alc_problems <- fetch(rs, n=-1)

names(alc_problems)[2] <- "alc_problems"
alc_problems <- alc_problems[,1:2]

alc_problems$alc_problems <- as.Date(alc_problems$alc_problems)

query <- 'SELECT * FROM freya.obesity;'
rs <- dbSendQuery(db, query)
obesity <- fetch(rs, n=-1)

names(obesity)[2] <- "obesity"
obesity <- obesity[,1:2]

obesity$obesity <- as.Date(obesity$obesity)

## To get tables for the main symptoms need to extract from this table of all the main symptoms after filtering for potential eligibilty 
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

#-----------------------
## Joining in the symptoms to each cohort dataframe using the old function## 

symp_list <- list(dyspepsia=cohort_dyspepsia,dysphagia=cohort_dysphagia,abd_pain=cohort_abd_pain,vomiting=cohort_vomiting)

main_gastritis_duodenitis_2 <- main_gastritis_duodenitis_1_22
for(symp_name in names(symp_list)) {
  main_gastritis_duodenitis_2 <- disease_join(main_gastritis_duodenitis_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}

main_barretts_2 <- main_barretts_1_22
for(symp_name in names(symp_list)) {
  main_barretts_2 <- disease_join(main_barretts_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}

main_gord_2 <- main_gord_1_22
for(symp_name in names(symp_list)) {
  main_gord_2 <- disease_join(main_gord_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}

main_oesoph_ulc_2 <- main_oesoph_ulc_1_22
for(symp_name in names(symp_list)) {
  main_oesoph_ulc_2 <- disease_join(main_oesoph_ulc_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}

main_hernia_abdo_2 <- main_hernia_abdo_1_22
for(symp_name in names(symp_list)) {
  main_hernia_abdo_2 <- disease_join(main_hernia_abdo_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}
#-----------
vague_join <- function(df_main, df_disease, disease_name) {
  df_main %>%
    mutate(eventdate = as.Date(eventdate)) %>%
    left_join(df_disease, by = "e_patid") %>%
    mutate(
      "{disease_name}" := ifelse(eventdate >= !!sym(as.character(disease_name)) &
                                   eventdate %m-% months(6) <= !!sym(as.character(disease_name)), 1, 0),
      "{disease_name}" := replace_na(!!sym(disease_name), 0)
    ) %>%
    select(-matches("eventdate.y")) %>%
    clean_names()
}

symp_list <- list(weight_loss=weight_loss,cough=cough,fatigue=fatigue,appetite_loss=appetite_loss
  )

main_gastritis_duodenitis_2_2 <- main_gastritis_duodenitis_2
for(symp_name in names(symp_list)) {
  main_gastritis_duodenitis_2_2 <- vague_join(main_gastritis_duodenitis_2_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}

main_barretts_2_2 <- main_barretts_2
for(symp_name in names(symp_list)) {
  main_barretts_2_2 <- vague_join(main_barretts_2_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}

main_gord_2_2 <- main_gord_2
for(symp_name in names(symp_list)) {
  main_gord_2_2 <- vague_join(main_gord_2_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}

main_oesoph_ulc_2_2 <- main_oesoph_ulc_2
for(symp_name in names(symp_list)) {
  main_oesoph_ulc_2_2 <- vague_join(main_oesoph_ulc_2_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}

main_hernia_abdo_2_2 <- main_hernia_abdo_2
for(symp_name in names(symp_list)) {
  main_hernia_abdo_2_2 <- vague_join(main_hernia_abdo_2_2, symp_list[[symp_name]], symp_name) %>%
    distinct()
}

#--------------------------------
#Running the function to only keep 1s for patients with more than one eligible symptom recording

symp_vec <- c('weight_loss','appetite_loss','cough','fatigue',
                    'dyspepsia','dysphagia','abd_pain','vomiting')  

main_hernia_abdo_3 <- main_hernia_abdo_2_2
for(symp_name in (symp_vec)) {
  main_hernia_abdo_3 <- keep_pos(main_hernia_abdo_3, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_barretts_3 <- main_barretts_2_2
for(symp_name in (symp_vec)) {
  main_barretts_3 <- keep_pos(main_barretts_3, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_oesoph_ulc_3 <- main_oesoph_ulc_2_2
for(symp_name in (symp_vec)) {
  main_oesoph_ulc_3 <- keep_pos(main_oesoph_ulc_3, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_gord_3 <- main_gord_2_2
for(symp_name in (symp_vec)) {
  main_gord_3 <- keep_pos(main_gord_3, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_gastritis_duodenitis_3 <- main_gastritis_duodenitis_2_2
for(symp_name in (symp_vec)) {
  main_gastritis_duodenitis_3 <- keep_pos(main_gastritis_duodenitis_3, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

#running the keep_pos2 function to ensure only one value of the td variables for each patient
symp_vec <- c("td_dyspepsia","td_dysphagia","td_abd_pain","td_vomiting")  

main_hernia_abdo_32 <- main_hernia_abdo_3
for(symp_name in (symp_vec)) {
  main_hernia_abdo_32 <- keep_pos2(main_hernia_abdo_32, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_barretts_32 <- main_barretts_3
for(symp_name in (symp_vec)) {
  main_barretts_32 <- keep_pos2(main_barretts_32, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_oesoph_ulc_32 <- main_oesoph_ulc_3
for(symp_name in (symp_vec)) {
  main_oesoph_ulc_32 <- keep_pos2(main_oesoph_ulc_32, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_gord_32 <- main_gord_2_2
for(symp_name in (symp_vec)) {
  main_gord_32 <- keep_pos2(main_gord_32, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

main_gastritis_duodenitis_32 <- main_gastritis_duodenitis_3
for(symp_name in (symp_vec)) {
  main_gastritis_duodenitis_32 <- keep_pos2(main_gastritis_duodenitis_32, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

#----------------------------------------------------------------------
#cretaing a same day diagnosis column for all the main features
main_oesoph_ulc_32 <- main_oesoph_ulc_32 %>% 
  mutate(same_day_gord = ifelse(td_gord !=0 | is.na(td_gord),0,1))%>% 
  mutate(same_day_barretts = ifelse(td_barretts !=0 | is.na(td_barretts),0,1))%>% 
  mutate(same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis !=0 | is.na(td_gastritis_duodenitis),0,1))%>% 
  mutate(same_day_dyspepsia = ifelse(td_dyspepsia !=0 | is.na(td_dyspepsia),0,1))%>% 
  mutate(same_day_dysphagia = ifelse(td_dysphagia !=0 | is.na(td_dysphagia),0,1))%>% 
  mutate(same_day_abd_pain = ifelse(td_abd_pain !=0 | is.na(td_abd_pain),0,1))%>% 
  mutate(same_day_vomiting = ifelse(td_vomiting !=0 | is.na(td_vomiting),0,1))%>% 
  mutate(same_day_hernia_abdo = ifelse(td_hernia_abdo !=0 | is.na(td_hernia_abdo),0,1))#%>% 

main_barretts_32 <- main_barretts_32 %>% 
  mutate(same_day_gord = ifelse(td_gord !=0 | is.na(td_gord),0,1))%>% 
  mutate(same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis !=0 | is.na(td_gastritis_duodenitis),0,1))%>% 
  mutate(same_day_dyspepsia = ifelse(td_dyspepsia !=0 | is.na(td_dyspepsia),0,1))%>% 
  mutate(same_day_dysphagia = ifelse(td_dysphagia !=0 | is.na(td_dysphagia),0,1))%>% 
  mutate(same_day_abd_pain = ifelse(td_abd_pain !=0 | is.na(td_abd_pain),0,1))%>% 
  mutate(same_day_vomiting = ifelse(td_vomiting !=0 | is.na(td_vomiting),0,1))%>% 
  mutate(same_day_hernia_abdo = ifelse(td_hernia_abdo !=0 | is.na(td_hernia_abdo),0,1))%>% 
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc !=0 | is.na(td_oesoph_ulc),0,1))

main_gord_32 <- main_gord_32 %>% 
  mutate(same_day_barretts = ifelse(td_barretts !=0 | is.na(td_barretts),0,1))%>% 
  mutate(same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis !=0 | is.na(td_gastritis_duodenitis),0,1))%>% 
  mutate(same_day_dyspepsia = ifelse(td_dyspepsia !=0 | is.na(td_dyspepsia),0,1))%>% 
  mutate(same_day_dysphagia = ifelse(td_dysphagia !=0 | is.na(td_dysphagia),0,1))%>% 
  mutate(same_day_abd_pain = ifelse(td_abd_pain !=0 | is.na(td_abd_pain),0,1))%>% 
  mutate(same_day_vomiting = ifelse(td_vomiting !=0 | is.na(td_vomiting),0,1))%>% 
  mutate(same_day_hernia_abdo = ifelse(td_hernia_abdo !=0 | is.na(td_hernia_abdo),0,1))%>% 
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc !=0 | is.na(td_oesoph_ulc),0,1))

main_hernia_abdo_32 <- main_hernia_abdo_32 %>% 
  mutate(same_day_gord = ifelse(td_gord !=0 | is.na(td_gord),0,1))%>% 
  mutate(same_day_barretts = ifelse(td_barretts !=0 | is.na(td_barretts),0,1))%>% 
  mutate(same_day_gastritis_duodenitis = ifelse(td_gastritis_duodenitis !=0 | is.na(td_gastritis_duodenitis),0,1))%>% 
  mutate(same_day_dyspepsia = ifelse(td_dyspepsia !=0 | is.na(td_dyspepsia),0,1))%>% 
  mutate(same_day_dysphagia = ifelse(td_dysphagia !=0 | is.na(td_dysphagia),0,1))%>% 
  mutate(same_day_abd_pain = ifelse(td_abd_pain !=0 | is.na(td_abd_pain),0,1))%>% 
  mutate(same_day_vomiting = ifelse(td_vomiting !=0 | is.na(td_vomiting),0,1))%>% 
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc !=0 | is.na(td_oesoph_ulc),0,1))

main_gastritis_duodenitis_32 <- main_gastritis_duodenitis_32 %>% 
  mutate(same_day_gord = ifelse(td_gord !=0 | is.na(td_gord),0,1))%>% 
  mutate(same_day_barretts = ifelse(td_barretts !=0 | is.na(td_barretts),0,1))%>% 
  mutate(same_day_dyspepsia = ifelse(td_dyspepsia !=0 | is.na(td_dyspepsia),0,1))%>% 
  mutate(same_day_dysphagia = ifelse(td_dysphagia !=0 | is.na(td_dysphagia),0,1))%>% 
  mutate(same_day_abd_pain = ifelse(td_abd_pain !=0 | is.na(td_abd_pain),0,1))%>% 
  mutate(same_day_vomiting = ifelse(td_vomiting !=0 | is.na(td_vomiting),0,1))%>% 
  mutate(same_day_hernia_abdo = ifelse(td_hernia_abdo !=0 | is.na(td_hernia_abdo),0,1))%>% 
  mutate(same_day_oesoph_ulc = ifelse(td_oesoph_ulc !=0 | is.na(td_oesoph_ulc),0,1))
#--------------------------------------------------
#Joining in alcohol and obesity problems if 'ever', using a 240 month lookback
#removing the TD variables in this step as the values were extracted above, and only the same day flag is required from now on
lifestyle_join <- function(df_main, df_lf, lf_name) {
  df_main %>%
    mutate(eventdate = as.Date(eventdate)) %>%
    left_join(df_lf, by = "e_patid") %>%
    mutate(
      # This allows for dynamic naming of column names, where the symptom from the passed df name
      # -- is extracted as a symbol and converted to a string and assigned to the column name.
      # -- a flag is added if it meets the conditions if the additional symptom occurs withiin 6 months (otherwise NA == 0).
      "{lf_name}" := ifelse(eventdate >= !!sym(as.character(lf_name)) &
                              eventdate %m-% months(240) <= !!sym(as.character(lf_name)), 1, 0),
      "{lf_name}" := replace_na(!!sym(lf_name), 0)
    ) %>%
    select(-matches("eventdate.y"))
  return(df_main)
}
#--------------------------------------------------
lifestyle_list <- list(obesity=obesity,alc_problems=alc_problems)

main_gastritis_duodenitis_4 <- main_gastritis_duodenitis_32
for(lf_name in names(lifestyle_list)) {
  main_gastritis_duodenitis_4 <- lifestyle_join(main_gastritis_duodenitis_4, lifestyle_list[[lf_name]], lf_name) %>%
    distinct()
}

main_barretts_4 <- main_barretts_32
for(lf_name in names(lifestyle_list)) {
  main_barretts_4 <- lifestyle_join(main_barretts_4, lifestyle_list[[lf_name]], lf_name) %>%
    distinct()
}

main_gord_4 <- main_gord_32
for(lf_name in names(lifestyle_list)) {
  main_gord_4 <- lifestyle_join(main_gord_4, lifestyle_list[[lf_name]], lf_name) %>%
    distinct()
}

main_hernia_abdo_4 <- main_hernia_abdo_32
for(lf_name in names(lifestyle_list)) {
  main_hernia_abdo_4 <- lifestyle_join(main_hernia_abdo_4, lifestyle_list[[lf_name]], lf_name) %>%
    distinct()
}

main_oesoph_ulc_4 <- main_oesoph_ulc_32
for(lf_name in names(lifestyle_list)) {
  main_oesoph_ulc_4 <- lifestyle_join(main_oesoph_ulc_4, lifestyle_list[[lf_name]], lf_name) %>%
    distinct()
}

#--------------------------------
#Running the function to only keep 1s for patients with more than one eligible vague symptom recording

symp_vec <- c('obesity','alc_problems')  

for(symp_name in (symp_vec)) {
  main_hernia_abdo_4 <- keep_pos(main_hernia_abdo_4, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

for(symp_name in (symp_vec)) {
  main_barretts_4 <- keep_pos(main_barretts_4, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

for(symp_name in (symp_vec)) {
  main_gastritis_duodenitis_4 <- keep_pos(main_gastritis_duodenitis_4, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

for(symp_name in (symp_vec)) {
  main_oesoph_ulc_4 <- keep_pos(main_oesoph_ulc_4, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

for(symp_name in (symp_vec)) {
  main_gord_4 <- keep_pos(main_gord_4, quo(symp_name), as.name(symp_name)) %>%
    distinct()
}

#--------------------------------------
# count of non-neoplastic outcomes
main_gastritis_duodenitis_4 <- main_gastritis_duodenitis_4 %>%
  mutate(count_non_neo=rowSums(across(c(oesoph_ulc,barretts,gord,hernia_abdo)))) %>%
  mutate(count_symptoms=rowSums(across(c(dyspepsia,dysphagia,fatigue,low_hb,weight_loss, appetite_loss,vomiting,abd_pain,cough))))

main_gord_4 <- main_gord_4 %>%
  mutate(count_non_neo=rowSums(across(c(oesoph_ulc,barretts,gastritis_duodenitis,hernia_abdo)))) %>%
  mutate(count_symptoms=rowSums(across(c(dyspepsia,dysphagia,fatigue,low_hb,weight_loss, appetite_loss,vomiting,abd_pain,cough))))

main_oesoph_ulc_4 <- main_oesoph_ulc_4 %>%
  mutate(count_non_neo=rowSums(across(c(gord,barretts,gastritis_duodenitis,hernia_abdo)))) %>%
  mutate(count_symptoms=rowSums(across(c(dyspepsia,dysphagia,fatigue,low_hb,weight_loss, appetite_loss,vomiting,abd_pain,cough))))

main_barretts_4 <- main_barretts_4 %>%
  mutate(count_non_neo=rowSums(across(c(gord,oesoph_ulc,gastritis_duodenitis,hernia_abdo)))) %>%
  mutate(count_symptoms=rowSums(across(c(dyspepsia,dysphagia,fatigue,low_hb,weight_loss, appetite_loss,vomiting,abd_pain,cough))))

main_hernia_abdo_4 <- main_hernia_abdo_4 %>%
  mutate(count_non_neo=rowSums(across(c(gord,oesoph_ulc,gastritis_duodenitis,barretts)))) %>%
  mutate(count_symptoms=rowSums(across(c(dyspepsia,dysphagia,fatigue,low_hb,weight_loss, appetite_loss,vomiting,abd_pain,cough))))

#-------------------------------------------------------------------------------------------
hernia_abdo <- main_barretts_4 %>% filter(hernia_abdo == 1)
hernia_abdo <- table(hernia_abdo$same_day_hernia_abdo)

gastritis_duodenitis <- main_barretts_4 %>% filter(gastritis_duodenitis == 1)
gastritis_duodenitis <- table(gastritis_duodenitis$same_day_gastritis_duodenitis)

oesoph_ulc <- main_barretts_4 %>% filter(oesoph_ulc == 1)
oesoph_ulc <- table(oesoph_ulc$same_day_oesoph_ulc)

gord <- main_barretts_4 %>% filter(gord == 1)
gord <- table(gord$same_day_gord)

dyspepsia <- main_barretts_4 %>% filter(dyspepsia == 1)
dyspepsia <- table(dyspepsia$same_day_dyspepsia)

dysphagia <- main_barretts_4 %>% filter(dysphagia == 1)
dysphagia <- table(dysphagia$same_day_dysphagia)

abd_pain <- main_barretts_4 %>% filter(abd_pain == 1)
abd_pain <- table(abd_pain$same_day_abd_pain)

vomiting <- main_barretts_4 %>% filter(vomiting == 1)
vomiting <- table(vomiting$same_day_vomiting)

same_day_barretts <- rbind(hernia_abdo,dyspepsia,dysphagia,gastritis_duodenitis,gord,oesoph_ulc,abd_pain,vomiting)

same_day_barretts <- same_day_barretts %>%
  as.data.frame() %>%
  mutate(prop = round(`1`/(`1`+`0`),3))

xlsx::write.xlsx(same_day_barretts,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "Barretts",append =TRUE)

#-------------------
hernia_abdo <- main_oesoph_ulc_4 %>% filter(hernia_abdo == 1)
hernia_abdo <- table(hernia_abdo$same_day_hernia_abdo)

gastritis_duodenitis <- main_oesoph_ulc_4 %>% filter(gastritis_duodenitis == 1)
gastritis_duodenitis <- table(gastritis_duodenitis$same_day_gastritis_duodenitis)

gord <- main_oesoph_ulc_4 %>% filter(gord == 1)
gord <- table(gord$same_day_gord)

dyspepsia <- main_oesoph_ulc_4 %>% filter(dyspepsia == 1)
dyspepsia <- table(dyspepsia$same_day_dyspepsia)

dysphagia <- main_oesoph_ulc_4 %>% filter(dysphagia == 1)
dysphagia <- table(dysphagia$same_day_dysphagia)

abd_pain <- main_oesoph_ulc_4 %>% filter(abd_pain == 1)
abd_pain <- table(abd_pain$same_day_abd_pain)

vomiting <- main_oesoph_ulc_4 %>% filter(vomiting == 1)
vomiting <- table(vomiting$same_day_vomiting)

barretts <- main_oesoph_ulc_4 %>% filter(barretts == 1)
barretts <- table(barretts$same_day_barretts)

same_day_oesoph_ulc <- rbind(hernia_abdo,dyspepsia,dysphagia,gastritis_duodenitis,gord,barretts,abd_pain,vomiting)

same_day_oesoph_ulc <- same_day_oesoph_ulc %>%
  as.data.frame() %>%
  mutate(prop = round(`1`/(`1`+`0`),3))

xlsx::write.xlsx(same_day_oesoph_ulc,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "oesoph_ulc",append =TRUE)

#-------------------------------
hernia_abdo <- main_gord_4 %>% filter(hernia_abdo == 1)
hernia_abdo <- table(hernia_abdo$same_day_hernia_abdo)

gastritis_duodenitis <- main_gord_4 %>% filter(gastritis_duodenitis == 1)
gastritis_duodenitis <- table(gastritis_duodenitis$same_day_gastritis_duodenitis)

oesoph_ulc <- main_gord_4 %>% filter(oesoph_ulc == 1)
oesoph_ulc <- table(oesoph_ulc$same_day_oesoph_ulc)

dyspepsia <- main_gord_4 %>% filter(dyspepsia == 1)
dyspepsia <- table(dyspepsia$same_day_dyspepsia)

dysphagia <- main_gord_4 %>% filter(dysphagia == 1)
dysphagia <- table(dysphagia$same_day_dysphagia)

abd_pain <- main_gord_4 %>% filter(abd_pain == 1)
abd_pain <- table(abd_pain$same_day_abd_pain)

vomiting <- main_gord_4 %>% filter(vomiting == 1)
vomiting <- table(vomiting$same_day_vomiting)

barretts <- main_gord_4 %>% filter(barretts == 1)
barretts <- table(barretts$same_day_barretts)

same_day_gord <- rbind(hernia_abdo,dyspepsia,dysphagia,gastritis_duodenitis,oesoph_ulc,barretts,abd_pain,vomiting)

same_day_gord <- same_day_gord %>%
  as.data.frame() %>%
  mutate(prop = round(`1`/(`1`+`0`),3))

xlsx::write.xlsx(same_day_gord,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "gord",append =TRUE)

#--------------------------------
hernia_abdo <- main_gastritis_duodenitis_4 %>% filter(hernia_abdo == 1)
hernia_abdo <- table(hernia_abdo$same_day_hernia_abdo)

oesoph_ulc <- main_gastritis_duodenitis_4 %>% filter(oesoph_ulc == 1)
oesoph_ulc <- table(oesoph_ulc$same_day_oesoph_ulc)

gord <- main_gastritis_duodenitis_4 %>% filter(gord == 1)
gord <- table(gord$same_day_gord)

dyspepsia <- main_gastritis_duodenitis_4 %>% filter(dyspepsia == 1)
dyspepsia <- table(dyspepsia$same_day_dyspepsia)

dysphagia <- main_gastritis_duodenitis_4 %>% filter(dysphagia == 1)
dysphagia <- table(dysphagia$same_day_dysphagia)

abd_pain <- main_gastritis_duodenitis_4 %>% filter(abd_pain == 1)
abd_pain <- table(abd_pain$same_day_abd_pain)

vomiting <- main_gastritis_duodenitis_4 %>% filter(vomiting == 1)
vomiting <- table(vomiting$same_day_vomiting)

barretts <- main_gastritis_duodenitis_4 %>% filter(barretts == 1)
barretts <- table(barretts$same_day_barretts)

same_day_gastritis_duodenitis <- rbind(hernia_abdo,dyspepsia,dysphagia,gord,oesoph_ulc,barretts,abd_pain,vomiting)

same_day_gastritis_duodenitis <- same_day_gastritis_duodenitis %>%
  as.data.frame() %>%
  mutate(prop = round(`1`/(`1`+`0`),3))

xlsx::write.xlsx(same_day_gastritis_duodenitis,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "gastritis_duodenitis",append =TRUE)

#------------------------------------
gastritis_duodenitis <- main_hernia_abdo_4 %>% filter(gastritis_duodenitis == 1)
gastritis_duodenitis <- table(gastritis_duodenitis$same_day_gastritis_duodenitis)

oesoph_ulc <- main_hernia_abdo_4 %>% filter(oesoph_ulc == 1)
oesoph_ulc <- table(oesoph_ulc$same_day_oesoph_ulc)

gord <- main_hernia_abdo_4 %>% filter(gord == 1)
gord <- table(gord$same_day_gord)

dyspepsia <- main_hernia_abdo_4 %>% filter(dyspepsia == 1)
dyspepsia <- table(dyspepsia$same_day_dyspepsia)

dysphagia <- main_hernia_abdo_4 %>% filter(dysphagia == 1)
dysphagia <- table(dysphagia$same_day_dysphagia)

abd_pain <- main_hernia_abdo_4 %>% filter(abd_pain == 1)
abd_pain <- table(abd_pain$same_day_abd_pain)

vomiting <- main_hernia_abdo_4 %>% filter(vomiting == 1)
vomiting <- table(vomiting$same_day_vomiting)

barretts <- main_hernia_abdo_4 %>% filter(barretts == 1)
barretts <- table(barretts$same_day_barretts)

same_day_hernia_abdo <- rbind(gastritis_duodenitis,dyspepsia,dysphagia,gord,oesoph_ulc,barretts,abd_pain,vomiting)

same_day_hernia_abdo <- same_day_hernia_abdo %>%
  as.data.frame() %>%
  mutate(prop = round(`1`/(`1`+`0`),3))

xlsx::write.xlsx(same_day_hernia_abdo,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "hernia_abdo",append =TRUE)

#----------------------------------------------------------------------------------------------
#getting ttd summary
hernia_abdo <- summary(main_barretts_4$td_hernia_abdo)
gastritis_duodenitis <- summary(main_barretts_4$td_gastritis_duodenitis)
oesoph_ulc <- summary(main_barretts_4$td_oesoph_ulc)
gord <- summary(main_barretts_4$td_gord)
dyspepsia <- summary(main_barretts_4$td_dyspepsia)
dysphagia <- summary(main_barretts_4$td_dysphagia)
abd_pain <- summary(main_barretts_4$td_abd_pain)
vomiting <- summary(main_barretts_4$td_vomiting)

td_barretts <- rbind(hernia_abdo,dyspepsia,dysphagia,gastritis_duodenitis,gord,oesoph_ulc,abd_pain,vomiting)

xlsx::write.xlsx(td_barretts,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "Barretts_td",append =TRUE)

hernia_abdo <- summary(main_oesoph_ulc_4$td_hernia_abdo)
gastritis_duodenitis <- summary(main_oesoph_ulc_4$td_gastritis_duodenitis)
barretts <- summary(main_oesoph_ulc_4$td_barretts)
gord <- summary(main_oesoph_ulc_4$td_gord)
dyspepsia <- summary(main_oesoph_ulc_4$td_dyspepsia)
dysphagia <- summary(main_oesoph_ulc_4$td_dysphagia)
abd_pain <- summary(main_oesoph_ulc_4$td_abd_pain)
vomiting <- summary(main_oesoph_ulc_4$td_vomiting)

td_oesoph_ulc <- rbind(hernia_abdo,dyspepsia,dysphagia,gastritis_duodenitis,gord,barretts,abd_pain,vomiting)

xlsx::write.xlsx(td_oesoph_ulc,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "oesoph_ulc_td",append =TRUE)

hernia_abdo <- summary(main_gord_4$td_hernia_abdo)
gastritis_duodenitis <- summary(main_gord_4$td_gastritis_duodenitis)
barretts <- summary(main_gord_4$td_barretts)
oesoph_ulc <- summary(main_gord_4$td_oesoph_ulc)
dyspepsia <- summary(main_gord_4$td_dyspepsia)
dysphagia <- summary(main_gord_4$td_dysphagia)
abd_pain <- summary(main_gord_4$td_abd_pain)
vomiting <- summary(main_gord_4$td_vomiting)

td_gord <- rbind(hernia_abdo,dyspepsia,dysphagia,gastritis_duodenitis,oesoph_ulc,barretts,abd_pain,vomiting)

xlsx::write.xlsx(td_gord,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "gord_td",append =TRUE)

hernia_abdo <- summary(main_gastritis_duodenitis_4$td_hernia_abdo)
gord <- summary(main_gastritis_duodenitis_4$td_gord)
barretts <- summary(main_gastritis_duodenitis_4$td_barretts)
oesoph_ulc <- summary(main_gastritis_duodenitis_4$td_oesoph_ulc)
dyspepsia <- summary(main_gastritis_duodenitis_4$td_dyspepsia)
dysphagia <- summary(main_gastritis_duodenitis_4$td_dysphagia)
abd_pain <- summary(main_gastritis_duodenitis_4$td_abd_pain)
vomiting <- summary(main_gastritis_duodenitis_4$td_vomiting)

td_gastritis_duodenitis <- rbind(hernia_abdo,dyspepsia,dysphagia,gord,oesoph_ulc,barretts,abd_pain,vomiting)

xlsx::write.xlsx(td_gastritis_duodenitis,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "gastritis_duodenitis_td",append =TRUE)

gastritis_duodenitis <- summary(main_hernia_abdo_4$td_gastritis_duodenitis)
gord <- summary(main_hernia_abdo_4$td_gord)
barretts <- summary(main_hernia_abdo_4$td_barretts)
oesoph_ulc <- summary(main_hernia_abdo_4$td_oesoph_ulc)
dyspepsia <- summary(main_hernia_abdo_4$td_dyspepsia)
dysphagia <- summary(main_hernia_abdo_4$td_dysphagia)
abd_pain <- summary(main_hernia_abdo_4$td_abd_pain)
vomiting <- summary(main_hernia_abdo_4$td_vomiting)

td_hernia_abdo <- rbind(gastritis_duodenitis,dyspepsia,dysphagia,gord,oesoph_ulc,barretts,abd_pain,vomiting)

xlsx::write.xlsx(td_hernia_abdo,file=".\\Count Tables & Graphs\\Study 1 outputs\\Revised results\\Same day diagnoses v2.xlsx",sheetName = "hernia_abdo_td",append =TRUE)

# save to sql
dbWriteTable(db, "disease_index_gord_all_flags",main_gord_4, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "disease_index_oesoph_ulc_all_flags",main_oesoph_ulc_4, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "disease_index_barretts_all_flags",main_barretts_4, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "disease_index_hernia_abdo_all_flags",main_hernia_abdo_4, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "disease_index_gastritis_duodenitis_all_flags",main_gastritis_duodenitis_4, row.names = FALSE, overwrite = TRUE)

#go to sql script 3.5
