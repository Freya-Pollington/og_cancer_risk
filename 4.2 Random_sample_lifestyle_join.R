rm(list=ls())

#### Loading Packages ####
library(pacman)

pacman::p_load(tidyverse,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI,comorbidity,mice)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)
#-------------------------
#Extracting ICD10 codes for elixhauser score
dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.elixhauser_codes_rand_v2;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.elixhauser_codes_rand_v2
SELECT a.e_patid,a.eventdate,b.icd
FROM freya.random_sample_elig_v2 a 
LEFT JOIN 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_hosp b on b.e_patid = a.e_patid
WHERE b.discharged >= date_sub(a.eventdate,interval 20 year)
AND b.discharged <= a.eventdate
;')


query <- 'SELECT * FROM freya.random_sample_elig_v2;'
rs <- dbSendQuery(db, query)
rand <- fetch(rs, n=-1)

query <- 'SELECT * FROM freya.elixhauser_codes_rand_v2;'
rs <- dbSendQuery(db, query)
elixhauser <- fetch(rs, n=-1)

#----------------------------------
#calculate elixhauser score

com <- comorbidity(x=elixhauser, id="e_patid", code= "icd",map = "elixhauser_icd10_quan",assign0 = FALSE)
#doing my own version removing metastatic cancer
com <- com %>% select(- c(metacanc,solidtum))

unw_score <- rowSums(com[,2:30])

elixhauser_score <- cbind(com$e_patid, unw_score)
elixhauser_score <- as.data.frame(elixhauser_score)
names(elixhauser_score)[1] <- "e_patid"

# Joining the elixhauser scores back into the main dataframe
data_all <- left_join(rand, elixhauser_score)
data_all$unw_score <- ifelse(is.na(data_all$unw_score),0,data_all$unw_score)

#saving the score table to sql
dbWriteTable(db, "random_sample_elig_elix_v2",data_all, row.names = FALSE, overwrite = TRUE)

#--------------------------------
#join in smoking information

dbSendQuery(
  conn = db,
  statement = '
CREATE INDEX e_patid on freya.random_sample_elig_elix_v2(e_patid);')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.smoking_pot_elig_rand_v2;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.smoking_pot_elig_rand_v2(
  SELECT b.e_patid, a.eventdate AS smok_date, m.smokingcat
  FROM freya.random_sample_elig_elix_v2 b
  INNER JOIN 18_299_Lyratzopoulos_e2.cprd_clinical a ON a.e_patid=b.e_patid
  INNER JOIN freya.smoking_codelist m ON a.medcode=m.medcode
  WHERE a.eventdate <= "2017-12-31" AND a.eventdate >= "1992-01-01"
);')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.smoking_pot_elig_rand_dist_v2;')

dbSendQuery(
  conn = db,
  statement = '
CREATE TABLE freya.smoking_pot_elig_rand_dist_v2
SELECT DISTINCT *
  FROM freya.smoking_pot_elig_rand_v2;')

dbSendQuery(
  conn = db,
  statement = '
DROP TABLE IF EXISTS freya.random_sample_elig_elix_sm_v2;')

dbSendQuery(
  conn = db,
  statement = '
  CREATE TABLE freya.random_sample_elig_elix_sm_v2
  SELECT v2.*, cl.smokingcat, cl.smok_date
  FROM freya.random_sample_elig_elix_v2 v2
  LEFT JOIN freya.smoking_pot_elig_rand_dist_v2 cl ON cl.e_patid = v2.e_patid
  WHERE cl.smok_date >= date_sub(v2.eventdate, interval 20 year) 
  AND cl.smok_date <= v2.eventdate;'
  )


#--------------------
#Function for changing to being current, ex smoker or never smoker and saving table to sql
smoker_fun <- function(data,tab_name){
  #create a flag for the most recent smoking date, a flag for if the record is the most recent, 
  #change the 'current or ex' to current, create a column of the most recent smoking status
  data <- data %>%
    mutate(smok_date = as.Date(smok_date)) %>%
    group_by(e_patid) %>%
    mutate(most_recent_smok_date = max(smok_date))%>%
    mutate(max_smok_date_flag = ifelse(most_recent_smok_date == smok_date,1,0)) %>%
    mutate(smokingcat = ifelse(smokingcat == 3, 4, ifelse(is.na(smokingcat),1,smokingcat))) %>%
    mutate(recent_smoking_status = ifelse(max_smok_date_flag == 1, smokingcat, 0))
  
  #find the number of different smoking statuses for each patient
  data_2 <- data %>%
    group_by(e_patid) %>%
    summarise(n_distinct(smokingcat))
  names(data_2)[-1] <- "n_dis"
  
  data_jn <- left_join(data,data_2)
  
  #create a column for if a patient has more than one smoking status, what is the 'highest' i.e. 4 is current smoker
  #filter for the most recent smoking status date, and if the most recent smoking status is 1, but they have had a previously 
  #'higher' smoking status, then keep the higher one for the final
  #'    #create a column for the order placement of a smoking status if a patient has more than one smoking status
  #'        #fill in the most recent value for patients with more than one smoking status over time
  #'            #find those who have a recent never status, but an ex status within the previous 5 records
  #'                #find those who have a recent never status, but current status within the previous 5 records
  #'                    #if a patient has most recent never smoker status, but also a recent ex status, code ex, if no ex but a current, code current
  #'                        #if only never codes for the previous 5 records, but they have an ex or current code before that, code ex, otherwise leave as is for most recent smoking status
  #'                        #filtering steps to ensure one row per patient

  data_jn2 <- data_jn %>%
    group_by(e_patid) %>%
    arrange(e_patid,desc(smok_date)) %>%
    mutate(n_rec = row_number()) %>%
    fill(recent_smoking_status, .direction="downup") %>%
    mutate(recent_ex_smoker = ifelse(n_dis>1 & n_rec >= 2 & n_rec <= 5 & recent_smoking_status == 1 & smokingcat == 2,1,0)) %>%
    mutate(recent_smoker = ifelse(n_dis>1 & n_rec >= 2 & n_rec <= 5 & recent_smoking_status == 1 & smokingcat == 4,1,0)) %>%
    mutate(updated_smoking_status = ifelse(recent_smoking_status == 1 & recent_ex_smoker == 1,2,
                                           ifelse(recent_smoking_status == 1 & recent_smoker == 1,4,
                                                  ifelse(recent_smoking_status == 1 & recent_ex_smoker == 0 & recent_smoker == 0 & max(smokingcat) > 1,2,recent_smoking_status)))) %>% 
    filter(most_recent_smok_date == smok_date) %>%
    select(-c(most_recent_smok_date,max_smok_date_flag,smokingcat,recent_smoking_status,n_dis,smok_date,n_rec,recent_ex_smoker,recent_smoker)) %>%
    distinct() %>%
    mutate(final_smoking_status = max(updated_smoking_status)) %>%
    select(-updated_smoking_status) %>%
    distinct()
  
  dbWriteTable(db, tab_name,data_jn2, row.names = FALSE, overwrite = TRUE)
  
  return(data_jn2)
}
#---------------------------
query <- 'SELECT * FROM freya.random_sample_elig_elix_sm_v2;'
rs <- dbSendQuery(db, query)
data <- fetch(rs, n=-1)

tab_name <- "random_sample_elig_elix_sm_flag_v2"

data_flags <- smoker_fun(data,tab_name)
#---------------------------
#joining in obesity and alcohol info

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
}
#--------------------------------------------------
query <- 'SELECT * FROM freya.alc_problems;'
rs <- dbSendQuery(db, query)
alc_problems <- fetch(rs, n=-1)

names(alc_problems)[2] <- "alc_problems"
alc_problems <- alc_problems[,1:2]

query <- 'SELECT * FROM freya.obesity;'
rs <- dbSendQuery(db, query)
obesity <- fetch(rs, n=-1)

names(obesity)[2] <- "obesity"
obesity <- obesity[,1:2]

query <- 'SELECT * FROM random_sample_elig_elix_sm_flag_v2;'
rs <- dbSendQuery(db, query)
data_flags <- fetch(rs, n=-1)

lifestyle_list <- list(obesity=obesity,alc_problems=alc_problems)

for(lf_name in names(lifestyle_list)) {
  data_flags <- lifestyle_join(data_flags, lifestyle_list[[lf_name]], lf_name) %>%
    distinct()
}

#----------------------------------------------------
#adding in height/weight for bmi calculation

bmi_calc <- function(data,height_data,weight_data){
  data_full_join <-left_join(data,weight, by = 'e_patid')
  
  #sense checking weight values
  data_full_join <- data_full_join %>%
    mutate(eventdate.x = as.Date(eventdate.x)) %>%
    mutate(eventdate.y = as.Date(eventdate.y))
  
  data_full_join_fil <- data_full_join %>%
    filter(data1 > 30 & data1 < 250) 
  
  #taking only values before the eventdate and then the most recent weight 
  #and one per person with a random selection if more than one on one day
  data_full_join_2 <- data_full_join_fil %>%
    mutate(previous_weight = ifelse(eventdate.y <= eventdate.x,1,0))%>%
    filter(previous_weight == 1) %>%
    group_by(e_patid) %>%
    slice_max(order_by = eventdate.y,n=1)%>%
    distinct()%>%
    sample_n(1) %>%
    select(-previous_weight)
  
  #finding those patients who did not have a valid weight before the eventdate
  data_full_join_anti <- data_full_join %>%
    anti_join(data_full_join_2,by='e_patid') %>%
    group_by(e_patid) %>%
    sample_n(1) 
  
  #joining the patients without a valid weight into the main dataframe
  data_full_join_3 <- data_full_join_2 %>%
    union(data_full_join_anti) %>%
    rename(weight = data1)
  
  #joining most recent height information and calculating bmi
  #creating a bmi category variable for obese (3), overweight (2), underweight (1), and healthy weight (0)
  data_full_join_4 <- data_full_join_3 %>%
    left_join(height_fil,by='e_patid') %>%
    mutate(height = as.numeric(height)) %>%
    mutate(weight = as.numeric(weight)) %>%
    mutate(bmi = (weight/(height*height))) %>%
    mutate(bmi_cat = ifelse(bmi>=30,3,
                            ifelse(bmi<30 & bmi >= 25,2,
                                   ifelse(bmi<25 & bmi>=18.5,0,
                                          ifelse(bmi<18.5,1,NA))))) %>%
    mutate(bmi_cat = ifelse(is.na(bmi_cat) & obesity == 1, 3,ifelse(is.na(bmi_cat) & obesity == 0,NA,bmi_cat)))
  
  #imputing the remaining bmi values
  data_full_join_prep <- data_full_join_4 %>% ungroup() %>%
    select(-c(height,bmi,eventdate.y)) %>%
    mutate(bmi_cat = as.factor(bmi_cat))
  
  set.seed(123)
  data_impute <- mice(data_full_join_prep,method='cart')
  
  data_impute_complete <- complete(data_impute,1)
}

#---------------------------------------------------
#loading height and weight data
height <- readRDS('S://ECHO_IHI_CPRD//Data//Yangfan//pat_basic_data//all_height.rds')

weight <- readRDS('S://ECHO_IHI_CPRD//Data//Yangfan//pat_basic_data//all_weight.rds')

height <- height %>% arrange((data1))

#finding most recent height and taking one per patient
height_fil <- height %>%
  group_by(e_patid) %>%
  filter(data1 > 1.45 & data1 < 2.25) %>% #sense checking height
  mutate(most_recent_height = max(eventdate))%>%
  mutate(most_recent_height_flag = ifelse(most_recent_height == eventdate,1,0)) %>%
  mutate(height = ifelse(most_recent_height_flag == 1, data1, NA)) %>%
  filter(!is.na(height)) %>%
  select(e_patid,eventdate,height) %>%
  distinct() %>%
  sample_n(1)

data_out <- bmi_calc(data_flags,height_fil,weight)

dbWriteTable(db, "random_sample_lifestyle_intermediate_v2",data_out, row.names = FALSE, overwrite = TRUE)

#------------------------------
#function to process alcohol information and make a factor variable for alcohol status

alc_fun <- function(data_alcohol,data_complete){
  #data_alcohol <- random_alc
  data_alcohol_fil <- data_alcohol %>%
    group_by(e_patid) %>%
    mutate(alc_eventdate = as.Date(alc_eventdate)) %>%
    mutate(most_recent_date = max(alc_eventdate))%>%
    mutate(most_recent_flag = ifelse(most_recent_date == alc_eventdate,1,0)) %>%
    mutate(alc_status = ifelse(most_recent_flag == 1, data1, NA)) %>%
    filter(!is.na(alc_status))%>%
    select(-c(data1,alc_eventdate,most_recent_date,most_recent_flag))
  
  #need to filter by most recent alcohol status for those with more than one
  #same method as applied to smoking
  data_alcohol_fil2 <- data_alcohol_fil %>%
    group_by(e_patid) %>%
    #re-structuring the alcohol levels to make more sense i.e. 3 is yes, 2 is ex, 1 is no, 0 is NA
    mutate(alc_status = ifelse(alc_status == 1, 3, 
                               ifelse(alc_status == 3, 2,
                                      ifelse(alc_status == 2, 1,0)))) %>%
    slice_max(alc_status)
  
  #join into the main dataframe
  data_complete_join <- data_out %>%
    left_join(data_alcohol_fil2, by='e_patid') %>%
    mutate(alc_status_comb = ifelse(alc_problems == 1 & alc_status == 3, 5, #drinker with a history of alcohol problems
                                    ifelse(alc_problems == 0 & alc_status == 3, 4, #drinker with no history of alcohol problems
                                           ifelse(alc_problems == 1 & alc_status == 2, 3, #ex drinker with history of alcohol problems
                                                  ifelse(alc_problems == 0 & alc_status == 2, 2, #ex drinker with no history of alcohol problems
                                                         ifelse(alc_problems == 1 & alc_status == 1, 3, #non-drinker with history of alcohol problems (setting them as an ex drinker with history)
                                                                ifelse(alc_problems == 0 & alc_status == 1, 1,NA)))))))#non-drinker with no history of alcohol problems
  
  
  #imputing the remaining alcohol values
  data_complete_join_prep <- data_complete_join %>% ungroup() %>%
    select(-c(alc_status,eventdate.x.x,eventdate.y,weight)) %>%
    mutate(alc_status_comb = as.factor(alc_status_comb)) %>%
    rename(eventdate = eventdate.x)
  
  set.seed(123)
  data_impute <- mice(data_complete_join_prep,method='cart')
  
  data_impute_complete <- complete(data_impute,1)
  
}

#creating the random sample alcohol variable

dbSendQuery(
  conn = db,
  statement = 'DROP TABLE IF EXISTS freya.cohort_random_sample_alc_v2;')
    
dbSendQuery(
  conn = db,
  statement = 
"CREATE TABLE freya.cohort_random_sample_alc_v2
SELECT v.e_patid,ab.eventdate,v.eventdate as alc_eventdate,v.data1
FROM freya.random_sample_elig_elix_sm_flag_v2 ab
INNER JOIN freya.alcohol_join_dis v on v.e_patid = ab.e_patid
AND ab.eventdate >= v.eventdate
;")


query <- 'SELECT * FROM freya.cohort_random_sample_alc_v2;'
rs <- dbSendQuery(db, query)
random_alc <- fetch(rs, n=-1)

query <- 'SELECT * FROM freya.random_sample_lifestyle_intermediate_v2;'
rs <- dbSendQuery(db, query)
data_out <- fetch(rs, n=-1)

#taking only 5% of the random sample to avoid overlap
set.seed(123)
data_out <- data_out %>%
  group_by(e_patid) %>%
  dplyr::sample_frac(0.05,replace=FALSE) %>%
  ungroup() %>%
  distinct()

data_final <- alc_fun(random_alc,data_out)

data_final <- distinct(data_final)

dbWriteTable(db, "random_sample_final_v2",data_final, row.names = FALSE, overwrite = TRUE)

#go to R script 5.0