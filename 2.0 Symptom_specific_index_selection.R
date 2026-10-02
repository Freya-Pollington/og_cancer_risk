rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(tidyverse,
                RMySQL, DBI)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)
#--------------------------------------------------------------------
query <- 'SELECT * FROM freya.cohort_dyspepsia_cr;'
rs <- dbSendQuery(db, query)
dyspepsia <- fetch(rs, n=-1)

dyspepsia <- dyspepsia %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(diagnosisdatebest = as.Date(diagnosisdatebest))
  
query <- 'SELECT * FROM freya.cohort_dysphagia_cr;'
rs <- dbSendQuery(db, query)
dysphagia <- fetch(rs, n=-1)

dysphagia <- dysphagia %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(diagnosisdatebest = as.Date(diagnosisdatebest))

query <- 'SELECT * FROM freya.cohort_vomiting_cr;'
rs <- dbSendQuery(db, query)
vomiting <- fetch(rs, n=-1)

vomiting <- vomiting %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(diagnosisdatebest = as.Date(diagnosisdatebest))

query <- 'SELECT * FROM freya.cohort_abd_pain_cr;'
rs <- dbSendQuery(db, query)
abd_pain <- fetch(rs, n=-1)

abd_pain <- abd_pain %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(diagnosisdatebest = as.Date(diagnosisdatebest))

#-------------------------------------------------------------------------------------------------
## Filtering to one eligible index

#Extracting patients with a stomach or oesophageal cancer diagnosis 
#(could also have other diagnoses or blanks where ICD10 non cancerous code was present in the cancer data, but I removed and replaced with NAs)

elig_ind <- function(x){

  #filtering out non cancerous icd 10 in site specific column
  x <- x %>%
    mutate(cancer_site_desc_v2 = ifelse(cancer_site_desc == "Non cancerous ICD10",NA,cancer_site_desc)) %>%
    select(-"cancer_site_desc")%>%
    mutate(diagnosisdatebest2 = ifelse(is.na(cancer_site_desc_v2)==TRUE,NA,diagnosisdatebest))%>% 
    mutate(diagnosisdatebest2 = as.Date(diagnosisdatebest2)) %>% 
    select(-"diagnosisdatebest")
  
  n <- x %>%
    group_by(e_patid, eventdate) %>%
    filter(cancer_site_desc_v2 == "Stomach" | cancer_site_desc_v2 == "Oesophagus") %>%
    ungroup() %>%
    select(e_patid)
  
  #joining into dataframe to see other diagnoses
  data_elig_jn <- left_join(n,x)
  
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
  anti <- anti_join(x,n)
  
  #union to add the patients with those diagnoses back in without the extra diagnoses/blanks 
  union_post <- union(anti,union_pre)
  length(unique(union_post$e_patid))
  
  #one symptom (could have more than one occurrence of the same symptom, so now need to randomly take one as index)
  set.seed(4321)
  data_elig_one <- union_post %>%
    group_by(e_patid) %>%
    sample_n(1)
  
  #Creating a flag for valid diagnosis of Oesophago-gastric cancer within 1 year of index
  data_elig_one <- data_elig_one %>%
    mutate(og_cancer = ifelse(
      (is.na(diagnosisdatebest2) == FALSE) &
        ((cancer_site_desc_v2 == "Stomach") | (cancer_site_desc_v2 == "Oesophagus")) &
        (diagnosisdatebest2 >= eventdate) &
        (diagnosisdatebest2 <= eventdate %m+% years(1)),
      1,0
    ))
  
}

dyspepsia_elig <- elig_ind(dyspepsia)
abd_pain_elig <- elig_ind(abd_pain)
dysphagia_elig <- elig_ind(dysphagia)
vomiting_elig <- elig_ind(vomiting)

#Sending the data back to SQL to join in non-neoplastic and smoking information
dbWriteTable(db, "cohort_dyspepsia_cr_flag",dyspepsia_elig, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_dysphagia_cr_flag",dysphagia_elig, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_vomiting_cr_flag",vomiting_elig, row.names = FALSE, overwrite = TRUE)
dbWriteTable(db, "cohort_abd_pain_cr_flag",abd_pain_elig, row.names = FALSE, overwrite = TRUE)

#go to sql script 2.1