rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI, tsibble, desc_tools)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

query <- 'SELECT * FROM freya.h_pylori_prescriptions;'
rs <- dbSendQuery(db, query)
data <- fetch(rs, n=-1)

data <- data %>%
  select(-c(prodcode,qty))

#splitting the data into individual prescription cohorts
amoxicillin <- data %>%
  filter(drugsubstance == "Amoxicillin trihydrate" | drugsubstance == "Amoxicillin trihydrate/Potassium clavulanate") %>%
  rename(amoxicillin = drugsubstance)

clarithromycin <- data %>%
  filter(drugsubstance == "Clarithromycin")%>%
  rename(clarithromycin = drugsubstance)

metronidazole <- data %>%
  filter(drugsubstance == "Metronidazole"|drugsubstance == "Metronidazole/Diloxanide Furoate"|drugsubstance == "Metronidazole benzoate")%>%
  rename(metronidazole = drugsubstance)

tetracycline <- data %>%
  filter(grepl("cycline", drugsubstance, ignore.case=TRUE))%>%
  filter(grepl("hyclate", drugsubstance, ignore.case=TRUE))%>%
  rename(tetracycline = drugsubstance)

ppis <- data %>%
  filter(drugsubstance == "Lansoprazole"|drugsubstance =="Omeprazole"|drugsubstance =="Esomeprazole"|
           drugsubstance =="Pantoprazole"|drugsubstance =="Rabprazole")%>%
  rename(ppis = drugsubstance)

bismuth <- data %>%
  filter(grepl("bismuth", drugsubstance, ignore.case=TRUE))%>%
    rename(bismuth = drugsubstance)

#joining the antibiotic cohorts back into the ppi cohort for each combination of treatments
#specified in the audit document
all_data_v1 <- ppis %>%
  left_join(amoxicillin, by = c("e_patid","eventdate")) %>%
  left_join(clarithromycin, by = c("e_patid","eventdate")) %>%
  left_join(metronidazole, by = c("e_patid","eventdate")) %>%
  mutate(amoxicillin = ifelse(is.na(amoxicillin),0,1)) %>%
  mutate(clarithromycin = ifelse(is.na(clarithromycin),0,1)) %>%
  mutate(metronidazole = ifelse(is.na(metronidazole),0,1)) %>%
  mutate(ppis = ifelse(is.na(ppis),0,1)) %>%
  mutate(h_pylori_flag = ifelse(amoxicillin == 1 & (clarithromycin == 1 | metronidazole == 1),1,0)) %>%
  filter(h_pylori_flag == 1) %>%
  select(e_patid,eventdate)

all_data_v2 <- ppis %>%
  left_join(clarithromycin, by = c("e_patid","eventdate")) %>%
  left_join(metronidazole, by = c("e_patid","eventdate")) %>%
  mutate(clarithromycin = ifelse(is.na(clarithromycin),0,1)) %>%
  mutate(metronidazole = ifelse(is.na(metronidazole),0,1)) %>%
  mutate(ppis = ifelse(is.na(ppis),0,1)) %>%
  mutate(h_pylori_flag = ifelse(clarithromycin == 1 & metronidazole == 1,1,0)) %>%
  filter(h_pylori_flag == 1) %>%
  select(e_patid,eventdate)

all_data_v3 <- ppis %>%
  left_join(bismuth, by = c("e_patid","eventdate")) %>%
  left_join(tetracycline, by = c("e_patid","eventdate")) %>%
  left_join(metronidazole, by = c("e_patid","eventdate")) %>%
  mutate(bismuth = ifelse(is.na(bismuth),0,1)) %>%
  mutate(tetracycline = ifelse(is.na(tetracycline),0,1)) %>%
  mutate(metronidazole = ifelse(is.na(metronidazole),0,1)) %>%
  mutate(ppis = ifelse(is.na(ppis),0,1)) %>%
  mutate(h_pylori_flag = ifelse(bismuth == 1 & tetracycline == 1 & metronidazole == 1,1,0)) %>%
  filter(h_pylori_flag == 1) %>%
  select(e_patid,eventdate)

#union the dataframes and save to sql to join into main dataframe
final_data <- all_data_v1 %>%
  union(all_data_v2) %>%
  union(all_data_v3) %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  rename(h_pylori = eventdate)

#load main dataframe
query <- 'SELECT * FROM freya.cohortv1_1_1;'
rs <- dbSendQuery(db, query)
cohort <- fetch(rs, n=-1)

cohort_join <- cohort %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  left_join(final_data, by = c("e_patid")) %>%
  mutate(h_pylori_flag = ifelse(h_pylori >= eventdate %m-% months(6) & h_pylori <= eventdate, 1,0 )) %>%
  select(-h_pylori) %>%
  mutate(h_pylori_flag = ifelse(is.na(h_pylori_flag),0,1)) %>%
  distinct()

dbWriteTable(db, "cohortv1_1_2",cohort_join,row.names= FALSE, overwrite = TRUE)

#go to script 1.3

