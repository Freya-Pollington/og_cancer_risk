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

#------------------------------------------------------------------------------------------
#### Define potentially eligible events for all####

query <- 'SELECT * FROM freya.disease_index_all;'
rs <- dbSendQuery(db, query)
data <- fetch(rs, n=-1)
data <- data %>%
     mutate(eventdate = as.Date(eventdate)) 

#Filling the gastritis descriptions from the unincluded medcodes in the lookup with a description
data$disease_name <- ifelse(is.na(data$disease_name) & data$code_number %like% "K29","gastritis_duodenitis",data$disease_name)
data$disease_name <- ifelse(is.na(data$disease_name) & (data$code_number %like% "K44" | data$code_number %like% "K43"),"hernia_abdo",data$disease_name)

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
  mutate(deathdate = as.Date(deathdate)) %>%
  mutate(age30date = as.Date(age30date, format = "%Y-%m-%d")) %>%
  mutate(age100date = as.Date(age100date, format = "%Y-%m-%d"))

# Study start & end values

# Study_start: Cohort from CPRD is from start 2007
study_start <- as.Date("2007-01-01")

# Study end: NCRAS data will be until end of 2018, 
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
  mutate(data, pot_elig = ifelse (
    eventdate >= (crd %m+% years(1)) &
      eventdate >= (uts %m+% years(1))  &
      eventdate >= age30date &
      eventdate >= study_start &
      
      eventdate <= lcd & 
      (eventdate <= tod | is.na(tod))   & 
      
      (eventdate <= deathdate |  is.na(deathdate)) &
      eventdate <= age100date &
      eventdate <= study_end,
    1, 0
  ))

#saving this table to sql for use with other symptoms before filtering
dbWriteTable(db, "disease_index_all_pot_elig_flag",data,overwrite = TRUE, row.names =FALSE)

#filtering to only potentially eligible events for joining into the symptom cohorts
data_elig <- data %>% filter(pot_elig == 1)

dbWriteTable(db, "disease_index_all_pot_elig",data_elig,overwrite = TRUE, row.names =FALSE)
  
# Go to R script 3.2  