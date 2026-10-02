rm(list=ls())

#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI, tsibble)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#------------------------------------------------
#Joining the test info into the main disease table
dbSendQuery(
  conn = db,
  statement = "
drop table if exists  freya.disease_index_all_pot_elig_tests;")

dbSendQuery(
  conn = db,
  statement = "
create table freya.disease_index_all_pot_elig_tests
(
  select l.*, d.enttype, d.medcode as test_medcode, d.test_date,d.descc,d.operator,d.value,d.specimenunitofmeasure,d.testqualifier,d.rangefrom,d.rangeto
  from freya.disease_index_all_pot_elig l
  left join freya.tests_desc d on d.e_patid = l.e_patid 
  and d.test_date <= l.eventdate
  and d.test_date >= date_sub(l.eventdate, interval 6 month)
)
;"
)

#------------------------------------------
query <- 'SELECT * FROM freya.disease_index_all_pot_elig_tests;'
rs <- dbSendQuery(db, query)
cohort_join <- fetch(rs, n=-1)

cohort_join <- cohort_join %>%
  mutate(eventdate=as.Date(eventdate))

#----------------------------------------
## Converting the haemoglobin variables to the same units (g/Dl)
cohort_join_convert <- cohort_join %>%
  filter(descc == "Haemoglobin estimation") %>%
  mutate(value = ifelse(specimenunitofmeasure == 'g/L',value/10,value))%>%
  mutate(specimenunitofmeasure = ifelse(specimenunitofmeasure == 'g/L','g/dL',specimenunitofmeasure))

#ensuring values are on the same scale if there is no specified unit of measure
cohort_join_na <- cohort_join_convert %>%
  filter(specimenunitofmeasure == 'No Data Entered' & !is.na(value))

cohort_join_na_gl <- cohort_join_na %>%
  filter(descc == "Haemoglobin estimation" & value > 30, value < 250 & testqualifier %in% c("Higher","Low","Abnormal","Normal")) %>%
  mutate(value =value/10) %>%
  mutate(specimenunitofmeasure = 'g/dL')

cohort_join_na_dl <- cohort_join_na %>%
  filter(descc == "Haemoglobin estimation" & value > 3, value < 25 & testqualifier %in% c("Higher","Low","Abnormal","Normal")) %>%
  mutate(specimenunitofmeasure = 'g/dL')

cohort_union <- union(cohort_join_na_gl,cohort_join_na_dl)

#extracting those with g/dl measurements
cohort_join_convert_dl <- cohort_join_convert %>% filter(descc == "Haemoglobin estimation" & specimenunitofmeasure == 'g/dL')

cohort_union_anaemia <- union(cohort_union,cohort_join_convert_dl)

#removing remaining impossible values & creating abnormal hb flag 
cohort_union_anaemia <- cohort_union_anaemia %>% 
  mutate(value = ifelse(value < 3 | value > 25, NA, value))%>%
  mutate(low_hb = ifelse(gender==1 & (value < 13),1,ifelse(gender==2 & (value < 12),1,0)))%>%
  select(c(e_patid,eventdate,low_hb)) %>%
  distinct()

#------------------------------------------------------------
#Sorting the platelets and wbcs
other_tests <- cohort_join %>%
  filter(descc == "Platelet count" | descc == "Total white cell count") %>%
  mutate(platelets = ifelse(descc == "Platelet count",1,0)) %>%
  mutate(wbc = ifelse(descc == "Total white cell count",1,0))

#creating raised flags and a dataframe of only platelets and wbc test results
other_tests <- other_tests %>%
  filter(platelets == 1 | wbc == 1) %>%
  mutate(raise_platelets = ifelse(platelets == 1 & value > 400 & specimenunitofmeasure == "10*9/L",1,0)) %>%
  mutate(raise_wbc = ifelse(wbc == 1 & value > 11 & specimenunitofmeasure == "10*9/L", 1,0)) %>%
  select(c(e_patid,eventdate,raise_platelets,raise_wbc))%>%
  distinct()

#-------------------------------------------------------------------
#Joining the test results to main dataframe before test results joined in
query <- 'SELECT * FROM freya.disease_index_all_pot_elig;'
rs <- dbSendQuery(db, query)
original_cohort <- fetch(rs, n=-1)

cohort_join_all <- original_cohort %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  left_join(cohort_union_anaemia,by=c("e_patid","eventdate")) %>%
  left_join(other_tests,by=c("e_patid","eventdate")) %>%
  mutate(low_hb = ifelse(is.na(low_hb),0,low_hb),
         raise_platelets = ifelse(is.na(raise_platelets),0,raise_platelets),
         raise_wbc = ifelse(is.na(raise_wbc),0,raise_wbc))

##--------------
## patients could have both a 0 and 1 test result for the same index date
## to combat this, ensuring only the positive test result is kept

## Function to keep only the positive test result or symptom recording
keep_pos <- function(data,variable,new_variable){
  #data <- main_all
  sub <- data %>%
    select(e_patid,UQ(variable)) %>%
    arrange(e_patid,UQ(variable)) %>%
    distinct()
  
  n <- sub %>%
    group_by(e_patid) %>%
    count(e_patid)
  
  sub_jn <- left_join(sub,n)
  
  sub_jn <- sub_jn %>%
    mutate("{new_variable}" := ifelse(n>1,1,0)) %>%
    select(-n) %>%
    distinct()
  
  data <- data %>%
    select(-c(UQ({variable}))) %>%
    distinct()
  
  out <- left_join(data,sub_jn)
  #out <- subset(out,select=-22) 
  return(out)
}

#anaemia
variable <- quo('low_hb')
new_variable <- as.name('low_hb')
cohort_join_all_1 <- keep_pos(cohort_join_all,variable,new_variable)

#platelets
variable <- quo('raise_platelets')
new_variable <- as.name('raise_platelets')
cohort_join_all_2 <- keep_pos(cohort_join_all_1,variable,new_variable)

#wbcs
variable <- quo('raise_wbc')
new_variable <- as.name('raise_wbc')
cohort_join_all_3 <- keep_pos(cohort_join_all_2,variable,new_variable)


dbWriteTable(db, "disease_index_all_pot_elig_tests_1",cohort_join_all_3,row.names= FALSE, overwrite = TRUE)

#Go to R script 3.2.1