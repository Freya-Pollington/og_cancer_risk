rm(list=ls())

#### Loading Packages ####
library(pacman)
pacman::p_load(tidyverse,tidyr,lubridate, 
               data.table, RMySQL, DBI,xlsx,purrr,table1,gtools,flextable)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#---------------------------------------------
#Joining all dataframes together

  query <- 'SELECT * FROM freya.random_sample_final_v2;'
  rs <- dbSendQuery(db, query)
  rand <- fetch(rs, n=-1)
  
  #adding columns into random sample
  rand <- rand %>%
    mutate(dyspepsia = 0) %>%
    mutate(dysphagia = 0) %>%
    mutate(low_hb = 0) %>%
    mutate(raise_platelets = 0) %>%
    mutate(raise_wbc = 0) %>%
    mutate(weight_loss = 0) %>%
    mutate(appetite_loss = 0) %>%
    mutate(cough = 0) %>%
    mutate(fatigue = 0) %>%
    mutate(abd_pain = 0) %>%
    #mutate(ever_alcohol_problems = 0) %>%
    mutate(vomiting = 0) %>%
    mutate(h_pylori_flag = 0) %>% 
    mutate(eventdate = as.Date(eventdate)) %>%
    mutate(oesoph_ulc = 0) %>%
    mutate(gord = 0) %>%
    mutate(barretts = 0) %>%
    mutate(hernia_abdo = 0) %>%
    mutate(gastritis_duodenitis = 0) %>%
    mutate(cat = "0") 
  
  rand$category <- "Random sample"
  
query <- 'SELECT * FROM cohort_abd_pain_final;'
rs <- dbSendQuery(db, query)
abd_pain <- fetch(rs, n=-1)

abd_pain$category <- "Abdominal pain"

#removing those in the random sample from each cohort
abd_pain <- abd_pain %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  anti_join(rand,by=c("e_patid","eventdate"))

abd_pain <- abd_pain %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                                oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)
#---
query <- 'SELECT * FROM cohort_dyspepsia_final;'
rs <- dbSendQuery(db, query)
dyspepsia <- fetch(rs, n=-1)

dyspepsia$category <- "Dyspepsia"

dyspepsia <- dyspepsia %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  anti_join(rand,by=c("e_patid","eventdate"))

dyspepsia <- dyspepsia %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                                  oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)
#-----
query <- 'SELECT * FROM cohort_dysphagia_final;'
rs <- dbSendQuery(db, query)
dysphagia <- fetch(rs, n=-1)

dysphagia$category <- "Dysphagia"

dysphagia <- dysphagia %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  anti_join(rand,by=c("e_patid","eventdate"))

dysphagia <- dysphagia %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                                  oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)
#-----------
query <- 'SELECT * FROM cohort_vomiting_final;'
rs <- dbSendQuery(db, query)
vomiting <- fetch(rs, n=-1)

vomiting$category <- "Vomiting"

vomiting <- vomiting %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  anti_join(rand,by=c("e_patid","eventdate"))

vomiting <- vomiting %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                                oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)
#--------
query <- 'SELECT * FROM cohort_barretts_final;'
rs <- dbSendQuery(db, query)
barretts <- fetch(rs, n=-1)

barretts$barretts <- NA

barretts$category <- "Barretts"

barretts <- barretts %>%
  rename(ever_alcohol_problems=alc_problems) %>%
  rename(ever_obesity=obesity)

barretts <- barretts %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  anti_join(rand,by=c("e_patid","eventdate"))

barretts <- barretts %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                                oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)
#-----------
query <- 'SELECT * FROM cohort_gord_final;'
rs <- dbSendQuery(db, query)
gord <- fetch(rs, n=-1)

gord$gord <- NA

gord$category <- "GORD"

gord <- gord %>%
  rename(ever_alcohol_problems=alc_problems) %>%
  rename(ever_obesity=obesity)

gord <- gord %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  anti_join(rand,by=c("e_patid","eventdate"))

gord <- gord %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                        oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)
#------------
query <- 'SELECT * FROM cohort_gastritis_duodenitis_final;'
rs <- dbSendQuery(db, query)
gastritis_duodenitis <- fetch(rs, n=-1)

gastritis_duodenitis$gastritis_duodenitis <- NA

gastritis_duodenitis$category <- "Gastritis duodenitis"

gastritis_duodenitis <- gastritis_duodenitis %>%
  rename(ever_alcohol_problems=alc_problems) %>%
  rename(ever_obesity=obesity)

gastritis_duodenitis <- gastritis_duodenitis %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  anti_join(rand,by=c("e_patid","eventdate"))

gastritis_duodenitis <- gastritis_duodenitis %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                                                        oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)
#------------
query <- 'SELECT * FROM cohort_hernia_abdo_final;'
rs <- dbSendQuery(db, query)
hernia_abdo <- fetch(rs, n=-1)

hernia_abdo$hernia_abdo <- NA

hernia_abdo$category <- "Abdominal hernia"

hernia_abdo <- hernia_abdo %>%
  rename(ever_alcohol_problems=alc_problems) %>%
  rename(ever_obesity=obesity)

hernia_abdo <- hernia_abdo %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  anti_join(rand,by=c("e_patid","eventdate"))

hernia_abdo <- hernia_abdo %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                                      oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)
#------------
query <- 'SELECT * FROM cohort_oesoph_ulc_final;'
rs <- dbSendQuery(db, query)
oesoph_ulc <- fetch(rs, n=-1)

oesoph_ulc$oesoph_ulc <- NA

oesoph_ulc$category <- "Oeosphageal ulcer"

oesoph_ulc <- oesoph_ulc %>%
  rename(ever_alcohol_problems=alc_problems) %>%
  rename(ever_obesity=obesity)

oesoph_ulc <- oesoph_ulc %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  anti_join(rand,by=c("e_patid","eventdate"))

oesoph_ulc <- oesoph_ulc %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                                    oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)

rand <- rand %>% select(e_patid,gender,alc_status_comb,bmi_cat,low_hb,raise_platelets,raise_wbc,h_pylori_flag,weight_loss,cough,appetite_loss,fatigue,abd_pain,dyspepsia,dysphagia,vomiting,age_band, 
                        oesoph_ulc,gord,barretts,hernia_abdo,gastritis_duodenitis,unw_score,final_smoking_status,category)
#-----------
data <- rbind(rand,abd_pain,dyspepsia,dysphagia,vomiting,hernia_abdo,barretts,gastritis_duodenitis,gord,oesoph_ulc)
data <- data %>% distinct()


#creating a banded comorbidity status variable
data$unw_score <- ifelse(is.na(data$unw_score),0,data$unw_score)
data$unw_score <- as.numeric(data$unw_score)

data$elix_band <- cut(data$unw_score,breaks = c(0,1,3,5,27),include.lowest=TRUE)

#currently non neoplastic variables are count variables not binary, need to change
data <- data %>%
  mutate(oesoph_ulc_bin = ifelse(oesoph_ulc == 0,0,1)) %>%
  mutate(gord_bin = ifelse(gord == 0,0,1)) %>%
  mutate(barretts_bin = ifelse(barretts == 0,0,1)) %>%
  mutate(gastritis_duodenitis_bin = ifelse(gastritis_duodenitis == 0,0,1)) %>%
  mutate(hernia_abdo_bin = ifelse(hernia_abdo == 0,0,1))

# creating 80+ category
data <- data %>%
  mutate(age_band = ifelse(age_band == "90,100" | age_band == "80,90","80+",age_band))

#fixing the na values for platelets and wbcs
data <- data %>%
  mutate(raise_platelets = ifelse(is.na(raise_platelets),0,raise_platelets)) %>%
  mutate(raise_wbc = ifelse(is.na(raise_wbc),0,raise_wbc)) %>%
  mutate(low_hb = ifelse(is.na(low_hb),0,low_hb))

#creating variables for number of unique patients with any OG cancer symptom and for the vague/alarm symptoms and the non-neoplastic diseases
data <- data %>%
  group_by(category) %>%
  mutate(anyOGsymp = ifelse(dyspepsia == 1 | dysphagia == 1 | abd_pain == 1 | vomiting == 1, 1, 0)) %>%
  mutate(anycansymp = ifelse(cough == 1 | appetite_loss == 1 | low_hb == 1 | raise_platelets == 1| raise_wbc == 1| fatigue == 1 | weight_loss == 1, 1, 0)) %>%
  mutate(anynonneo = ifelse(hernia_abdo_bin == 1 | barretts_bin == 1 | gord_bin == 1 | gastritis_duodenitis_bin == 1 | oesoph_ulc_bin == 1, 1,0))

#combining the ex-drinker variable with the drinker variable
data <- data %>%
  mutate(alc_status_comb = ifelse(alc_status_comb == 5,3,#if drinker with alc problems then join with ex drinker with problems
                                  ifelse(alc_status_comb == 4,2,#if drinker no alc problems merge with ex drinker no alc problems
                                         alc_status_comb)))

#-----------------------------------
data[sapply(data,is.numeric)] <- lapply(data[sapply(data,is.numeric)],as.factor)
data[sapply(data,is.character)] <- lapply(data[sapply(data,is.character)],as.factor)

#labelling the variables
label(data$gender) <- "Gender"
label(data$age_band) <- "Age group"
label(data$final_smoking_status) <- "Smoking status"
label(data$elix_band) <- "Elixhauser score"
label(data$bmi_cat) <- "BMI Category"
label(data$abd_pain) <- "Upper abdominal pain"
label(data$dyspepsia) <- "Dyspepsia"
label(data$dysphagia) <- "Dysphagia"
label(data$alc_status_comb) <- "Alcohol status"
label(data$vomiting) <- "Vomiting"
label(data$fatigue) <- "Fatigue"
label(data$cough) <- "Cough"
label(data$low_hb) <- "Anaemia"
label(data$raise_platelets) <- "Raised platelets"
label(data$raise_wbc) <- "Raised WBCs"
label(data$h_pylori_flag) <- "H.pylori treatment"
label(data$weight_loss) <- "Weight loss"
label(data$appetite_loss) <- "Appetite loss"
label(data$hernia_abdo_bin) <- "Diaphragmatic hernia"
label(data$barretts_bin) <- "Barretts"
label(data$gastritis_duodenitis_bin) <- "Gastritis duodenitis"
label(data$gord_bin) <- "GORD"
label(data$oesoph_ulc_bin) <- "Oesophageal ulcer"
label(data$anyOGsymp) <- "Any OG cancer symptom"
label(data$anycansymp) <- "Any general/alarm cancer symptom"
label(data$anynonneo) <- "Any non-neoplastic disease of interest"

#setting names
levels(data$gender)[levels(data$gender) == "1"] <- "Male"
levels(data$gender)[levels(data$gender) == "2"] <- "Female"

levels(data$age_band)[levels(data$age_band) == "30,40"] <- "30-39"
levels(data$age_band)[levels(data$age_band) == "40,50"] <- "40-49"
levels(data$age_band)[levels(data$age_band) == "50,60"] <- "50-59"
levels(data$age_band)[levels(data$age_band) == "60,70"] <- "60-69"
levels(data$age_band)[levels(data$age_band) == "70,80"] <- "70-79"

levels(data$final_smoking_status)[levels(data$final_smoking_status) == "1"] <- "Never smoker"
levels(data$final_smoking_status)[levels(data$final_smoking_status) == "2"] <- "Ex-smoker"
levels(data$final_smoking_status)[levels(data$final_smoking_status) == "4"] <- "Current smoker"

levels(data$elix_band)[levels(data$elix_band) == "[0,1]"] <- "0"
levels(data$elix_band)[levels(data$elix_band) == "(1,3]"] <- "1-2"
levels(data$elix_band)[levels(data$elix_band) == "(3,5]"] <- "3-4"
levels(data$elix_band)[levels(data$elix_band) == "(5,27]"] <- "5+"

levels(data$bmi_cat)[levels(data$bmi_cat) == 0] <- "Healthy weight"
levels(data$bmi_cat)[levels(data$bmi_cat) == 1] <- "Underweight"
levels(data$bmi_cat)[levels(data$bmi_cat) == 2] <- "Overweight"
levels(data$bmi_cat)[levels(data$bmi_cat) == 3] <- "Obese"

levels(data$dyspepsia)[levels(data$dyspepsia) == "0"] <- "Absent"
levels(data$dyspepsia)[levels(data$dyspepsia) == "1"] <- "Dyspepsia"

levels(data$dysphagia)[levels(data$dysphagia) == "0"] <- "Absent"
levels(data$dysphagia)[levels(data$dysphagia) == "1"] <- "Dysphagia"

levels(data$vomiting)[levels(data$vomiting) == "0"] <- "Absent"
levels(data$vomiting)[levels(data$vomiting) == "1"] <- "Vomiting"

levels(data$alc_status_comb)[levels(data$alc_status_comb) == "1"] <- "Non-drinker"
levels(data$alc_status_comb)[levels(data$alc_status_comb) == "2"] <- "Ever drinker"
levels(data$alc_status_comb)[levels(data$alc_status_comb) == "3"] <- "Ever drinker with alc problems"

levels(data$abd_pain)[levels(data$abd_pain) == "0"] <- "Absent"
levels(data$abd_pain)[levels(data$abd_pain) == "1"] <- "Upper abdominal pain"

levels(data$weight_loss)[levels(data$weight_loss) == "0"] <- "Absent"
levels(data$weight_loss)[levels(data$weight_loss) == "1"] <- "Weight loss"

levels(data$appetite_loss)[levels(data$appetite_loss) == "0"] <- "Absent"
levels(data$appetite_loss)[levels(data$appetite_loss) == "1"] <- "Appetite loss"

levels(data$low_hb)[levels(data$low_hb) == "0"] <- "Absent"
levels(data$low_hb)[levels(data$low_hb) == "1"] <- "Anaemia"

levels(data$raise_platelets)[levels(data$raise_platelets) == "0"] <- "Absent"
levels(data$raise_platelets)[levels(data$raise_platelets) == "1"] <- "Raised platelets"

levels(data$raise_wbc)[levels(data$raise_wbc) == "0"] <- "Absent"
levels(data$raise_wbc)[levels(data$raise_wbc) == "1"] <- "Raised WBCs"

levels(data$h_pylori_flag)[levels(data$h_pylori_flag) == "0"] <- "Absent"
levels(data$h_pylori_flag)[levels(data$h_pylori_flag) == "1"] <- "H.pylori treatment"

levels(data$fatigue)[levels(data$fatigue) == "0"] <- "Absent"
levels(data$fatigue)[levels(data$fatigue) == "1"] <- "Fatigue"

levels(data$cough)[levels(data$cough) == "0"] <- "Absent"
levels(data$cough)[levels(data$cough) == "1"] <- "Cough"

levels(data$gord_bin)[levels(data$gord_bin) == "0"] <- "Absent"
levels(data$gord_bin)[levels(data$gord_bin) == "1"] <- "GORD"

levels(data$oesoph_ulc_bin)[levels(data$oesoph_ulc_bin) == "0"] <- "Absent"
levels(data$oesoph_ulc_bin)[levels(data$oesoph_ulc_bin) == "1"] <- "Oeosphageal ulcer"

levels(data$barretts_bin)[levels(data$barretts_bin) == "0"] <- "Absent"
levels(data$barretts_bin)[levels(data$barretts_bin) == "1"] <- "Barret's oesophagus"

levels(data$hernia_abdo_bin)[levels(data$hernia_abdo_bin) == "0"] <- "Absent"
levels(data$hernia_abdo_bin)[levels(data$hernia_abdo_bin) == "1"] <- "Abdominal hernia"

levels(data$gastritis_duodenitis_bin)[levels(data$gastritis_duodenitis_bin) == "0"] <- "Absent"
levels(data$gastritis_duodenitis_bin)[levels(data$gastritis_duodenitis_bin) == "1"] <- "Gastritis & duodenitis"

levels(data$anyOGsymp)[levels(data$anyOGsymp) == "0"] <- "Absent"
levels(data$anyOGsymp)[levels(data$anyOGsymp) == "1"] <- "Any OG cancer symptom"

levels(data$anycansymp)[levels(data$anycansymp) == "0"] <- "Absent"
levels(data$anycansymp)[levels(data$anycansymp) == "1"] <- "Any general/alarm cancer symptom"

levels(data$anynonneo)[levels(data$anynonneo) == "0"] <- "Absent"
levels(data$anynonneo)[levels(data$anynonneo) == "1"] <- "Any non-neoplastic disease of interest"

#table 1 creation
table1(~gender + age_band 
       + final_smoking_status 
       + elix_band 
       +bmi_cat
       +alc_status_comb+ 
         h_pylori_flag+anyOGsymp  + abd_pain + dyspepsia + dysphagia + vomiting
              + anycansymp + low_hb + raise_platelets + raise_wbc + appetite_loss + cough + fatigue + weight_loss
              + anynonneo + hernia_abdo_bin + barretts_bin + gord_bin + gastritis_duodenitis_bin + oesoph_ulc_bin 
         | category
          ,
       overall=FALSE,
       data=data)


inv <- data %>% filter(age_band == "20,30")


# Kill all connections
killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    + dbDisconnect(con)
}

killDbConnections()

