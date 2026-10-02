rm(list=ls())

#### Loading Packages ####
library(pacman)
# If it asks "install from sources that need compilation?" Hit "no"
pacman::p_load(tidyverse,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI, ggplot2, RColorBrewer,xlsx,purrr)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

##Repeating the same code for each symptom group, tried to write into a function but gets complicated as they don't have the same column names
#where there isn't a pairwise column for the same name

#Specifying the function
format_foo <- function(data,cohort_var){

#making variable to count the number of different other symptoms
data$symp_count <- rowSums(data[,6:16])

#creating a banded comorbidity status variable
data <- data %>%
  mutate(elix_band = cut(data$unw_score,breaks = c(0,1,3,5,25),include.lowest=TRUE))


#filtering out those in the rand sample from relevant cohort
data <- data %>% anti_join(rand,by=c("e_patid","eventdate"))

#currently non neoplastic variables are count variables not binary, need to change
data <- data %>%
  mutate(oesoph_ulc_bin = ifelse(oesoph_ulc == 0,0,1)) %>%
  mutate(gord_bin = ifelse(gord == 0,0,1)) %>%
  mutate(barretts_bin = ifelse(barretts == 0,0,1)) %>%
  mutate(gastritis_duodenitis_bin = ifelse(gastritis_duodenitis == 0,0,1)) %>%
  mutate(hernia_abdo_bin = ifelse(hernia_abdo == 0,0,1))

data$gender <- as.factor(data$gender)
data$age_band <- as.factor(data$age_band)

#combining the ex-drinker variable with the drinker variable
data <- data %>%
  mutate(alc_status_comb = ifelse(alc_status_comb == 5,3,#if drinker with alc problems then join with ex drinker with problems
                                  ifelse(alc_status_comb == 4,2,#if drinker no alc problems merge with ex drinker no alc problems
                                         alc_status_comb))) %>%
  mutate(alc_status_comb = as.factor(alc_status_comb))

#-----------------------------------------------------------------------------------------
#Overall cancer risk for all patients with a symptom
data_out_all <- data
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

agg_join_age <- data %>%
  replace(is.na(.),0) %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper) %>%
  mutate(Category = age_band) 

#by gender
data <- data_out_all %>%
  group_by(gender,og_cancer,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

agg_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper) %>%
  mutate(Category = NA)%>%
  mutate(age_band =NA)

#by smoking status
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,final_smoking_status,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,final_smoking_status,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

smok_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(smok_join)[4] <- "Category"
smok_join$Category <- factor(smok_join$Category)
levels(smok_join$Category) <- c("Never smoker","Ex-smoker","Current smoker")

#by comorbidity status

#creating a banded comorbidity status variable
data_out_all <- data_out_all %>%
  mutate(elix_band = cut(unw_score,breaks = c(0,1,3,5,25),include.lowest=TRUE))

data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,elix_band,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,elix_band,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

elix_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(elix_join)[4] <- "Category"
elix_join$Category <- factor(elix_join$Category)
levels(elix_join$Category) <- c("0 comorbidities","1 comorbidity","2 comorbidities","3 comorbidities","4 comorbidities","5+ comorbidities")

#### All other symptoms ####

#low hb
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,low_hb,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,low_hb,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

low_hb_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(low_hb_join)[4] <- "Category"
low_hb_join$Category <- factor(low_hb_join$Category)
levels(low_hb_join$Category) <- c("No low_hb","low_hb")

#weight loss
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,weight_loss,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,weight_loss,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

weight_loss_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(weight_loss_join)[4] <- "Category"
weight_loss_join$Category <- factor(weight_loss_join$Category)
levels(weight_loss_join$Category) <- c("No weight_loss","weight_loss")

#cough
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,cough,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,cough,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

cough_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(cough_join)[4] <- "Category"
cough_join$Category <- factor(cough_join$Category)
levels(cough_join$Category) <- c("No cough","cough")

#appetite loss
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,appetite_loss,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,appetite_loss,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

appetite_loss_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(appetite_loss_join)[4] <- "Category"
appetite_loss_join$Category <- factor(appetite_loss_join$Category)
levels(appetite_loss_join$Category) <- c("No appetite_loss","appetite_loss")

#fatigue
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,fatigue,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,fatigue,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

fatigue_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(fatigue_join)[4] <- "Category"
fatigue_join$Category <- factor(fatigue_join$Category)
levels(fatigue_join$Category) <- c("No fatigue","fatigue")

#dyspepsia
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,dyspepsia,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,dyspepsia,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

dyspepsia_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(dyspepsia_join)[4] <- "Category"
dyspepsia_join$Category <- factor(dyspepsia_join$Category)
levels(dyspepsia_join$Category) <- c("No dyspepsia","dyspepsia")

#dysphagia
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,dysphagia,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,dysphagia,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

dysphagia_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(dysphagia_join)[4] <- "Category"
dysphagia_join$Category <- factor(dysphagia_join$Category)
levels(dysphagia_join$Category) <- c("No dysphagia","dysphagia")

#ever_obesity
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,bmi_cat,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,bmi_cat,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

ever_obesity_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(ever_obesity_join)[4] <- "Category"
ever_obesity_join$Category <- factor(ever_obesity_join$Category)
levels(ever_obesity_join$Category) <- c("Healthy weight","Underweight","Overweight","Obese")

#vomiting
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,vomiting,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,vomiting,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

vomiting_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(vomiting_join)[4] <- "Category"
vomiting_join$Category <- factor(vomiting_join$Category)
levels(vomiting_join$Category) <- c("No vomiting","vomiting")

#ever_alcohol_problems
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,alc_status_comb,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,alc_status_comb,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

ever_alcohol_problems_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(ever_alcohol_problems_join)[4] <- "Category"
ever_alcohol_problems_join$Category <- factor(ever_alcohol_problems_join$Category)
levels(ever_alcohol_problems_join$Category) <- c("Non-drinker","Ever drinker","Ever drinker with alc problems")

#abd_pain
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,abd_pain,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,abd_pain,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

abd_pain_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(abd_pain_join)[4] <- "Category"
abd_pain_join$Category <- factor(abd_pain_join$Category)
levels(abd_pain_join$Category) <- c("No abdominal pain","Abdominal pain")

#platelets
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,raise_platelets,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,raise_platelets,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

raise_platelets_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(raise_platelets_join)[4] <- "Category"
raise_platelets_join$Category <- factor(raise_platelets_join$Category)
levels(raise_platelets_join$Category) <- c("No raise_platelets_join","raise_platelets_join")

#wbcs
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,raise_wbc,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,raise_wbc,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

raise_wbc_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(raise_wbc_join)[4] <- "Category"
raise_wbc_join$Category <- factor(raise_wbc_join$Category)
levels(raise_wbc_join$Category) <- c("No raise_wbc","raise_wbc")

#### All non-neo diseases ####

#barretts
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,barretts_bin,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,barretts_bin,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

barretts_bin_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(barretts_bin_join)[4] <- "Category"
barretts_bin_join$Category <- factor(barretts_bin_join$Category)
levels(barretts_bin_join$Category) <- c("No barretts","barretts")

#hernia_abdo
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,hernia_abdo_bin,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,hernia_abdo_bin,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

hernia_abdo_bin_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(hernia_abdo_bin_join)[4] <- "Category"
hernia_abdo_bin_join$Category <- factor(hernia_abdo_bin_join$Category)
levels(hernia_abdo_bin_join$Category) <- c("No hernia abdo","hernia abdo")

#gord
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,gord_bin,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,gord_bin,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

gord_bin_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(gord_bin_join)[4] <- "Category"
gord_bin_join$Category <- factor(gord_bin_join$Category)
levels(gord_bin_join$Category) <- c("No gord","gord")

#gastritis_duodenitis
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,gastritis_duodenitis_bin,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,gastritis_duodenitis_bin,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

gastritis_duodenitis_bin_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(gastritis_duodenitis_bin_join)[4] <- "Category"
gastritis_duodenitis_bin_join$Category <- factor(gastritis_duodenitis_bin_join$Category)
levels(gastritis_duodenitis_bin_join$Category) <- c("No gastritis duodenitis","gastritis duodenitis")

#oesoph_ulc
data <- data_out_all %>%
  group_by(gender,age_band,og_cancer,oesoph_ulc_bin,.drop=FALSE) %>%
  count(og_cancer,.drop=FALSE) %>%
  ungroup() 

data2 <- data %>%
  group_by(gender,age_band,oesoph_ulc_bin,.drop=FALSE) %>%
  summarise(freq = sum(n))

data <- data %>% filter(og_cancer == 1) %>%
  right_join(data2)

oesoph_ulc_bin_join <- data %>%
  replace(is.na(.),0) %>%   mutate(prop = binom.confint(n,freq,conf.level=0.95,method='wilson')$mean) %>%
  mutate(lower = binom.confint(n,freq,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(n,freq,conf.level=0.95,method='wilson')$upper)

names(oesoph_ulc_bin_join)[4] <- "Category"
oesoph_ulc_bin_join$Category <- factor(oesoph_ulc_bin_join$Category)
levels(oesoph_ulc_bin_join$Category) <- c("No oesophageal ulcer","oesophageal ulcer")

#union all tables together
all <- union(agg_join,smok_join)
all <- union(all,agg_join_age)
all <- union(all,elix_join)
all <- union(all,dyspepsia_join)
all <- union(all,low_hb_join)
all <- union(all,raise_platelets_join)
all <- union(all,raise_wbc_join)
all <- union(all,weight_loss_join)
all <- union(all,dysphagia_join)
all <- union(all,ever_obesity_join)
all <- union(all,vomiting_join)
all <- union(all,abd_pain_join)
all <- union(all,ever_alcohol_problems_join)
all <- union(all,fatigue_join)
all <- union(all,cough_join)
all <- union(all,appetite_loss_join)
all <- union(all,oesoph_ulc_bin_join)
all <- union(all,gord_bin_join)
all <- union(all,barretts_bin_join)
all <- union(all,hernia_abdo_bin_join)
all <- union(all,gastritis_duodenitis_bin_join)

return(all)
}

#----------------------------------------
#Random sample
query = 'SELECT * FROM freya.random_sample_final_v2;'
rs = dbSendQuery(db, query)
rand = fetch(rs, n=-1)

## Loading data for each cohort and running format function
query <- 'SELECT * FROM cohort_abd_pain_final;'
rs <- dbSendQuery(db, query)
abd_pain <- fetch(rs, n=-1)

abd_pain <- abd_pain %>% select(-contains('same_day')) 

abd_pain_var <- "abd_pain"
all_abd_pain <- format_foo(abd_pain,abd_pain_var)

#--
query <- 'SELECT * FROM cohort_vomiting_final;'
rs <- dbSendQuery(db, query)
vomiting <- fetch(rs, n=-1)

vomiting <- vomiting %>% select(-contains('same_day'))

vomiting_var <- "vomiting"
all_vomiting <- format_foo(vomiting,vomiting_var)

#--
query <- 'SELECT * FROM cohort_dyspepsia_final;'
rs <- dbSendQuery(db, query)
dyspepsia <- fetch(rs, n=-1)

dyspepsia <- dyspepsia %>% select(-contains('same_day')) 

dyspepsia_var <- "dyspepsia"
all_dyspepsia <- format_foo(dyspepsia,dyspepsia_var)

#--
query <- 'SELECT * FROM cohort_dysphagia_final;'
rs <- dbSendQuery(db, query)
dysphagia <- fetch(rs, n=-1)

dysphagia <- dysphagia %>% select(-contains('same_day')) 

dysphagia_var <- "dysphagia"
all_dysphagia <- format_foo(dysphagia,dysphagia_var)

#------------------------------------------------------
## joining all symptom tables together

#male and female split

split_foo <- function(data){
  data_overall <- data[1:2,]

data <- data %>%
  group_by(gender,age_band,.drop=FALSE) %>%
  arrange(age_band)

data <- data %>% filter(!grepl("no",Category,ignore.case = T))

all_data <- rbind(data_overall,data)

all_data_f <- all_data %>% filter(gender == 2)
all_data_m <- all_data %>% filter(gender == 1)
all_data <- list(all_data_f,all_data_m)
names(all_data) <- c("Women","Men")

return(all_data)
}

#running function on each dataframe
all_abd_pain_split <- split_foo(all_abd_pain)

all_dysphagia_split <- split_foo(all_dysphagia)

all_dyspepsia_split <- split_foo(all_dyspepsia)

all_vomiting_split <- split_foo(all_vomiting)

#joining together the sex specific data
vomiting_xtra <- as.data.frame(rbind(as.data.frame(all_vomiting_split[1]),c(NA,NA,NA,NA,NA,NA,NA,NA,NA)))
dysphagia_xtra <- as.data.frame(rbind(as.data.frame(all_dysphagia_split[1]),c(NA,NA,NA,NA,NA,NA,NA,NA,NA)))

all_all_f <- cbind(as.data.frame(all_abd_pain_split[1]),
                   all_dysphagia_split[1],
                   as.data.frame(all_dyspepsia_split[1]),
                   all_vomiting_split[1]
                   )

all_all_f_fil <- cbind(all_all_f[,5],all_all_f[,8:9],all_all_f[,14],all_all_f[,23],all_all_f[,32])

all_all_f_fil <- all_all_f_fil %>%
  distinct() %>%
  rename(og_dyspepsia = `all_all_f[, 23]`) %>%
  rename(og_dysphagia = `all_all_f[, 14]`) %>%
  rename(og_abd_pain = `all_all_f[, 5]`) %>%
  rename(og_vomiting = `all_all_f[, 32]`) %>%
  rename(Category = Women.Category) %>%
  rename(age_band = Women.age_band) %>%
  select(Category, age_band, og_dyspepsia,og_dysphagia,og_abd_pain,og_vomiting)
#-------------------
dyspepsia_xtra <- as.data.frame(rbind(as.data.frame(all_dyspepsia_split[2]),c(NA,NA,NA,NA,NA,NA,NA,NA,NA)))

all_all_m <- cbind(as.data.frame(all_abd_pain_split[2]),as.data.frame(all_dysphagia_split[2]),as.data.frame(all_dyspepsia_split[2]),as.data.frame(all_vomiting_split[2]))#dyspepsia_xtra

all_all_m_fil <- cbind(all_all_m[,5],all_all_m[,8:9],all_all_m[,14],all_all_m[,23],all_all_m[,32])

all_all_m_fil <- all_all_m_fil %>%
  distinct() %>%
  rename(og_dyspepsia = `all_all_m[, 23]`) %>%
  rename(og_dysphagia = `all_all_m[, 14]`) %>%
  rename(og_abd_pain = `all_all_m[, 5]`) %>%
  rename(og_vomiting = `all_all_m[, 32]`) %>%
  rename(Category = Men.Category) %>%
  rename(age_band = Men.age_band) %>%
  select(Category, age_band, og_dyspepsia,og_dysphagia,og_abd_pain,og_vomiting)

#extracting the messy datafarme of everything
all_all_f_fil_2 <- cbind(all_all_f[,3:9],all_all_f[,12:18],all_all_f[,21:27],all_all_f[,30:36])
all_all_m_fil_2 <- cbind(all_all_m[,3:9],all_all_m[,12:18],all_all_m[,21:27],all_all_m[,30:36])

xlsx::write.xlsx(all_all_f_fil_2,file = ".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\All female table v2.xlsx",sheetName = "All OG risk full",append=TRUE)
xlsx::write.xlsx(all_all_m_fil_2,file = ".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\All male table v2.xlsx",sheetName = "All OG risk full",append=TRUE)

#extracting just risk
xlsx::write.xlsx(all_all_f_fil,file = ".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\All female table v2.xlsx",sheetName = "All OG risk limited",append=TRUE)
xlsx::write.xlsx(all_all_m_fil,file = ".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\All male table v2.xlsx",sheetName = "All OG risk limited",append=TRUE)

#--------------------------------------------------
#extracting certain field to plot
women_select <- cbind(all_all_f_fil_2[,6:7],all_all_f_fil_2[2:5])

women_select2 <- cbind(all_all_f_fil_2[,6:7],all_all_f_fil_2[9:12])

women_select3 <- cbind(all_all_f_fil_2[,6:7],all_all_f_fil_2[16:19])

women_select4 <- cbind(all_all_f_fil_2[,6:7],all_all_f_fil_2[23:26])

women_select <- women_select %>%
  mutate(Cohort = "Dyspepsia")
  
women_select2 <- women_select2 %>%
  mutate(Cohort = "Dysphagia")

women_select3 <- women_select3 %>%
  mutate(Cohort = "Upper abdominal pain")

women_select4 <- women_select4 %>%
  mutate(Cohort = "Vomiting")

women_all <- women_select %>%
  union(women_select2) %>%
  union(women_select3) %>%
  union(women_select4) %>%
  mutate(Sex = "Women")
  
names(women_all) <- c("Category","age_band","sample_size",
                           "prop","lower","upper","Cohort","Sex")

men_select <- cbind(all_all_m_fil_2[,6:7],all_all_m_fil_2[2:5])

men_select2 <- cbind(all_all_m_fil_2[,6:7],all_all_m_fil_2[9:12])

men_select3 <- cbind(all_all_m_fil_2[,6:7],all_all_m_fil_2[16:19])

men_select4 <- cbind(all_all_m_fil_2[,6:7],all_all_m_fil_2[23:26])

men_select <- men_select %>%
  mutate(Cohort = "Dyspepsia")

men_select2 <- men_select2 %>%
  mutate(Cohort = "Dysphagia")

men_select3 <- men_select3 %>%
  mutate(Cohort = "Upper abdominal pain")

men_select4 <- men_select4 %>%
  mutate(Cohort = "Vomiting")

men_all <- men_select %>%
  union(men_select2) %>%
  union(men_select3) %>%
  union(men_select4) %>%
  mutate(Sex = "Men")

names(men_all) <- c("Category","age_band","sample_size",
                      "prop","lower","upper","Cohort","Sex")


sex_union <- union(women_all,men_all)

#filtering out values with 0% proportion
sex_union <- sex_union %>%
  mutate(Pairwise_combination = paste0(Cohort," & ",Category)) %>%
  filter(sample_size > 100)

sex_union2 <- sex_union %>%
  group_by(age_band,Pairwise_combination) %>%
  count(Pairwise_combination)

sex_union <- sex_union %>%
  left_join(sex_union2, by=c("age_band","Pairwise_combination")) %>%
  filter(n > 1) %>%
  select(-n)

#creating a version averaging across all ages
sex_union_avg <- sex_union %>%
  mutate(n = prop*sample_size) %>%
  group_by(Pairwise_combination,Cohort,Sex) %>%
  select(Pairwise_combination,Cohort,Sex,sample_size,n) %>%
  mutate(sample_size =as.numeric(sample_size))%>%
  summarise(sum_sample_size = sum(sample_size),
            sum_n = sum(n),
            .groups='drop') %>%
  ungroup()

sex_union_avg <- sex_union_avg %>%
  mutate(prop = binom.confint(x=sum_n,n=sum_sample_size,conf.level=0.95,methods='wilson')$mean) %>%
  mutate(lower = binom.confint(sum_n,sum_sample_size,conf.level=0.95,method='wilson')$lower) %>%
  mutate(upper = binom.confint(sum_n,sum_sample_size,conf.level=0.95,method='wilson')$upper) %>%
  filter(!grepl("40",Pairwise_combination),
         !grepl("60",Pairwise_combination),
         !grepl("80",Pairwise_combination),
         !grepl("NA",Pairwise_combination),
         !grepl("appetite_loss",Pairwise_combination)) %>%
  mutate(across('Pairwise_combination',str_replace,'hernia abdo','diaphragmatic hernia')) %>%
  mutate(across('Pairwise_combination',str_replace,'abd pain','upper abdominal pain')) %>%
  mutate(across('Pairwise_combination',str_replace,'0 comorbidities','Elixhauser score 0')) %>%
  mutate(across('Pairwise_combination',str_replace,'1 comorbidity','Elixhauser score 1-2')) %>%
  mutate(across('Pairwise_combination',str_replace,'2 comorbidities','Elixhauser score 3-4')) %>%
  mutate(across('Pairwise_combination',str_replace,'3 comorbidities','Elixhauser score 5+')) %>%
  mutate(across('Pairwise_combination',str_replace,'raise_platelets_join','raised platelets'))%>%
  mutate(across('Pairwise_combination',str_replace,'raise_wbc','raised WBCs'))%>%
  mutate(across('Pairwise_combination',str_replace,'weight_loss','weight loss'))%>%
  mutate(across('Pairwise_combination',str_replace,'low_hb','anaemia'))%>%
  mutate(across('Pairwise_combination',str_replace,'Underweight','underweight'))%>%
  mutate(across('Pairwise_combination',str_replace,'Overweight','overweight'))%>%
  mutate(across('Pairwise_combination',str_replace,'Obese','obese'))%>%
  mutate(across('Pairwise_combination',str_replace,'Healthy weight','healthy weight'))%>%
  mutate(across('Pairwise_combination',str_replace,'Abdominal pain','upper abdominal pain'))%>%
  mutate(across('Pairwise_combination',str_replace,'Never smoker','never smoker'))%>%
  mutate(across('Pairwise_combination',str_replace,'Ex-smoker','ex-smoker'))%>%
  mutate(across('Pairwise_combination',str_replace,'Current smoker','current smoker'))%>%
  mutate(across('Pairwise_combination',str_replace,'Ever drinker','ever drinker'))
  
#----------------------------------------------------------------------------------------------------
colour_vec <- c("firebrick","royalblue4") #"black",

#one overall plot for all ages
ggplot()+
  geom_point(sex_union_avg,mapping=aes(x=prop*100,y=Pairwise_combination,colour=Sex),alpha=0.75,position=position_dodge(width=1))+
  #geom_point(out,mapping=aes(x=`Co-occurring feature`,y=Odds,group=Cohort))+
  geom_errorbar(sex_union_avg,mapping=aes(xmin=lower*100,xmax=upper*100,y=Pairwise_combination,colour=Sex),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  #theme(axis.text.x = element_text(angle=75,vjust=1,hjust=1))+
  scale_colour_manual(values=colour_vec)+
  theme_bw() +
  xlab("Risk of OG cancer (%)")+
  geom_hline(yintercept=seq(1.5,106.5,1),linetype='dashed',alpha=0.4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,14.2))

ggsave("S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Pairwise risk men women comparison.png",width = 9,height=13,dpi=200)

#splitting the data into age groups
ggplot()+
  geom_point(sex_union %>% filter(age_band == "30,40"),mapping=aes(x=prop*100,y=Pairwise_combination,colour=Sex),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(sex_union %>% filter(age_band == "30,40"),mapping=aes(xmin=lower*100,xmax=upper*100,y=Pairwise_combination,colour=Sex),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec)+
  theme_bw() +
  xlab("Risk of OG cancer (%)")

ggplot()+
  geom_point(sex_union %>% filter(age_band == "40,50"),mapping=aes(x=prop*100,y=Pairwise_combination,colour=Sex),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(sex_union %>% filter(age_band == "40,50"),mapping=aes(xmin=lower*100,xmax=upper*100,y=Pairwise_combination,colour=Sex),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec)+
  theme_bw() +
  xlab("Risk of OG cancer (%)")+
  geom_hline(yintercept=seq(1.5,89.5,1),linetype='dashed',alpha=0.6)+
  scale_x_continuous(expand=c(0,0),limits=c(0,10))
  
ggsave("S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Pairwise risk men women comparison 40,50.png",width = 9,height=13,dpi=200)


ggplot()+
  geom_point(sex_union %>% filter(age_band == "50,60"),mapping=aes(x=prop*100,y=Pairwise_combination,colour=Sex),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(sex_union %>% filter(age_band == "50,60"),mapping=aes(xmin=lower*100,xmax=upper*100,y=Pairwise_combination,colour=Sex),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec)+
  theme_bw() +
  xlab("Risk of OG cancer (%)")+
  geom_hline(yintercept=seq(1.5,95.5,1),linetype='dashed',alpha=0.6)

ggsave("S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Pairwise risk men women comparison 50,60.png",width = 9,height=13,dpi=200)

ggplot()+
  geom_point(sex_union %>% filter(age_band == "60,70"),mapping=aes(x=prop*100,y=Pairwise_combination,colour=Sex),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(sex_union %>% filter(age_band == "60,70"),mapping=aes(xmin=lower*100,xmax=upper*100,y=Pairwise_combination,colour=Sex),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec)+
  theme_bw() +
  xlab("Risk of OG cancer (%)")+
  geom_hline(yintercept=seq(1.5,107.5,1),linetype='dashed',alpha=0.6)

ggsave("S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Pairwise risk men women comparison 60,70.png",width = 9,height=13,dpi=200)

ggplot()+
  geom_point(sex_union %>% filter(age_band == "70,80"),mapping=aes(x=prop*100,y=Pairwise_combination,colour=Sex),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(sex_union %>% filter(age_band == "70,80"),mapping=aes(xmin=lower*100,xmax=upper*100,y=Pairwise_combination,colour=Sex),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec)+
  theme_bw() +
  xlab("Risk of OG cancer (%)")+
  geom_hline(yintercept=seq(1.5,113.5,1),linetype='dashed',alpha=0.6)

ggsave("S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Pairwise risk men women comparison 70,80.png",width = 9,height=13,dpi=200)

ggplot()+
  geom_point(sex_union %>% filter(age_band == "80+"),mapping=aes(x=prop*100,y=Pairwise_combination,colour=Sex),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(sex_union %>% filter(age_band == "80+"),mapping=aes(xmin=lower*100,xmax=upper*100,y=Pairwise_combination,colour=Sex),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec)+
  theme_bw() +
  xlab("Risk of OG cancer (%)")+
  geom_hline(yintercept=seq(1.5,113.5,1),linetype='dashed',alpha=0.6)

ggsave("S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Pairwise risk men women comparison 80+.png",width = 9,height=13,dpi=200)

#----------------------------------------------------------------------------------
# Finding the youngest age band for each combination were the risk exceeds 3%

all_all_f_fil <- cbind(all_all_f[,1:4],all_all_f[,8],all_all_f[,12],all_all_f[,16])

all_all_f_fil <- all_all_f_fil %>%
  rename(og_abd_pain = og_cancer) %>%
  rename(og_dysphagia = `all_all_f[, 8]`) %>%
  rename(og_dyspepsia = `all_all_f[, 12]`) %>%
  rename(og_vomiting = `all_all_f[, 16]`)

all_all_m_fil <- cbind(all_all_m[,1:4],all_all_m[,8],all_all_m[,12],all_all_m[,16])

all_all_m_fil <- all_all_m_fil %>%
  rename(og_abd_pain = og_cancer) %>%
  rename(og_dysphagia = `all_all_m[, 8]`) %>%
  rename(og_dyspepsia = `all_all_m[, 12]`) %>%
  rename(og_vomiting = `all_all_m[, 16]`)


all_all_f_fil_t <- all_all_f_fil %>%
  mutate(og_dyspepsia = ifelse(og_dyspepsia > 0.03, og_dyspepsia, NA)) %>%
  mutate(og_dysphagia = ifelse(og_dysphagia > 0.03, og_dysphagia, NA)) %>%
  mutate(og_abd_pain = ifelse(og_abd_pain > 0.03, og_abd_pain, NA)) %>%
  mutate(og_vomiting = ifelse(og_vomiting > 0.03, og_vomiting, NA)) 

all_all_m_fil_t <- all_all_m_fil %>%
  mutate(og_dyspepsia = ifelse(og_dyspepsia > 0.03, og_dyspepsia, NA)) %>%
  mutate(og_dysphagia = ifelse(og_dysphagia > 0.03, og_dysphagia, NA)) %>%
  mutate(og_abd_pain = ifelse(og_abd_pain > 0.03, og_abd_pain, NA)) %>%
  mutate(og_vomiting = ifelse(og_vomiting > 0.03, og_vomiting, NA)) 

xlsx::write.xlsx(all_all_f_fil_t,file = ".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\All female table v2.xlsx",sheetName = "Threshold breach tablev",append=TRUE)
xlsx::write.xlsx(all_all_m_fil_t,file = ".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\All male table v2.xlsx",sheetName = "Threshold breach tablev",append=TRUE)

