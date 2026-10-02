rm(list=ls())

#### Loading Packages ####
library(pacman)

pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, RMySQL, DBI, ggplot2, RColorBrewer, xlsx)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

###Repeating the same code for each disease####

format_foo_non_neo <- function(data,cohort_var,name_var){
  data$age_band <- ifelse(data$age_band == '80,90'|data$age_band == '90,100','80+',data$age_band)

data$x <- 0
names(data)[43] <- cohort_var

#changing gender to 0 and 1
data$gender <- ifelse(data$gender == 2,1,0)

#filtering out those in the rand sample from relevant cohort
data <- data %>% mutate(eventdate = as.Date(eventdate))%>%anti_join(rand,by=c("e_patid","eventdate"))

#combining the ex-drinker variable with the drinker variable
data <- data %>%
  mutate(alc_status_comb = ifelse(alc_status_comb == 5,3,#if drinker with alc problems then join with ex drinker with problems
                                  ifelse(alc_status_comb == 4,2,#if drinker no alc problems merge with ex drinker no alc problems
                                         alc_status_comb))) %>%
  mutate(alc_status_comb = as.factor(alc_status_comb)) %>%
  select(-data_source)

uni2 <- data[,3:42]
data_out <- uni2 %>% select(-c(age,og_cancer))
data_age <- uni2 %>% select(c(age,og_cancer))

data_out[sapply(data_out,is.numeric)] <- lapply(data_out[sapply(data_out,is.numeric)],as.factor)
data_out$unw_score <- as.numeric(data_out$unw_score)

#currently they are count variables not binary, need to change
data_out <- data_out %>%
  mutate(gord_bin = as.factor(ifelse(gord == 0,0,1))) %>%
  mutate(barretts_bin = as.factor(ifelse(barretts == 0,0,1))) %>%
  mutate(gastritis_duodenitis_bin = as.factor(ifelse(gastritis_duodenitis == 0,0,1))) %>%
  mutate(hernia_abdo_bin = as.factor(ifelse(hernia_abdo == 0,0,1)))%>%
  mutate(oesoph_ulc_bin = as.factor(ifelse(oesoph_ulc == 0,0,1)))

levels(data_out$gord_bin) <- c(0,1)
levels(data_out$barretts_bin) <- c(0,1)
levels(data_out$gastritis_duodenitis_bin) <- c(0,1)
levels(data_out$hernia_abdo_bin) <- c(0,1)
levels(data_out$oesoph_ulc_bin) <- c(0,1)

data_out_all <- cbind(data_out,data_age)

#-----------------------------------------------------------------------------------------
#Overall cancer risk for all patients with a oesoph_ulc symptom

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

# #filling in the blanks for the non neo variable of interest
age_band <- c('30,40','40,50','50,60','60,70','70,80','80+')

rows <- as.data.frame(rbind(cbind(rep(0,length(age_band)),rep(0,length(age_band)),rep(0,length(age_band)),
                                  rep(0,length(age_band)),rep(0,length(age_band)),rep(0,length(age_band)),rep(0,length(age_band)),rep(name_var,length(age_band)),age_band),

                            cbind(rep(1,length(age_band)),rep(0,length(age_band)),rep(0,length(age_band)),
                                  rep(0,length(age_band)),rep(0,length(age_band)),rep(0,length(age_band)),rep(0,length(age_band)),rep(name_var,length(age_band)),age_band)))
names(rows) <- names(all)
all_out <- rbind(all,rows)

return(all_out)
}

#----------------------
#Random sample
query = 'SELECT * FROM freya.random_sample_final_v2;'
rs = dbSendQuery(db, query)
rand = fetch(rs, n=-1)

rand <- rand %>% mutate(eventdate = as.Date(eventdate))

#Oesophageal ulcer
query <- 'SELECT * FROM freya.cohort_oesoph_ulc_final;'
rs <- dbSendQuery(db, query)
oesoph_ulc <- fetch(rs, n=-1)

oesoph_ulc <- oesoph_ulc %>% select(-contains('same_day')) %>% select(-weight)

oesoph_ulc_var <- 'oesoph_ulc'
oesoph_ulc_name <- 'oesophageal ulc'

oesoph_ulc <- oesoph_ulc %>% rename(ever_obesity = obesity) %>% rename(ever_alcohol_problems = alc_problems)

all_oesoph_ulc <- format_foo_non_neo(oesoph_ulc,oesoph_ulc_var,oesoph_ulc_name)

#--------------------------------
#Barretts
query <- 'SELECT * FROM freya.cohort_barretts_final;'
rs <- dbSendQuery(db, query)
barretts <- fetch(rs, n=-1)

barretts <- barretts %>% select(-contains('same_day')) %>% select(-weight)

barretts_var <- 'barretts'
barretts_var_name <- 'barretts'

barretts <- barretts %>% rename(ever_obesity = obesity) %>% rename(ever_alcohol_problems = alc_problems)

all_barretts <- format_foo_non_neo(barretts,barretts_var,barretts_var_name)

#---------------------
#GORD
query <- 'SELECT * FROM freya.cohort_gord_final;'
rs <- dbSendQuery(db, query)
gord <- fetch(rs, n=-1)

gord <- gord %>% select(-contains('same_day')) %>% select(-weight)

gord_var <- 'gord'
gord_var_name <- 'gord'

gord <- gord %>% rename(ever_obesity = obesity) %>% rename(ever_alcohol_problems = alc_problems)

all_gord <- format_foo_non_neo(gord,gord_var,gord_var_name)
#-----------------------
## Abdominal hernia
query <- 'SELECT * FROM freya.cohort_hernia_abdo_final;'
rs <- dbSendQuery(db, query)
hernia_abdo <- fetch(rs, n=-1)

hernia_abdo <- hernia_abdo %>% select(-contains('same_day')) %>% select(-weight)

hernia_abdo_var_name <- 'hernia abdo'
hernia_abdo_var <- 'hernia_abdo'

hernia_abdo <- hernia_abdo %>% rename(ever_obesity = obesity) %>% rename(ever_alcohol_problems = alc_problems)

all_hernia_abdo <- format_foo_non_neo(hernia_abdo,hernia_abdo_var,hernia_abdo_var_name)
#-----------------------------
#Gastritis
query <- 'SELECT * FROM freya.cohort_gastritis_duodenitis_final;'
rs <- dbSendQuery(db, query)
gastritis_duodenitis <- fetch(rs, n=-1)

gastritis_duodenitis <- gastritis_duodenitis %>% select(-contains('same_day')) %>% select(-weight)

gastritis_duodenitis_var_name <- 'gastritis duodenitis'
gastritis_duodenitis_var <- 'gastritis_duodenitis'

gastritis_duodenitis <- gastritis_duodenitis %>% rename(ever_obesity = obesity) %>% rename(ever_alcohol_problems = alc_problems)

all_gastritis_duodenitis <- format_foo_non_neo(gastritis_duodenitis,gastritis_duodenitis_var,gastritis_duodenitis_var_name)
#-------------------------------
## joining all symptom tables together

#male and female split
split_foo <- function(data){
  data_overall <- data[1:2,]

  data <- data %>%
    arrange(age_band)
  
  data <- data %>% filter(!grepl("no",Category,ignore.case = T))
  
  all_data <- rbind(data_overall,data)
  
  all_data_f <- all_data %>% filter(gender == 1)
  all_data_m <- all_data %>% filter(gender == 0)
  all_data <- list(all_data_f,all_data_m)
  names(all_data) <- c("Women","Men")
  
  return(all_data)
}

#running function on each dataframe
all_barretts_split <- split_foo(all_barretts)

all_oesoph_ulc_split <- split_foo(all_oesoph_ulc)

all_gastritis_duodenitis_split <- split_foo(all_gastritis_duodenitis)

all_gord_split <- split_foo(all_gord)

all_hernia_abdo_split <- split_foo(all_hernia_abdo)

#joining together the sex specific data
all_all_f <- cbind(all_hernia_abdo_split[1],all_barretts_split[1],all_gastritis_duodenitis_split[1],all_gord_split[1],all_oesoph_ulc_split[1])

join_f <- left_join(as.data.frame(all_all_f[1]),as.data.frame(all_all_f[2]),by=c('age_band','Category'))
join_f <- left_join(join_f,as.data.frame(all_all_f[3]),by=c('age_band','Category'))
join_f <- left_join(join_f,as.data.frame(all_all_f[4]),by=c('age_band','Category'))
join_f <- left_join(join_f,as.data.frame(all_all_f[5]),by=c('age_band','Category'))

join_f <- join_f %>% select(-c(gender,gender.x,gender.x.x,gender.y,gender.y.y))

all_all_m <- cbind(all_hernia_abdo_split[2],all_barretts_split[2],all_gastritis_duodenitis_split[2],all_gord_split[2],all_oesoph_ulc_split[2])

join_m <- left_join(as.data.frame(all_all_m[1]),as.data.frame(all_all_m[2]),by=c('age_band','Category'))
join_m <- left_join(join_m,as.data.frame(all_all_m[3]),by=c('age_band','Category'))
join_m <- left_join(join_m,as.data.frame(all_all_m[4]),by=c('age_band','Category'))
join_m <- left_join(join_m,as.data.frame(all_all_m[5]),by=c('age_band','Category'))

join_m <- join_m %>% select(-c(gender,gender.x,gender.x.x,gender.y,gender.y.y))

xlsx::write.xlsx(join_f,file = "*",sheetName = "All OG risk - disease v3", append = TRUE)
xlsx::write.xlsx(join_m,file = "*",sheetName = "All OG risk - disease3",append=TRUE)

#----------------------------------------------------------------------------------
# Finding the youngest age band for each combination were the risk exceeds 3%

all_all_f_fil <- cbind(join_f[,4],join_f[,7:8],join_f[,12],join_f[,18],join_f[,24],join_f[,30])

all_all_f_fil <- all_all_f_fil %>%
  distinct() %>%
  rename(og_hernia_abdo = `join_f[, 4]`) %>%
  rename(og_barretts = `join_f[, 12]`) %>%
  rename(og_gastritis_duodenitis = `join_f[, 18]`) %>%
  rename(og_gord = `join_f[, 24]`) %>%
  rename(og_oesoph_ulc = `join_f[, 30]`) %>%
  select(Category, age_band, og_barretts,og_hernia_abdo,og_gastritis_duodenitis,og_gord,og_oesoph_ulc)

all_all_f_fil_t <- all_all_f_fil %>%
  mutate(og_barretts = ifelse(og_barretts > 0.03, og_barretts, NA)) %>%
  mutate(og_hernia_abdo = ifelse(og_hernia_abdo > 0.03, og_hernia_abdo, NA)) %>%
  mutate(og_gastritis_duodenitis = ifelse(og_gastritis_duodenitis > 0.03, og_gastritis_duodenitis, NA)) %>%
  mutate(og_gord = ifelse(og_gord > 0.03, og_gord, NA)) %>%
  mutate(og_oesoph_ulc = ifelse(og_oesoph_ulc > 0.03, og_oesoph_ulc, NA))
#----------------------------------
all_all_m_fil <- cbind(join_m[,4],join_m[,7:8],join_m[,12],join_m[,18],join_m[,24],join_m[,30])

all_all_m_fil <- all_all_m_fil %>%
  distinct() %>%
  rename(og_hernia_abdo = `join_m[, 4]`) %>%
  rename(og_barretts = `join_m[, 12]`) %>%
  rename(og_gastritis_duodenitis = `join_m[, 18]`) %>%
  rename(og_gord = `join_m[, 24]`) %>%
  rename(og_oesoph_ulc = `join_m[, 30]`) %>%
  select(Category, age_band, og_barretts,og_hernia_abdo,og_gastritis_duodenitis,og_gord,og_oesoph_ulc)

all_all_m_fil_t <- all_all_m_fil %>%
  mutate(og_barretts = ifelse(og_barretts > 0.03, og_barretts, NA)) %>%
  mutate(og_hernia_abdo = ifelse(og_hernia_abdo > 0.03, og_hernia_abdo, NA)) %>%
  mutate(og_gastritis_duodenitis = ifelse(og_gastritis_duodenitis > 0.03, og_gastritis_duodenitis, NA)) %>%
  mutate(og_gord = ifelse(og_gord > 0.03, og_gord, NA)) %>%
  mutate(og_oesoph_ulc = ifelse(og_oesoph_ulc > 0.03, og_oesoph_ulc, NA))

xlsx::write.xlsx(all_all_f_fil,file = "*",sheetName = "All risk disease",append=TRUE)
xlsx::write.xlsx(all_all_m_fil,file = "*",sheetName = "All risk disease",append=TRUE)
 
xlsx::write.xlsx(all_all_f_fil_t,file = "*",sheetName = "Threshold breach table Disease",append=TRUE)
xlsx::write.xlsx(all_all_m_fil_t,file = "*",sheetName = "Threshold breach table Disease",append=TRUE)

