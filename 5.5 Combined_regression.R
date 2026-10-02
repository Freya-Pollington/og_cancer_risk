rm(list=ls())

library(pacman)

.libPaths("S://ECHO_IHI_CPRD//Freya//Project 1//Rpackages")
# Install & load packages at same time
pacman::p_load(tidyverse, tidyr,splines,lmtest,
               RMySQL, DBI, xlsx,broom,readxl,data.table,RColorBrewer,lme4,janitor,broom.mixed,mltools,jtools,glmmLasso,forcats)


#----------------------------------------------------------------------------------------
##### Send query to retrieve initial table of upper GI symptoms within the study dates ####
db = dbConnect(MySQL(), host = "dsh-00872msq01.idhs.ucl.ac.uk", user = "rmjlfpo", password = "Fre11_pol23", 
                dbname = "freya", port = 3306)

#-------------------------------------------------------------------------
#A function to format cohort dataframes

data_format = function(data,cohort_var,cohort_num){
  #data = dysphagia
  data = data %>%
    select(-c(count_non_neo)) %>%
    #mutate(x = ifelse(symp_n >0,1,0)) %>%
    mutate(cohort = cohort_num)#%>%
    #select(-symp_n)
  
  #names(data)[37] = cohort_var
  
  #changing gender to 0 and 1
  data$gender = ifelse(data$gender == 2,1,0)
  
  #filtering out those in the rand sample from relevant cohort
  data <- data %>% anti_join(rand,by="e_patid","eventdate")
  
  data_out = data %>% select(-c(age,e_patid))
  data_age = data %>% select(c(age,e_patid))
  
  data_out[sapply(data_out,is.character)] = lapply(data_out[sapply(data_out,is.character)],as.factor)
  data_out[sapply(data_out,is.numeric)] = lapply(data_out[sapply(data_out,is.numeric)],as.factor)
  #data_out$symp_n = as.numeric(data_out$symp_n)
  data_out$unw_score = as.numeric(data_out$unw_score)
  
  data_out = data_out %>%
    mutate(elix_band = as.factor(cut(data_out$unw_score,breaks = c(0,1,3,5,25),include.lowest=TRUE)))%>%
    mutate(oesoph_ulc_bin = as.factor(ifelse(oesoph_ulc == 0,0,1))) %>%
    mutate(gord_bin = as.factor(ifelse(gord == 0,0,1))) %>%
    mutate(barretts_bin = as.factor(ifelse(barretts == 0,0,1))) %>%
    mutate(gastritis_duodenitis_bin = as.factor(ifelse(gastritis_duodenitis == 0,0,1))) %>%
    mutate(hernia_abdo_bin = as.factor(ifelse(hernia_abdo == 0,0,1)))
  
  data_out_all = cbind(data_out,data_age)
  
  #combining the ex-drinker variable with the drinker variable
  data_out_all = data_out_all %>%
    mutate(alc_status_comb = ifelse(alc_status_comb == 5,3,#if drinker with alc problems then join with ex drinker with problems
                                    ifelse(alc_status_comb == 4,2,#if drinker no alc problems merge with ex drinker no alc problems
                                           alc_status_comb))) %>%
    mutate(alc_status_comb = as.factor(alc_status_comb)) %>%
    select(-eventdate)
  
  return(as.data.frame(data_out_all))
}

#--------------------------------------------
data_format_non_neo = function(data,cohort_var,cohort_num){
  #data = gord
  data = data %>%
    select(-c(s_or_o_cancer,stomach_c,oesophageal_c,weight,count_non_neo,count_symptoms,data_source)) %>%
    mutate(x = 0) %>%
    rename(ever_obesity = obesity) %>%
    rename(ever_alcohol_problems = alc_problems) %>%
    mutate(cohort = cohort_num)
  
  names(data)[45] = cohort_var
  
  #changing gender to 0 and 1
  data$gender = ifelse(data$gender == 2,1,0)
  
  #filtering out those in the rand sample from relevant cohort
  data <- data %>% anti_join(rand,by=c("e_patid","eventdate")) %>% select(-eventdate)

  data_out = data %>% select(-c(age,e_patid))
  data_age = data %>% select(c(age,e_patid))
  
  data_out[sapply(data_out,is.character)] = lapply(data_out[sapply(data_out,is.character)],as.factor)
  data_out[sapply(data_out,is.numeric)] = lapply(data_out[sapply(data_out,is.numeric)],as.factor)
  data_out$unw_score = as.numeric(data_out$unw_score)
  
  data_out = data_out %>%
    mutate(elix_band = as.factor(cut(data_out$unw_score,breaks = c(0,1,3,5,25),include.lowest=TRUE)))%>%
    mutate(oesoph_ulc_bin = as.factor(ifelse(oesoph_ulc == 0,0,1))) %>%
    mutate(gord_bin = as.factor(ifelse(gord == 0,0,1))) %>%
    mutate(barretts_bin = as.factor(ifelse(barretts == 0,0,1))) %>%
    mutate(gastritis_duodenitis_bin = as.factor(ifelse(gastritis_duodenitis == 0,0,1))) %>%
    mutate(hernia_abdo_bin = as.factor(ifelse(hernia_abdo == 0,0,1)))
  
  data_out_all = cbind(data_out,data_age)
  
  #combining the ex-drinker variable with the drinker variable
  data_out_all = data_out_all %>%
    mutate(alc_status_comb = ifelse(alc_status_comb == 5,3,#if drinker with alc problems then join with ex drinker with problems
                                    ifelse(alc_status_comb == 4,2,#if drinker no alc problems merge with ex drinker no alc problems
                                           alc_status_comb))) %>%
    mutate(alc_status_comb = as.factor(alc_status_comb)) 
  
  
  return(as.data.frame(data_out_all))
}

#----------------------------------------
#Random sample

query = 'SELECT * FROM freya.random_sample_final_v2;'
rs = dbSendQuery(db, query)
rand = fetch(rs, n=-1)

#----------------------------------------
#GORD cohort
query = 'SELECT * FROM freya.cohort_gord_final;'
rs = dbSendQuery(db, query)
gord = fetch(rs, n=-1)

#data formatting
cohort_var = 'gord'
cohort_num = 1
gord_out = data_format_non_neo(gord,cohort_var,cohort_num)

#----------------------------------------
#Barretts cohort
query = 'SELECT * FROM freya.cohort_barretts_final;'
rs = dbSendQuery(db, query)
barretts = fetch(rs, n=-1)

#data formatting
cohort_var = 'barretts'
cohort_num = 5
barretts_out = data_format_non_neo(barretts,cohort_var,cohort_num)

#----------------------------------------
#oesoph_ulc cohort
query = 'SELECT * FROM freya.cohort_oesoph_ulc_final;'
rs = dbSendQuery(db, query)
oesoph_ulc = fetch(rs, n=-1)

#data formatting
cohort_var = 'oesoph_ulc'
cohort_num = 2
oesoph_ulc_out = data_format_non_neo(oesoph_ulc,cohort_var,cohort_num)

#----------------------------------------
#gastritis_duodenitis cohort
query = 'SELECT * FROM freya.cohort_gastritis_duodenitis_final;'
rs = dbSendQuery(db, query)
gastritis_duodenitis = fetch(rs, n=-1)

#data formatting
cohort_var = 'gastritis_duodenitis'
cohort_num = 3
gastritis_duodenitis_out = data_format_non_neo(gastritis_duodenitis,cohort_var,cohort_num)

#----------------------------------------
#hernia_abdo cohort
query = 'SELECT * FROM freya.cohort_hernia_abdo_final;'
rs = dbSendQuery(db, query)
hernia_abdo = fetch(rs, n=-1)

#data formatting
cohort_var = 'hernia_abdo'
cohort_num = 4
hernia_abdo_out = data_format_non_neo(hernia_abdo,cohort_var,cohort_num)

#----------------------------------------
#Dysphagia cohort
query = 'SELECT * FROM cohort_dysphagia_final;'
rs = dbSendQuery(db, query)
dysphagia = fetch(rs, n=-1)

#data formatting
cohort_var = 'dysphagia'
cohort_num = 6
dysphagia_out = data_format(dysphagia,cohort_var,cohort_num)

#----------------------------------------
#dyspepsia cohort
query = 'SELECT * FROM cohort_dyspepsia_final;'
rs = dbSendQuery(db, query)
dyspepsia = fetch(rs, n=-1)

#data formatting
cohort_var = 'dyspepsia'
cohort_num = 7
dyspepsia_out = data_format(dyspepsia,cohort_var,cohort_num)

#----------------------------------------
#abd_pain cohort
query = 'SELECT * FROM cohort_abd_pain_final;'
rs = dbSendQuery(db, query)
abd_pain = fetch(rs, n=-1)

#data formatting
cohort_var = 'abd_pain'
cohort_num = 8
abd_pain_out = data_format(abd_pain,cohort_var,cohort_num)

#----------------------------------------
#vomiting cohort
query = 'SELECT * FROM cohort_vomiting_final;'
rs = dbSendQuery(db, query)
vomiting = fetch(rs, n=-1)

#data formatting
cohort_var = 'vomiting'
cohort_num = 9
vomiting_out = data_format(vomiting,cohort_var,cohort_num)

all_cohorts = gord_out %>%
  mutate(same_day_gord=as.factor(0))%>%
  select(-contains('td_'))%>%
  union(gastritis_duodenitis_out%>%mutate(same_day_gastritis_duodenitis=as.factor(0))%>%
          select(-contains('td_')))%>%
  union(hernia_abdo_out%>%mutate(same_day_hernia_abdo=as.factor(0))%>%
          select(-contains('td_')))%>%
  union(barretts_out%>%mutate(same_day_barretts=as.factor(0))%>%
          select(-contains('td_')))%>%
  union(oesoph_ulc_out%>%mutate(same_day_oesoph_ulc=as.factor(0))%>%
          select(-contains('td_'))) %>%
  union(dyspepsia_out%>%mutate(same_day_dyspepsia=as.factor(0))%>%
          select(-contains('td_'))) %>%
  union(dysphagia_out%>%mutate(same_day_dysphagia=as.factor(0))%>%
          select(-contains('td_'))) %>%
  union(vomiting_out%>%mutate(same_day_vomiting=as.factor(0))%>%
          select(-contains('td_'))) %>%
  union(abd_pain_out%>%mutate(same_day_abd_pain=as.factor(0))%>%
          select(-contains('td_')))

#-----------------------------
#Joining in the random sample data

rand = rand %>%
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
  mutate(h_pylori_flag = 0) %>%
  mutate(vomiting = 0) %>%
  mutate(oesoph_ulc = 0) %>%
  mutate(gord = 0) %>%
  mutate(barretts = 0) %>%
  mutate(hernia_abdo = 0) %>%
  mutate(gastritis_duodenitis = 0) %>%
  mutate(cohort = "0") %>%
  rename(og_cancer = diag_1_yr) %>%
    rename(ever_obesity = obesity) %>%
    rename(ever_alcohol_problems = alc_problems)%>%
  select(-c(eventdate)) %>%
  mutate(same_day_gord = 0) %>%
  mutate(same_day_gastritis_duodenitis =0) %>%
  mutate(same_day_barretts = 0) %>%
  mutate(same_day_hernia_abdo = 0) %>%
  mutate(same_day_oesoph_ulc = 0) %>%
  mutate(same_day_dyspepsia = 0) %>%
  mutate(same_day_dysphagia = 0) %>%
  mutate(same_day_vomiting=0)%>%
  mutate(same_day_abd_pain=0)

rand$gender = ifelse(rand$gender == 2,1,0)

  rand_out = rand %>% select(-c(age,e_patid))
  rand_age = rand %>% select(c(age,e_patid))
  
  rand_out[sapply(rand_out,is.character)] = lapply(rand_out[sapply(rand_out,is.character)],as.factor)
  rand_out[sapply(rand_out,is.numeric)] = lapply(rand_out[sapply(rand_out,is.numeric)],as.factor)
  rand_out$unw_score = as.numeric(rand_out$unw_score)
  
  rand_out = rand_out %>%
    mutate(elix_band = as.factor(cut(rand_out$unw_score,breaks = c(0,1,3,5,25),include.lowest=TRUE)))%>%
    mutate(oesoph_ulc_bin = as.factor(ifelse(oesoph_ulc == 0,0,1))) %>%
    mutate(gord_bin = as.factor(ifelse(gord == 0,0,1))) %>%
    mutate(barretts_bin = as.factor(ifelse(barretts == 0,0,1))) %>%
    mutate(gastritis_duodenitis_bin = as.factor(ifelse(gastritis_duodenitis == 0,0,1))) %>%
    mutate(hernia_abdo_bin = as.factor(ifelse(hernia_abdo == 0,0,1)))
  
  rand_all = cbind(rand_out,rand_age)
  rand_all = distinct(rand_all)
  
  #combining the ex-drinker variable with the drinker variable
  rand_all = rand_all %>%
    mutate(alc_status_comb = ifelse(alc_status_comb == 5,3,#if drinker with alc problems then join with ex drinker with problems
                                    ifelse(alc_status_comb == 4,2,#if drinker no alc problems merge with ex drinker no alc problems
                                           alc_status_comb))) %>%
    mutate(alc_status_comb = as.factor(alc_status_comb)) #%>%
    #select(-c(most_recent_smok_date,smokingcat,smok_date,))
  
  
final_all = union(all_cohorts,rand_all)

dbWriteTable(db, "combined_data_v4",final_all, row.names = FALSE, overwrite = TRUE) #v2 has the random sample subampled and removed from the cohorts, V3 with the larger sample size
#-------------------------------------------------------------------------------------------------------------
# query = 'SELECT * FROM combined_data_v2;'
# rs = dbSendQuery(db, query)
# final_old = fetch(rs, n=-1)

query = 'SELECT * FROM combined_data_v4;'
rs = dbSendQuery(db, query)
final_all = fetch(rs, n=-1)

final_all[sapply(final_all,is.character)] = lapply(final_all[sapply(final_all,is.character)],as.factor)

final_all_ohe_2 = one_hot(as.data.table(final_all),cols='cohort')

#prepare data for modelling with age spline
model_data = final_all_ohe_2 %>%
  mutate(age_idate = (age-60)/10) %>%
  mutate(elix_band = factor(elix_band, levels=c("[0,1]","(1,3]","(3,5]","(5,25]")))

#a model without one hot encoded variables
model_data_2 = final_all %>%
  mutate(age_idate = (age-60)/10) %>%
  mutate(cohort = factor(cohort, levels=c(0,1,2,3,4,5,6,7,8,9))) %>%
  mutate(elix_band = factor(elix_band, levels=c("[0,1]","(1,3]","(3,5]","(5,25]")))

#----------------------------------------------------------
#run model without mixed effects
spline_mod3 = glm(og_cancer ~ ns(age_idate,knots=c(-1,0,1,2)) + ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8)+
                    cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
                   gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
                   oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
                  family = binomial,
                  data = model_data)
summary(spline_mod3)
saveRDS(spline_mod3,file="S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\spline_model.rda") #v2 with combined data and then no appetite loss in following iteration

spline_mode=readRDS('spline_model_final_pre_upgrade_no_sd.rda')
tidy(spline_mode,p.values=TRUE)

tab = as.data.frame(tidy(spline_mode,p.values=TRUE))
tab['odds_ratio'] = exp(tab['estimate'])
tab=tab %>% select (term,format(round(tab$odds_ratio,3)),format(round(tab$p.value,3)))

spline_mod3_men = glm(og_cancer ~ ns(age_idate,knots=c(-1,0,1,2)) + ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8)+
                    cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
                    bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
                    oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
                  family = binomial,
                  data = model_data%>%filter(gender==1))
summary(spline_mod3_men)
saveRDS(spline_mod3_men,file="S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\spline_model_men.rda") 

spline_mod3_women = glm(og_cancer ~ ns(age_idate,knots=c(-1,0,1,2)) + ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8)+
                        cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
                        bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
                        oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
                      family = binomial,
                      data = model_data%>%filter(gender==0))
summary(spline_mod3_women)
saveRDS(spline_mod3_women,file="S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\spline_model_women.rda") 
#-------------------------------------------------------------------
#run model with interaction between all cohorts and the co-occurring features and sex stratification

spline_mod4_men = glm(og_cancer ~ ns(age_idate,knots=c(-1,0,1,2)) + ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8)+
                        cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
                        (bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
                        oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin):(cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8)+
                        bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
                        oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
                      family = binomial,
                      data = model_data%>%filter(gender==0))
summary(spline_mod4_men)
saveRDS(spline_mod4_men,file="spline_model_men_complex.rda") 

spline_mod4_women = glm(og_cancer ~ ns(age_idate,knots=c(-1,0,1,2)) + ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8)+
                          cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
                          bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
                          oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin+
                          (bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
                             oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin):(cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8),
                        family = binomial,
                        data = model_data%>%filter(gender==1))
summary(spline_mod4_women)
saveRDS(spline_mod4_women,file="spline_model_women_complex.rda") 

#----------------------------------------------------------
#run model without one hot encoding
# spline_mod = glm(og_cancer ~ ns(age_idate,knots=c(-1,0,1,2))*cohort+
#                    gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#                    oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
#                  family = binomial,
#                  data = model_data_2)
# summary(spline_mod_v2)
# splin_out <- tidy(spline_mod_v2)
# splin_out <- splin_out %>% mutate(est_e = exp(estimate))
# saveRDS(spline_mod,file="spline_model_v5_no_app_loss.rda") #with combined data and not one hot encoded is v3, v5 is changing the knots, then ran again without appetite loss
#----------------------------------------------------------
#run model without one hot encoding
# spline_mod = glm(og_cancer ~ ns(age_idate,df=3)*cohort+
#                    gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#                    oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
#                  family = binomial,
#                  data = model_data_2)
# summary(spline_mod)
# # splin_out <- tidy(spline_mod_v2)
# # splin_out <- splin_out %>% mutate(est_e = exp(estimate))
# saveRDS(spline_mod,file="spline_model_final_pre_upgrade_v2.rda") #with combined data and not one hot encoded is v3, v5 is changing the knots to more, v6 is changin ghe knots back to df=3 and without app loss, v7 with updated random sample
# #vif(spline_mod3)
# 
# # spline_mod_test = glm(og_cancer ~ ns(age_idate,df=3)+cohort+
# #                    gender,
# #                  family = binomial(link="logit"),
# #                  data = model_data_2)
# # summary(spline_mod_test)
# 
# #trying restricting the ages
# spline_mod_age_rest = glm(og_cancer ~ ns(age_idate,df=3)*cohort+
#                    gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#                    oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
#                  family = binomial,
#                  data = model_data_2 %>% filter(age>40 & age<80))
# summary(spline_mod_age_rest)
# 
# #trying changing the knots
# spline_mod_age_change = glm(og_cancer ~ ns(age_idate,knots=c(-1,0,2))*cohort+
#                             gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#                             oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
#                           family = binomial,
#                           data = model_data_2 #%>% filter(age>40 & age<80)
#                           )
# summary(spline_mod_age_change)
# 
# #trying dropping df
# spline_mod_cohort_drop = glm(og_cancer ~ ns(age_idate,df=2)*cohort+
#                               gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#                               oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
#                             family = binomial,
#                             data = model_data_2 #%>% filter(cohort != 9 & cohort !=8)
# )
# summary(spline_mod_cohort_drop)
# saveRDS(spline_mod_cohort_drop,file="spline_model_final_pre_upgrade_drop_df.rda") #with combined data and not one hot encoded is v3, v5 is changing the knots to more, v6 is changin ghe knots back to df=3 and without app loss, v7 with updated random sample


# splin_out <- tidy(spline_mod_v2)
# splin_out <- splin_out %>% mutate(est_e = exp(estimate))
#saveRDS(spline_mod,file="spline_model_final_pre_upgrade.rda")

#----------------------------------------------------------
#run model one hot encoding and 

model_data_3 <- model_data %>% mutate(dyspepsia = ifelse(same_day_dyspepsia == 1 & dyspepsia == 1,as.factor(0),dyspepsia),
                                        dyspepsia=as.factor(ifelse(dyspepsia==1,0,1)),
                        dysphagia = ifelse(same_day_dysphagia == 1 & dysphagia == 1,as.factor(0),dysphagia),
                        dysphagia=as.factor(ifelse(dysphagia==1,0,1)),
                        vomiting = ifelse(same_day_vomiting == 1 & vomiting == 1,as.factor(0),vomiting),
                        vomiting=as.factor(ifelse(vomiting==1,0,1)),
                        abd_pain = ifelse(same_day_abd_pain == 1 & abd_pain == 1,as.factor(0),abd_pain),
                        abd_pain=as.factor(ifelse(abd_pain==1,0,1)),
                        barretts_bin = ifelse(same_day_barretts == 1 & barretts_bin == 1,as.factor(0),barretts_bin),
                        barretts_bin=as.factor(ifelse(barretts_bin==1,0,1)),
                        gord_bin = ifelse(same_day_gord == 1 & gord_bin == 1,as.factor(0),gord_bin),
                        gord_bin=as.factor(ifelse(gord_bin==1,0,1)),
                        gastritis_duodenitis_bin = ifelse(same_day_gastritis_duodenitis == 1 & gastritis_duodenitis_bin == 1,0,gastritis_duodenitis_bin),
                        gastritis_duodenitis_bin=as.factor(ifelse(gastritis_duodenitis_bin==1,0,1)),
                        oesoph_ulc_bin = ifelse(same_day_oesoph_ulc == 1 & oesoph_ulc == 1,as.factor(0),oesoph_ulc_bin),
                        oesoph_ulc_bin=as.factor(ifelse(oesoph_ulc_bin==1,0,1)),
                        hernia_abdo_bin = ifelse(same_day_hernia_abdo == 1 & hernia_abdo_bin == 1,as.factor(0),hernia_abdo_bin),
                        hernia_abdo_bin=as.factor(ifelse(hernia_abdo_bin==1,0,1)),)

spline_mod2 = glm(og_cancer ~ ns(age_idate,knots=c(-1,0,1,2)) + ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8)+
                    cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
                   gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
                   oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
                 family = binomial,
                 data = model_data_3)
summary(spline_mod2)
# splin_out <- tidy(spline_mod_v2)
# splin_out <- splin_out %>% mutate(est_e = exp(estimate))
saveRDS(spline_mod2,file="S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\spline_model_no_sd.rda") #with combined data and not one hot encoded is v3, v5 is changing the knots to more, v6 is changin ghe knots back to df=3 and without app loss, v7 with updated random sample
#----------------------------------------------------------
# spline_mod2 = glm(og_cancer ~ ns(age_idate,df=2)*cohort+
#                     gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#                     oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin,
#                   family = binomial,
#                   data = model_data_3)
# summary(spline_mod2)
# # splin_out <- tidy(spline_mod_v2)
# # splin_out <- splin_out %>% mutate(est_e = exp(estimate))
# saveRDS(spline_mod2,file="spline_model_final_pre_upgrade_no_sd_drop_df.rda") #with combined data and not one hot encoded is v3, v5 is changing the knots to more, v6 is changin ghe knots back to df=3 and without app loss, v7 with updated random sample
#----------------------------------------------------------
#run model without one hot encoding and with interactions with every covariate
# spline_mod_v3 = glm(og_cancer ~ cohort*(ns(age_idate,df=3)+
#                       gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#                       oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin+appetite_loss),
#                     family = binomial,
#                     data = model_data_2)
# summary(spline_mod_v3)
# splin_out <- tidy(spline_mod_v3)
# splin_out <- splin_out %>% mutate(est_e = exp(estimate))
# saveRDS(spline_mod_v3,file="spline_model_v4.rda") #with combined data and not one hot encoded
# ------------------------------------------------------------------
# #run model with mixed effects
# mixed_effects_mod = glmer(og_cancer ~ ns(age_idate,df=3) + ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9)+cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
#                              gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#                              oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin+appetite_loss+
#                              (1 | e_patid),
#                            family=binomial,
#                            data=model_data,
#                            nAGQ = 0, # fastest - least accurate - RE approximation
#                            control = glmerControl(calc.derivs = FALSE))
# 
# saveRDS(mixed_effects_mod,file="mixed_effects_model_fixed.rda")
# #--------------------------------------------------------
# #simplify model by removing other covariates
# mixed_effects_mod2 = glmer(og_cancer ~ ns(age_idate,df=3)+ ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9)+
#                              cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
#                              gender+ (1 | e_patid),
#                            family=binomial(link='logit'),
#                            data=model_data,
#                            nAGQ = 0, # fastest - least accurate - RE approximation
#                            control = glmerControl(calc.derivs = FALSE))
# 
# saveRDS(mixed_effects_mod2,file="mixed_effects_model_cohorts_only.rda")
# 
# summary(mixed_effects_mod2)
# #----------------------------------------------------------
# #trying to add random sample cohort in
# mixed_effects_mod3 = glmer(og_cancer ~ ns(age_idate,df=3)+ ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9)+
#                              cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+cohort_0+
#                              gender+ (1 | e_patid),
#                            family=binomial(link='logit'),
#                            data=model_data,
#                            nAGQ = 0, # fastest - least accurate - RE approximation
#                            control = glmerControl(calc.derivs = FALSE))
# 
# saveRDS(mixed_effects_mod3,file="mixed_effects_model_cohorts_only_rand.rda")
# 
# summary(mixed_effects_mod3)
# #----------------------------------------------------------
# #reduced the number of df in the interaction term
# mixed_effects_mod4 = glmer(og_cancer ~ ns(age_idate,df=3)+ ns(age_idate,df=1):(cohort_6+cohort_4+cohort_9)+
#                              cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+cohort_0+
#                              gender+ (1 | e_patid),
#                            family=binomial(link='logit'),
#                            data=model_data,
#                            nAGQ = 0, # fastest - least accurate - RE approximation
#                            control = glmerControl(calc.derivs = FALSE))
# 
# saveRDS(mixed_effects_mod4,file="mixed_effects_model_cohorts_only_rand_low_df.rda")
# 
# summary(mixed_effects_mod4)
# #-------------------------------------------------------------
# #removing the splines
# mixed_effects_mod5 = glmer(og_cancer ~ age_idate+ age_idate:(cohort_6+cohort_4+cohort_9)+
#                              cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+cohort_0+
#                              gender+ (1 | e_patid),
#                            family=binomial(link='logit'),
#                            data=model_data,
#                            nAGQ = 0, # fastest - least accurate - RE approximation
#                            control = glmerControl(calc.derivs = FALSE))
# 
# summary(mixed_effects_mod5)
# #-----------------------------------------------------------------
# #removing the splines and the interactions
# mixed_effects_mod6 = glmer(og_cancer ~ age_idate+
#                              cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+cohort_0+
#                              gender+ (1 | e_patid),
#                            family=binomial(link='logit'),
#                            data=model_data,
#                            nAGQ = 0, # fastest - least accurate - RE approximation
#                            control = glmerControl(calc.derivs = FALSE))
# 
# summary(mixed_effects_mod6)
# #----------------------------------------------------------------
# #testing just two of the cohorts
# mixed_effects_mod7 = glmer(og_cancer ~ ns(age_idate,df=3)+
#                              cohort_1+cohort_2+
#                              gender+ (1 | e_patid),
#                            family=binomial(link='logit'),
#                            data=model_data,
#                            nAGQ = 0, # fastest - least accurate - RE approximation
#                            control = glmerControl(calc.derivs = FALSE))
# 
# test <- model_data %>%
#   filter(cohort_6==1)
# 
# summary(mixed_effects_mod7)
# #-----------------------------------------------------------
# #trying the simplest model
# mixed_effects_mod = glmer(og_cancer ~ ns(age_idate,df=3)+
#                              gender+ (1 | e_patid),
#                            family=binomial(link='logit'),
#                            data=model_data,
#                            nAGQ = 0, # fastest - least accurate - RE approximation
#                            control = glmerControl(calc.derivs = FALSE)
# )
# 
# saveRDS(mixed_effects_mod8,file="mixed_effects_model_simplest.rda")
# 
# summary(mixed_effects_mod8)
# #-----------------------------------------------------------
# #trying a model with numeric variables rather than factors and only the cohorts
# model_data_num <- model_data %>%
#   select(-c(age_band,bmi_cat,alc_status_comb,elix_band))
# 
# model_data_num <- data.frame(lapply(model_data_num,function(x){
#   if(is.factor(x)){
#     return(as.numeric(as.character(x)))
#   } else{
#     return(x)
#   }
# }
# ))
# 
# model_data_num <- cbind(model_data_num, model_data %>%
#                           select(c(age_band,bmi_cat,alc_status_comb,elix_band)))

# mixed_effects_mod9 = glmer(og_cancer ~ ns(age_idate,df=3)+ ns(age_idate,df=1):(cohort_6+cohort_4+cohort_9)+
#                              cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+cohort_0+
#                            gender+ (1 | e_patid),
#                            family=binomial(link='logit'),
#                            data=model_data_num,
#                            nAGQ = 0, # fastest - least accurate - RE approximation
#                            control = glmerControl(calc.derivs = FALSE)
# )
# 
# summary(mixed_effects_mod9)
# #-----------------------------------------------------------------
# #trying the basic model again
# # model_data <- model_data %>%
# #   mutate(cohort_0 = as.factor(cohort_0))%>%
# #   mutate(cohort_1 = as.factor(cohort_1))%>%
# #   mutate(cohort_2 = as.factor(cohort_2))%>%
# #   mutate(cohort_3 = as.factor(cohort_3))%>%
# #   mutate(cohort_4 = as.factor(cohort_4))%>%
# #   mutate(cohort_5 = as.factor(cohort_5))%>%
# #   mutate(cohort_6 = as.factor(cohort_6))%>%
# #   mutate(cohort_7 = as.factor(cohort_7))%>%
# #   mutate(cohort_8 = as.factor(cohort_8))%>%
# #   mutate(cohort_9= as.factor(cohort_9))
# # 
# # mixed_effects_mod10 = glm(og_cancer ~ ns(age_idate,df=3)+ ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9)+
# #                              cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+cohort_0+
# #                              gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
# #                              oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin+appetite_loss,
# #                            family=binomial(link='logit'),
# #                            data=model_data#,
# #                            #nAGQ = 0, # fastest - least accurate - RE approximation
# #                            #control = glmerControl(calc.derivs = FALSE)
# # )
# # 
# # saveRDS(mixed_effects_mod10,file="model_no_mixed.rda")
# # 
# # summary(mixed_effects_mod10)
# # 
# # preds <- predict(mixed_effects_mod10,newdata=predict_data %>% mutate(cohort_6 = as.factor(1)) %>% mutate(cohort_6 = factor(cohort_6,levels=c(0,1))),type='response')
# 
# #-----------------------------
# #trying geeglm
# install.packages("geepack")
# library(geepack)
# marginal_model_og <- geepack::geeglm(
#   og_cancer ~ ns(age_idate, df=3) + gender,
#   id = e_patid,
#   family = binomial(link="logit"),
#   corstr = "exchangeable",
#   data = model_data_num %>% arrange(e_patid)
# )
# 
# saveRDS(marginal_model_og,file="marginal_model_og.rda")
# 
# marginal_model_og <- readRDS("marginal_model.rda")
# summary(marginal_model_og)
# #------------------------
# #trying marginal model with all variables
#   marginal_model <- geepack::geeglm(
#   og_cancer ~ ns(age_idate, df=3) + ns(age_idate,df=3):(cohort_6+cohort_4+cohort_9)+
#     cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
#     gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#     oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin+appetite_loss,
#   id = e_patid,
#   family = binomial(link="logit"),
#   corstr = "exchangeable",
#   data = model_data_num %>% arrange(e_patid)
# )

# saveRDS(marginal_model,file="marginal_model_full.rda")
# marginal_mod <- readRDS("marginal_model.rda")
#summary(marginal_mod)
# 
# install.packages("car")
# library(car)
# vi <- vif(marginal_model)
# 
# cor(model_data_num[,24:33])
# #------------------------
# #trying marginal model with no interactions
# marginal_model_3 <- geepack::geeglm(
#   og_cancer ~ ns(age_idate, df=3) +
#     cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
#     gender+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#     oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin+appetite_loss,
#   id = e_patid,
#   family = binomial(link="logit"),
#   corstr = "exchangeable",
#   data = model_data_num %>% arrange(e_patid)
# )
# 
# summary(marginal_model_3)
# 
# vi <- vif(marginal_model_3)
# #------------------------
# #trying marginal model with only the cohort variables and no interaction
# marginal_model_4 <- geepack::geeglm(
#   og_cancer ~ ns(age_idate, df=3) +
#     cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8+
#     gender,
#   id = e_patid,
#   family = binomial(link="logit"),
#   corstr = "exchangeable",
#   data = model_data_num %>% arrange(e_patid)
# )
# 
# # 
# summary(marginal_model_4)
# 
# vif(marginal_model_4)
# #------------------------
# #trying the marginal model with no interaction or cohort variables but all other variables (idk why)
# marginal_model_5 <- geepack::geeglm(
#   og_cancer ~ ns(age_idate, df=3) +
#     bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+
#     oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin+appetite_loss+
#     gender,
#   id = e_patid,
#   family = binomial(link="logit"),
#   corstr = "exchangeable",
#   data = model_data_num %>% arrange(e_patid)
# )
# # 
# summary(marginal_model_5)
# 
# vif(marginal_model_5)
# #------------------------
# #trying the marginal model without the one hot encoding of the cohort variable and with numeric variables
# model_data_2 = final_all %>%
#   mutate(age_idate = (age-60)/10)
# 
# model_data_2_num <- model_data_2 %>%
#   select(-c(age_band,bmi_cat,alc_status_comb,elix_band,cohort,gender))
# 
# model_data_2_num <- data.frame(lapply(model_data_2_num,function(x){
#   if(is.factor(x)){
#     return(as.numeric(as.character(x)))
#   } else{
#     return(x)
#   }
# }
# ))
# 
# model_data_2_num <- cbind(model_data_2_num, model_data_2 %>%
#                           select(c(age_band,bmi_cat,alc_status_comb,elix_band,cohort,gender)))
# 
# marginal_model_6 <- geepack::geeglm(
#   og_cancer ~ ns(age_idate, df=3) +cohort+
#     gender,
#   id = e_patid,
#   family = binomial(link="logit"),
#   corstr = "exchangeable",
#   data = model_data_2_num %>% arrange(e_patid)
# )

# 
# summary(marginal_model_6)
# 
# vif(marginal_model_6)
# #-----------------------
# #trying the marginela model with only age, cohort not one hot encoded and sex
# marginal_model_7 <- geepack::geeglm(
#   og_cancer ~ age_idate +cohort+
#     gender,
#   id = e_patid,
#   family = binomial(link="logit"),
#   corstr = "exchangeable",
#   data = model_data_2_num %>% arrange(e_patid)
# )
# 
# #saveRDS(marginal_model,file="marginal_model_full.rda")
# 
# summary(marginal_model_7)
# 
# vif(marginal_model_7)
# 
# #------------------------------------------
# #trying mixed effects with nAGQ set to 1 and optimizer used
# mixed_effects_mod11 = glmer(og_cancer ~ ns(age_idate,df=3)+ gender+(1|e_patid),
#                           family=binomial(link='logit'),
#                           data=model_data,
#                           nAGQ = 1, # fastest - least accurate - RE approximation
#                           control = glmerControl(optimizer = "bobyqa",
#                                                  calc.derivs = FALSE)
# )
# 
# saveRDS(mixed_effects_mod11,file="mixed_effects_bobyqa.rda")
# model <- readRDS("mixed_effects_bobyqa.rda")
# summary(model)
# #------------------------
# #trying duplicating all records to give each patient more than one record, and keeping model simple, and using an optimiser
# model_data_dup <- model_data %>%
#   slice(rep(1:n(),each=2))
# 
# mixed_effects_mod12 = glmer(og_cancer ~ ns(age_idate,df=3)+ gender+(1|e_patid),
#                             family=binomial(link='logit'),
#                             data=model_data_dup,
#                             nAGQ = 0, # fastest - least accurate - RE approximation
#                             control = glmerControl(optimizer = "bobyqa",
#                                                    calc.derivs = FALSE)
# )
# 
# saveRDS(mixed_effects_mod12,file="mixed_effects_bobyqa.rda")
# 
# #data <- readRDS("mixed_effects_bobyqa.rda")
# 
# summary(mixed_effects_mod12)
# #-------------------------------------------------------
# #trying the same but without bobyqa
# mixed_effects_mod13 = glmer(og_cancer ~ ns(age_idate,df=3)+ gender+(1|e_patid),
#                             family=binomial(link='logit'),
#                             data=model_data_dup,
#                             nAGQ = 0, # fastest - least accurate - RE approximation
#                             control = glmerControl( calc.derivs = FALSE)
# )
# 
# #saveRDS(mixed_effects_mod12,file="mixed_effects_bobyqa.rda")
# 
# #data <- readRDS("mixed_effects_bobyqa.rda")
# 
# summary(mixed_effects_mod13)
# #--------------------------------------------------------
# #trying normal data, basic model, nAGQ set to 2 and no optimiser
# mixed_effects_mod14 = glmer(og_cancer ~ ns(age_idate,df=3)+ gender+(1|e_patid),
#                             family=binomial(link='logit'),
#                             data=model_data,
#                             nAGQ = 2, # fastest - least accurate - RE approximation
#                             control = glmerControl( calc.derivs = FALSE)
# )
# 
# summary(mixed_effects_mod14)
# 
# 
# # tring glmmTMB------------------------------------------------------------------------------------
# # 
# # mixed_effects_mod3 = glmmTMB(og_cancer ~ age_idate*gender*(cohort_1+cohort_6+cohort_4+cohort_9+cohort_1+cohort_2+cohort_3+cohort_5+cohort_7+cohort_8)+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+
# #                               weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin+hernia_abdo_bin+appetite_loss+
# #                               + (1 | e_patid)
# #                             ,
# #                             family=binomial(link='logit'),
# #                             data=model_data,
# #                             #nAGQ = 0, # fastest - least accurate - RE approximation
# #                             #control = glmerControl(calc.derivs = FALSE)
# #                             )
# # 
# # saveRDS(mixed_effects_mod3,file="mixed_effects_model3.rda")
# #---------------------------------------------------------------------------------------
# #a model without one hot encoded variables
# model_data_2 = final_all %>%
#   mutate(age_idate = (age-60)/10)
# 
# model_data_og <- model_data_2 %>% filter(og_cancer == 0)
# 
# mixed_effects_mod4 = glmer(og_cancer ~ ns(age_idate,df=3)*gender*(cohort)+bmi_cat+alc_status_comb+final_smoking_status+low_hb+h_pylori_flag+
#                                 weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+oesoph_ulc_bin+barretts_bin+gord_bin+
#                              gastritis_duodenitis_bin+hernia_abdo_bin+appetite_loss+(1 | e_patid),
#                               family=binomial(link='logit'),
#                               data=model_data_2,
#                               nAGQ = 0, # fastest - least accurate - RE approximation
#                               control = glmerControl(calc.derivs = FALSE)
# )
# 
# saveRDS(mixed_effects_mod4,file="mixed_effects_model4.rda")
# 
# 
# mixed_effects_mod = readRDS(file='mixed_effects_model.rda')
# 
# #--------------------------------
# # And then to get predicted outcomes for cohort_6 == 1...
# model_pred_cohort_6_all_pats = 
#   marginaleffects::predictions(
#     model = mixed_effects_mod,
#     newdata = model_data |>
#       filter(cohort_1 == 1) %>%
#       select(-og_cancer)
#     ,
#     re.form=NA
#   )
# age_risk = aggregate(estimate~age+gender,model_pred_cohort_6_all_pats,mean)
# 
# #-------------------------------------------------------------------------
# # Starting point = every age for a single combination of covariates
# predict_data =
#   tibble(age = seq(30, 100, length.out = 71)) |>
#   mutate(age_idate = (age-60)/10) |>
#   mutate(gender = as.factor(0)) |>
#   mutate(
#     cohort_0 = as.factor(0),
#     cohort_1 = as.factor(0),
#     cohort_2 = as.factor(0),
#     cohort_3 = as.factor(0),
#     cohort_4 = as.factor(0),
#     cohort_5 = as.factor(0),
#     cohort_6 = as.factor(0),
#     cohort_7 = as.factor(0),
#     cohort_8 = as.factor(0),
#     cohort_9 = as.factor(0)
#   ) |>
#   mutate(
#     cohort_0 = factor(cohort_0,levels=c(0,1)),
#     cohort_1 =   factor(cohort_1,levels=c(0,1)),
#     cohort_2 =   factor(cohort_2,levels=c(0,1)),
#     cohort_3 =   factor(cohort_3,levels=c(0,1)),
#     cohort_4 =   factor(cohort_4,levels=c(0,1)),
#     cohort_5 =   factor(cohort_5,levels=c(0,1)),
#     cohort_6 =   factor(cohort_6,levels=c(0,1)),
#     cohort_7 =   factor(cohort_7,levels=c(0,1)),
#     cohort_8 =  factor(cohort_8,levels=c(0,1)),
#     cohort_9 =   factor(cohort_9,levels=c(0,1))
#   ) %>%
#   mutate(
#     bmi_cat = as.factor(0),
#     alc_status_comb = as.factor(1),
#     final_smoking_status = as.factor(1),
#     low_hb = as.factor(0),
#     h_pylori_flag = as.factor(0), 
#     elix_band = as.factor("[0,1]")
#   ) |>
#   mutate(
#     bmi_cat = factor(bmi_cat,levels=c(0,1,2,3)),
#     alc_status_comb =   factor(alc_status_comb,levels=c(1,2,3)),
#     final_smoking_status =   factor(final_smoking_status,levels=c(1,2,4)),
#     low_hb =   factor(low_hb,levels=c(0,1)),
#     h_pylori_flag =   factor(h_pylori_flag,levels=c(0,1)),
#     elix_band = factor(elix_band,levels=c("[0,1]","(1,3]","(3,5]","(5,25]"))
#   ) %>%
#   mutate(
#     weight_loss = as.factor(0), 
#     dyspepsia = as.factor(0), 
#     dysphagia = as.factor(0), 
#     fatigue = as.factor(0), 
#     cough = as.factor(0), 
#     vomiting = as.factor(0), 
#     abd_pain = as.factor(0), 
#     raise_platelets = as.factor(0), 
#     raise_wbc = as.factor(0),
#     oesoph_ulc_bin = as.factor(0), 
#     barretts_bin = as.factor(0), 
#     gord_bin = as.factor(0), 
#     gastritis_duodenitis_bin = as.factor(0), 
#     hernia_abdo_bin = as.factor(0), 
#     #appetite_loss = as.factor(0)
#   ) %>%
#   mutate(
#     weight_loss = factor(weight_loss,levels=c(0,1)),
#     dyspepsia =   factor(dyspepsia,levels=c(0,1)),
#     dysphagia =   factor(dysphagia,levels=c(0,1)),
#     fatigue =   factor(fatigue,levels=c(0,1)),
#     cough =   factor(cough,levels=c(0,1)),
#     vomiting =   factor(vomiting,levels=c(0,1)),
#     abd_pain =   factor(abd_pain,levels=c(0,1)),
#     raise_platelets =  factor(raise_platelets,levels=c(0,1)),
#     raise_wbc =   factor(raise_wbc,levels=c(0,1)),
#     #appetite_loss =   factor(appetite_loss,levels=c(0,1)),
#     oesoph_ulc_bin =   factor(oesoph_ulc_bin,levels=c(0,1)),
#     barretts_bin =   factor(barretts_bin,levels=c(0,1)),
#     gord_bin =   factor(gord_bin,levels=c(0,1)),
#     gastritis_duodenitis_bin =  factor(gastritis_duodenitis_bin,levels=c(0,1)),
#     hernia_abdo_bin =   factor(hernia_abdo_bin,levels=c(0,1))
#   ) %>%
#   mutate(e_patid = 0) %>%
#   mutate(og_cancer = NA)
# 
# # Want to consider both genders, so duplicate
# predict_data = predict_data |>
#   bind_rows(predict_data |> mutate(gender = as.factor(1))) 

#-------------------------------------------------
#spline_mod <- readRDS("spline_model_v8_no_app_loss.rda")
pred_data =
  tibble(age = seq(30, 100, length.out = 71)) |>
  mutate(age_idate = (age-60)/10) |>
  mutate(gender = as.factor(0)) |>
  mutate(
    cohort_0 = 0,
    cohort_1 = 0,
    cohort_2 = 0,
    cohort_3 = 0,
    cohort_4 = 0,
    cohort_5 = 0,
    cohort_6 = 0,
    cohort_7 = 0,
    cohort_8 = 0,
    cohort_9 = 0  ) |>
  # mutate(
  #   cohort_0 = factor(cohort_0,levels=c(0,1)),
  #   cohort_1 =   factor(cohort_1,levels=c(0,1)),
  #   cohort_2 =   factor(cohort_2,levels=c(0,1)),
  #   cohort_3 =   factor(cohort_3,levels=c(0,1)),
  #   cohort_4 =   factor(cohort_4,levels=c(0,1)),
  #   cohort_5 =   factor(cohort_5,levels=c(0,1)),
  #   cohort_6 =   factor(cohort_6,levels=c(0,1)),
  #   cohort_7 =   factor(cohort_7,levels=c(0,1)),
  #   cohort_8 =  factor(cohort_8,levels=c(0,1)),
  #   cohort_9 =   factor(cohort_9,levels=c(0,1))
  # ) %>%
  mutate(
    bmi_cat = as.factor(0),
    alc_status_comb = as.factor(1),
    final_smoking_status = as.factor(1),
    low_hb = as.factor(0),
    h_pylori_flag = as.factor(0), 
    elix_band = as.factor("[0,1]")
  ) |>
  mutate(
    bmi_cat = factor(bmi_cat,levels=c(0,1,2,3)),
    alc_status_comb =   factor(alc_status_comb,levels=c(1,2,3)),
    final_smoking_status =   factor(final_smoking_status,levels=c(1,2,4)),
    low_hb =   factor(low_hb,levels=c(0,1)),
    h_pylori_flag =   factor(h_pylori_flag,levels=c(0,1)),
    elix_band = factor(elix_band,levels=c("[0,1]","(1,3]","(3,5]","(5,25]"))
  ) %>%
  mutate(
    weight_loss = as.factor(0), 
    dyspepsia = as.factor(0), 
    dysphagia = as.factor(0), 
    fatigue = as.factor(0), 
    cough = as.factor(0), 
    vomiting = as.factor(0), 
    abd_pain = as.factor(0), 
    raise_platelets = as.factor(0), 
    raise_wbc = as.factor(0),
    oesoph_ulc_bin = as.factor(0), 
    barretts_bin = as.factor(0), 
    gord_bin = as.factor(0), 
    gastritis_duodenitis_bin = as.factor(0), 
    hernia_abdo_bin = as.factor(0), 
    #appetite_loss = as.factor(0)
  ) %>%
  mutate(
    weight_loss = factor(weight_loss,levels=c(0,1)),
    dyspepsia =   factor(dyspepsia,levels=c(0,1)),
    dysphagia =   factor(dysphagia,levels=c(0,1)),
    fatigue =   factor(fatigue,levels=c(0,1)),
    cough =   factor(cough,levels=c(0,1)),
    vomiting =   factor(vomiting,levels=c(0,1)),
    abd_pain =   factor(abd_pain,levels=c(0,1)),
    raise_platelets =  factor(raise_platelets,levels=c(0,1)),
    raise_wbc =   factor(raise_wbc,levels=c(0,1)),
    #appetite_loss =   factor(appetite_loss,levels=c(0,1)),
    oesoph_ulc_bin =   factor(oesoph_ulc_bin,levels=c(0,1)),
    barretts_bin =   factor(barretts_bin,levels=c(0,1)),
    gord_bin =   factor(gord_bin,levels=c(0,1)),
    gastritis_duodenitis_bin =  factor(gastritis_duodenitis_bin,levels=c(0,1)),
    hernia_abdo_bin =   factor(hernia_abdo_bin,levels=c(0,1))
  ) %>%
  mutate(e_patid = 0) %>%
  mutate(og_cancer = NA) %>%
  mutate(cohort = 0) %>%
  mutate(cohort=factor(cohort,levels=c(0,1,2,3,4,5,6,7,8,9)))

# Want to consider both genders, so duplicate
pred_data = pred_data |>
  bind_rows(pred_data |> mutate(gender = as.factor(1))) 
#------------------------------------------
#geting predicted risk to plot the main graphs
#setting for men then women
spline_mod3 <- readRDS('S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\spline_model_men.rda')

preds_1 <- predict(spline_mod3,pred_data %>% mutate(cohort_1 = 1),type='response')
preds_2 <- predict(spline_mod3,pred_data %>% mutate(cohort_2 = 1),type='response')
preds_3 <- predict(spline_mod3,pred_data %>% mutate(cohort_3 = 1),type='response')
preds_4 <- predict(spline_mod3,pred_data %>% mutate(cohort_4 = 1),type='response')
preds_5 <- predict(spline_mod3,pred_data %>% mutate(cohort_5 = 1),type='response')
preds_6 <- predict(spline_mod3,pred_data %>% mutate(cohort_6 = 1),type='response')
preds_7 <- predict(spline_mod3,pred_data %>% mutate(cohort_7 = 1),type='response')
preds_8 <- predict(spline_mod3,pred_data %>% mutate(cohort_8 = 1),type='response')
preds_9 <- predict(spline_mod3,pred_data %>% mutate(cohort_9 = 1),type='response')
preds_0 <- predict(spline_mod3,pred_data %>% mutate(cohort_0 = 1),type='response')

#------------------------------------------
#predicted data
# preds_1 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor("1")),type='response')
# preds_2 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort_2 = as.factor("1")),type='response')
# preds_3 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(3)),type='response')
# preds_4 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(4)),type='response')
# preds_5 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(5)),type='response')
# preds_6 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(6)),type='response')
# preds_7 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(7)),type='response')
# preds_8 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(8)),type='response')
# preds_9 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(9)),type='response')
# preds_0 <- predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(0)),type='response')


#-----------------
plot_comb <- function(preds,cohort_name,oddssheetname,risksheetname,filepath,filename){
#random sample data
  #pred_data <- preds_1
  random_data <- cbind(pred_data,preds_0,row.names = NULL)
  names(random_data)[37] <- "Risk"
  random_data <- as.data.frame(random_data)
  
  random_data <- random_data %>% 
    select(c(age,gender,Risk)) %>%
    arrange(gender) %>%
    mutate(Baseline = "Random sample") %>%
    mutate(Risk = Risk * 100)
  
  levels(random_data$gender) <- c("Men","Women")
  #--------------------------------------
  
  #the main cohort data
  #preds <- preds_1
  predict_data <- cbind(pred_data,preds,row.names = NULL)
names(predict_data)[37] <- "pred"
predict_data <- as.data.frame(predict_data)

predict_data <- predict_data %>% 
  select(c(age,gender,pred)) %>%
  arrange(gender)

predict_data <- predict_data %>%
  mutate(odds = pred/(1-pred))

generate_multiplication_table <- function(df1, column1, df2, column2){
  multiplication_table <- as.matrix(df1[[column1]]) %*% t(as.matrix(df2[[column2]]))
  colnames(multiplication_table) <- df2$variable
  final_table <- cbind(predict_data,multiplication_table)
  return(final_table)
}

out <- tidy(spline_mod3)
out$estimate_e <- round(exp(out$estimate),3)

out_fil <- as.data.frame(cbind(out$term,out$estimate_e,out$p.value))
names(out_fil) <- c("variable","estimate_e","p_val")

out_fil2 <- out_fil[5:40,]

out_fil2$estimate_e <- as.numeric(out_fil2$estimate_e)

xlsx::write.xlsx(out_fil2,file=".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Regression risk outputs combined model sex specific.xlsx",sheetName = oddssheetname,append =TRUE)

tab <- generate_multiplication_table(predict_data,"odds",out_fil2,"estimate_e")

risk_tab <- cbind(tab[,1:4],(tab[,5:ncol(tab)]/(1+tab[5:ncol(tab)])))

#calculating CIS
# z_val <- qnorm(0.975)
# risk_tab$logit_lwr <- preds_1_link$fit - z_val * preds_1_link$se.fit
# risk_tab$logit_upr <- preds_1_link$fit + z_val * preds_1_link$se.fit
# 
# risk_tab$predicted_risk_lwr <- plogis(risk_tab$logit_lwr)
# risk_tab$predicted_risk_upr <- plogis(risk_tab$logit_upr)

xlsx::write.xlsx(risk_tab,file=".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Regression risk outputs combined model sex specific.xlsx",sheetName = risksheetname,append =TRUE)

#risk_tab_simp <- cbind(risk_tab[,1:4],risk_tab[,11:ncol(risk_tab)])
#----------------

#keeping only significant values
out_fil2$p_val <- as.numeric(out_fil2$p_val)
out_fil2$sig <- ifelse(round(out_fil2$p_val,2) <= 0.05,1,0)

out_fil2 <- out_fil2 %>%
  filter(sig == 1)

tab2 <- generate_multiplication_table(predict_data,"odds",out_fil2,"estimate_e")

risk_tab2 <- cbind(tab2[,1:4],(tab2[,5:ncol(tab2)]/(1+tab2[5:ncol(tab2)])))

long_df <- pivot_longer(risk_tab2,cols=-c(age,gender),names_to = "Covariate",values_to = "Risk")

levels(long_df$gender) <- c("Men","Women")
#levels(long_df$cat) <- c("Background risk",var_name)

long_df$Risk <- long_df$Risk * 100

#nb.cols <- 23
#myColours <- colorRampPalette(brewer.pal(11,'Set3'),bias=2)(nb.cols)
baseline_data <- long_df %>% filter(Covariate == "pred")
#names(baseline_data)[4] <- "risk"
names(baseline_data)[3] <- "Baseline"
baseline_data <- baseline_data %>% mutate(Baseline = cohort_var) %>%
  union(random_data) %>%
  mutate(Baseline = factor(Baseline, levels = c( "Random sample",cohort_var)))

long_df <- long_df %>% filter(Covariate != "pred")
long_df$Covariate <- #ifelse(long_df$Covariate == "pred","Baseline risk",
  ifelse(long_df$Covariate == "final_smoking_status2","Ex-smoker",
         ifelse(long_df$Covariate == "final_smoking_status4","Current smoker",
                ifelse(long_df$Covariate == "bmi_cat1","Underweight",
                       ifelse(long_df$Covariate == "bmi_cat2","Overweight",
                              ifelse(long_df$Covariate == "bmi_cat3","Obese",
                                     ifelse(long_df$Covariate == "low_hb1","Anaemia",
                                            ifelse(long_df$Covariate == "weight_loss1","Weight loss",
                                                   ifelse(long_df$Covariate == "dyspepsia1","Dyspepsia",
                                                          ifelse(long_df$Covariate == "dysphagia1","Dysphagia",
                                                                 ifelse(long_df$Covariate == "fatigue1","Fatigue",
                                                                        ifelse(long_df$Covariate == "cough1","Cough",
                                                                               ifelse(long_df$Covariate == "alc_status_comb2","Ever drinker",
                                                                                      ifelse(long_df$Covariate == "alc_status_comb3","Ever drinker with alc problems",
                                                                                             ifelse(long_df$Covariate == "gord_bin1","GORD",
                                                                                                    ifelse(long_df$Covariate == "gastritis_duodenitis_bin1","Gastritis/Duodenitis",
                                                                                                           ifelse(long_df$Covariate == "hernia_abdo_bin1","Diaphragmatic hernia",
                                                                                                                  ifelse(long_df$Covariate == "barretts_bin1","Barretts",
                                                                                                                         ifelse(long_df$Covariate == "h_pylori_flag1","H.pylori treatment",
                                                                                                                         ifelse(long_df$Covariate == "elix_band(1,3]","Elix score 1-2",
                                                                                                                                ifelse(long_df$Covariate == "elix_band(3,5]","Elix score 3-4",
                                                                                                                                       ifelse(long_df$Covariate == "elix_band(5,25]","Elix score 5+",
                                                                                                                                              ifelse(long_df$Covariate == "vomiting1","Vomiting",
                                                                                                                                                     ifelse(long_df$Covariate == "abd_pain1","Upper abdominal pain",
                                                                                                                                                            ifelse(long_df$Covariate == "appetite_loss1","Appetite loss",
                                                                                                                                                                   ifelse(long_df$Covariate == "symp_n","Symptom count",
                                                                                                                                                                          ifelse(long_df$Covariate == "raise_platelets1","Raised platelets",
                                                                                                                                                                                 ifelse(long_df$Covariate == "raise_wbc1","Raised WBCs",
                                                                                                                                                                                        ifelse(long_df$Covariate == "oesoph_ulc_bin1","Oesophageal ulcer",
                                                                                                                                                                                               NA
                                                                                                                                                                                        ))))))))))))))))))))))))))))
#making the baseline risk category flag to change thickness on graph
# long_df <- long_df %>% mutate(bold = ifelse(Covariate == "Baseline risk",1,0)) %>%
#   mutate(bold = as.factor(Covariate) %>% fct_other(keep=c("Baseline risk"),other_level="Other keys"))

long_df <- long_df %>% 
  filter(!is.na(Covariate)) %>% 
  #filter(cat != "Background risk") %>%
  mutate('Co-occurring feature' = Covariate) %>%
  group_by(gender,age) %>%
  mutate(`Co-occurring feature` = fct_reorder(`Co-occurring feature`,Risk,.fun=desc)) %>%
  ungroup()

#creating a list of colours for each variable
colour_vec <- c("#0066CC","#E31A1C","green4","#6A3D9A","#FF7F00","gold","skyblue2","#FB9A99","palegreen","#CAB2D6",
                "#FDBF6F","gray70","khaki2","maroon","orchid1","deeppink1","yellow4","yellow3","darkorange4","brown",
                "#330055","#002355","#003300",'aquamarine2','seagreen',"#660000",'#ff5000'
)

names(colour_vec) <- c("Dyspepsia","Dysphagia","Vomiting","Weight loss","Anaemia","Raised platelets","Raised WBCs","Upper abdominal pain",
                       "GORD","Gastritis/duodenitis","Barretts","Cough","Oesophageal ulcer","Diaphragmatic hernia","Fatigue","Appetite loss",
                       "Ex-smoker","Current smoker","Elix score 1-2","Elix score 3-4","Elix score 5+","Underweight","Overweight",
                       "Obese","Ever drinker","Ever drinker with alc problems","H.pylori treatment")


plot <- ggplot()+
  geom_line(long_df,mapping=aes(age,Risk,colour=`Co-occurring feature`))+
  labs(fill='Co-occurring feature') +
  geom_line(data=baseline_data,mapping=aes(age,Risk,linetype=Baseline,group=Baseline)) +
  geom_hline(yintercept=3,linetype='twodash',color='red') +
  #facet_grid(c('gender'))+
  ylab("Risk (%)")+#+geom_point(aes(shape=long_df$bold))+
  scale_colour_manual(values=colour_vec)+
  theme_bw()#name=long_df$`Co-occurring feature`,
ggsave(plot,path=filepath,filename=filename,width=8,height=8,dpi=200)
}

filepath <- "S:/ECHO_IHI_CPRD/Freya/Project 1/Count Tables & Graphs/Study 1 outputs/Results 21042025"

cohort_var <- "GORD"
odds <- "GORD odds men"
risk <- "GORD risk men"
#plot_comb(preds_1,cohort_var,odds,risk,filepath,"combined_gord_drop_df.jpeg")
plot_comb(preds_1,cohort_var,odds,risk,filepath,"combined_gord_men.jpeg")

cohort_var <- "Oesophageal ulcer"
odds <- "oesoph_ulc odds men"
risk <- "oesoph_ulc risk men"
#plot_comb(preds_2,cohort_var,odds,risk,filepath,"combined_oesoph_ulc_drop_df.jpeg")
plot_comb(preds_2,cohort_var,odds,risk,filepath,"combined_oesoph_ulc_men.jpeg")

cohort_var <- "Gastritis/duodenitis"
odds <- "Gastritis odds men"
risk <- "Gastritis risk men"
#plot_comb(preds_3,cohort_var,odds,risk,filepath,"combined_gastritis_drop_df.jpeg")
plot_comb(preds_3,cohort_var,odds,risk,filepath,"combined_gastritis_men.jpeg")

cohort_var <- "Diaphgramatic hernia"
odds <- "hernia odds men"
risk <- "hernia risk men"
#plot_comb(preds_4,cohort_var,odds,risk,filepath,"combined_hernia_drop_df.jpeg")
plot_comb(preds_4,cohort_var,odds,risk,filepath,"combined_hernia_men.jpeg")

cohort_var <- "Barrett's oesophagus"
odds <- "Barrett odds men"
risk <- "Barrett risk men"
#plot_comb(preds_5,cohort_var,odds,risk,filepath,"combined_barretts_drop_df.jpeg")
plot_comb(preds_5,cohort_var,odds,risk,filepath,"combined_barretts_men.jpeg")

cohort_var <- "Dysphagia"
odds <- "Dysphagia odds men"
risk <- "Dysphagia risk men"
#plot_comb(preds_6,cohort_var,odds,risk,filepath,"combined_dysphagia_drop_df.jpeg")
plot_comb(preds_6,cohort_var,odds,risk,filepath,"combined_dysphagia_men.jpeg")

cohort_var <- "Dyspepsia"
odds <- "Dyspepsia odds men"
risk <- "Dyspepsia risk men"
#plot_comb(preds_7,cohort_var,odds,risk,filepath,"combined_dyspepsia_df.jpeg")
plot_comb(preds_7,cohort_var,odds,risk,filepath,"combined_dyspepsia_men.jpeg")

cohort_var <- "Upper abdominal pain"
odds <- "abdominal pain odds men"
risk <- "abdominal pain risk men"
#plot_comb(preds_8,cohort_var,odds,risk,filepath,"combined_abd_pain_drop_df.jpeg")
plot_comb(preds_8,cohort_var,odds,risk,filepath,"combined_abd_pain_men.jpeg")

cohort_var <- "Vomiting"
odds <- "Vomiting odds men"
risk <- "Vomiting risk men"
#plot_comb(preds_9,cohort_var,odds,risk,filepath,"combined_vomiting_drop_df.jpeg")
plot_comb(preds_9,cohort_var,odds,risk,filepath,"combined_vomiting_men.jpeg")

#-----------------
#REPEAT FOR WOMEN
spline_mod3 <- readRDS('S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\spline_model_women.rda')

preds_1 <- predict(spline_mod3,pred_data %>% mutate(cohort_1 = 1),type='response')
preds_2 <- predict(spline_mod3,pred_data %>% mutate(cohort_2 = 1),type='response')
preds_3 <- predict(spline_mod3,pred_data %>% mutate(cohort_3 = 1),type='response')
preds_4 <- predict(spline_mod3,pred_data %>% mutate(cohort_4 = 1),type='response')
preds_5 <- predict(spline_mod3,pred_data %>% mutate(cohort_5 = 1),type='response')
preds_6 <- predict(spline_mod3,pred_data %>% mutate(cohort_6 = 1),type='response')
preds_7 <- predict(spline_mod3,pred_data %>% mutate(cohort_7 = 1),type='response')
preds_8 <- predict(spline_mod3,pred_data %>% mutate(cohort_8 = 1),type='response')
preds_9 <- predict(spline_mod3,pred_data %>% mutate(cohort_9 = 1),type='response')
preds_0 <- predict(spline_mod3,pred_data %>% mutate(cohort_0 = 1),type='response')

cohort_var <- "GORD"
odds <- "GORD odds women"
risk <- "GORD risk women"
#plot_comb(preds_1,cohort_var,odds,risk,filepath,"combined_gord_drop_df.jpeg")
plot_comb(preds_1,cohort_var,odds,risk,filepath,"combined_gord_women.jpeg")

cohort_var <- "Oesophageal ulcer"
odds <- "oesoph_ulc odds women"
risk <- "oesoph_ulc risk women"
#plot_comb(preds_2,cohort_var,odds,risk,filepath,"combined_oesoph_ulc_drop_df.jpeg")
plot_comb(preds_2,cohort_var,odds,risk,filepath,"combined_oesoph_ulc_women.jpeg")

cohort_var <- "Gastritis/duodenitis"
odds <- "Gastritis odds women"
risk <- "Gastritis risk women"
#plot_comb(preds_3,cohort_var,odds,risk,filepath,"combined_gastritis_drop_df.jpeg")
plot_comb(preds_3,cohort_var,odds,risk,filepath,"combined_gastritis_women.jpeg")

cohort_var <- "Diaphgramatic hernia"
odds <- "hernia odds women"
risk <- "hernia risk women"
#plot_comb(preds_4,cohort_var,odds,risk,filepath,"combined_hernia_drop_df.jpeg")
plot_comb(preds_4,cohort_var,odds,risk,filepath,"combined_hernia_women.jpeg")

cohort_var <- "Barrett's oesophagus"
odds <- "Barrett odds women"
risk <- "Barrett risk women"
#plot_comb(preds_5,cohort_var,odds,risk,filepath,"combined_barretts_drop_df.jpeg")
plot_comb(preds_5,cohort_var,odds,risk,filepath,"combined_barretts_women.jpeg")

cohort_var <- "Dysphagia"
odds <- "Dysphagia odds women"
risk <- "Dysphagia risk women"
#plot_comb(preds_6,cohort_var,odds,risk,filepath,"combined_dysphagia_drop_df.jpeg")
plot_comb(preds_6,cohort_var,odds,risk,filepath,"combined_dysphagia_women.jpeg")

cohort_var <- "Dyspepsia"
odds <- "Dyspepsia odds women"
risk <- "Dyspepsia risk women"
#plot_comb(preds_7,cohort_var,odds,risk,filepath,"combined_dyspepsia_df.jpeg")
plot_comb(preds_7,cohort_var,odds,risk,filepath,"combined_dyspepsia_women.jpeg")

cohort_var <- "Upper abdominal pain"
odds <- "abdominal pain odds women"
risk <- "abdominal pain risk women"
#plot_comb(preds_8,cohort_var,odds,risk,filepath,"combined_abd_pain_drop_df.jpeg")
plot_comb(preds_8,cohort_var,odds,risk,filepath,"combined_abd_pain_women.jpeg")

cohort_var <- "Vomiting"
odds <- "Vomiting odds women"
risk <- "Vomiting risk women"
#plot_comb(preds_9,cohort_var,odds,risk,filepath,"combined_vomiting_drop_df.jpeg")
plot_comb(preds_9,cohort_var,odds,risk,filepath,"combined_vomiting_women.jpeg")

#--------------------------------------------------------------
#trying to calculate risk across all ages for each cohort with CIs
spline_mod2 <- readRDS("S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\spline_model_no_sd.rda")
pred_data =
tibble(age = seq(30, 100, length.out = 71)) |>
  mutate(age_idate = (age-60)/10) |>
  mutate(gender = as.factor(0)) |>
  mutate(
    cohort_0 = 0,
    cohort_1 = 0,
    cohort_2 = 0,
    cohort_3 = 0,
    cohort_4 = 0,
    cohort_5 = 0,
    cohort_6 = 0,
    cohort_7 = 0,
    cohort_8 = 0,
    cohort_9 = 0  ) |>
  mutate(
    bmi_cat = as.factor(0),
    alc_status_comb = as.factor(1),
    final_smoking_status = as.factor(1),
    low_hb = as.factor(0),
    h_pylori_flag = as.factor(0), 
    elix_band = as.factor("[0,1]")
  ) |>
  mutate(
    bmi_cat = factor(bmi_cat,levels=c(0,1,2,3)),
    alc_status_comb =   factor(alc_status_comb,levels=c(1,2,3)),
    final_smoking_status =   factor(final_smoking_status,levels=c(1,2,4)),
    low_hb =   factor(low_hb,levels=c(0,1)),
    h_pylori_flag =   factor(h_pylori_flag,levels=c(0,1)),
    elix_band = factor(elix_band,levels=c("[0,1]","(1,3]","(3,5]","(5,25]"))
  ) %>%
  mutate(
    weight_loss = as.factor(0), 
    dyspepsia = as.factor(0), 
    dysphagia = as.factor(0), 
    fatigue = as.factor(0), 
    cough = as.factor(0), 
    vomiting = as.factor(0), 
    abd_pain = as.factor(0), 
    raise_platelets = as.factor(0), 
    raise_wbc = as.factor(0),
    oesoph_ulc_bin = as.factor(0), 
    barretts_bin = as.factor(0), 
    gord_bin = as.factor(0), 
    gastritis_duodenitis_bin = as.factor(0), 
    hernia_abdo_bin = as.factor(0), 
    #appetite_loss = as.factor(0)
  ) %>%
  mutate(
    weight_loss = factor(weight_loss,levels=c(0,1)),
    dyspepsia =   factor(dyspepsia,levels=c(0,1)),
    dysphagia =   factor(dysphagia,levels=c(0,1)),
    fatigue =   factor(fatigue,levels=c(0,1)),
    cough =   factor(cough,levels=c(0,1)),
    vomiting =   factor(vomiting,levels=c(0,1)),
    abd_pain =   factor(abd_pain,levels=c(0,1)),
    raise_platelets =  factor(raise_platelets,levels=c(0,1)),
    raise_wbc =   factor(raise_wbc,levels=c(0,1)),
    #appetite_loss =   factor(appetite_loss,levels=c(0,1)),
    oesoph_ulc_bin =   factor(oesoph_ulc_bin,levels=c(0,1)),
    barretts_bin =   factor(barretts_bin,levels=c(0,1)),
    gord_bin =   factor(gord_bin,levels=c(0,1)),
    gastritis_duodenitis_bin =  factor(gastritis_duodenitis_bin,levels=c(0,1)),
    hernia_abdo_bin =   factor(hernia_abdo_bin,levels=c(0,1))
  ) %>%
  mutate(e_patid = 0) %>%
  mutate(og_cancer = NA) #%>%
  #mutate(cohort = 0) %>%
  #mutate(cohort=factor(cohort,levels=c(0,1,2,3,4,5,6,7,8,9)))

# Want to consider both genders, so duplicate
pred_data = pred_data |>
  bind_rows(pred_data |> mutate(gender = as.factor(1))) 

#taking mean age
pred_data_agg <- pred_data %>%
  filter(age==65)

#data for calculating CIs
# preds_1_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_1_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), dysphagia = as.factor("1")),type='link',se.fit=TRUE)
# preds_1_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), abd_pain = as.factor("1")),type='link',se.fit=TRUE)
# preds_1_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_1_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), vomiting = as.factor("1")),type='link',se.fit=TRUE)
# preds_1_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_1_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_1_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_1_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_1_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("1"), gord = as.factor("1")),type='link',se.fit=TRUE)
# 
# 
# preds_2_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_2_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), dysphagia = as.factor("1")),type='link',se.fit=TRUE)
# preds_2_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), abd_pain = as.factor("1")),type='link',se.fit=TRUE)
# preds_2_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_2_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), vomiting = as.factor("1")),type='link',se.fit=TRUE)
# preds_2_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_2_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_2_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_2_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_2_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = as.factor("1"), gord = as.factor("1")),type='link',se.fit=TRUE)
# 
# preds_3_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_3_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), dysphagia = as.factor("1")),type='link',se.fit=TRUE)
# preds_3_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), abd_pain = as.factor("1")),type='link',se.fit=TRUE)
# preds_3_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_3_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), vomiting = as.factor("1")),type='link',se.fit=TRUE)
# preds_3_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_3_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_3_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_3_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_3_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("3"), gord = as.factor("1")),type='link',se.fit=TRUE)
# 
# preds_4_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_4_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), dysphagia = as.factor("1")),type='link',se.fit=TRUE)
# preds_4_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), abd_pain = as.factor("1")),type='link',se.fit=TRUE)
# preds_4_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_4_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), vomiting = as.factor("1")),type='link',se.fit=TRUE)
# preds_4_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_4_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_4_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_4_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_4_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("4"), gord = as.factor("1")),type='link',se.fit=TRUE)
# 
# preds_5_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_5_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), dysphagia = as.factor("1")),type='link',se.fit=TRUE)
# preds_5_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), abd_pain = as.factor("1")),type='link',se.fit=TRUE)
# preds_5_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_5_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), vomiting = as.factor("1")),type='link',se.fit=TRUE)
# preds_5_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_5_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_5_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_5_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_5_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("5"), gord = as.factor("1")),type='link',se.fit=TRUE)
# 
# preds_6_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_6_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), dysphagia = as.factor("1")),type='link',se.fit=TRUE)
# preds_6_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), abd_pain = as.factor("1")),type='link',se.fit=TRUE)
# preds_6_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_6_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), vomiting = as.factor("1")),type='link',se.fit=TRUE)
# preds_6_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_6_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_6_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_6_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_6_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("6"), gord = as.factor("1")),type='link',se.fit=TRUE)
# 
# preds_7_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_7_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), dysphagia = as.factor("1")),type='link',se.fit=TRUE)
# preds_7_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), abd_pain = as.factor("1")),type='link',se.fit=TRUE)
# preds_7_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_7_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), vomiting = as.factor("1")),type='link',se.fit=TRUE)
# preds_7_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_7_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_7_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_7_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_7_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("7"), gord = as.factor("1")),type='link',se.fit=TRUE)
# 
# preds_8_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_8_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), dysphagia = as.factor("1")),type='link',se.fit=TRUE)
# preds_8_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), abd_pain = as.factor("1")),type='link',se.fit=TRUE)
# preds_8_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_8_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), vomiting = as.factor("1")),type='link',se.fit=TRUE)
# preds_8_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_8_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_8_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_8_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_8_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("8"), gord = as.factor("1")),type='link',se.fit=TRUE)
# 
# preds_9_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_9_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), dysphagia = as.factor("1")),type='link',se.fit=TRUE)
# preds_9_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), abd_pain = as.factor("1")),type='link',se.fit=TRUE)
# preds_9_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_9_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), vomiting = as.factor("1")),type='link',se.fit=TRUE)
# preds_9_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_9_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_9_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
# preds_9_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
# preds_9_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort = as.factor("9"), gord = as.factor("1")),type='link',se.fit=TRUE)

preds_1_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_1 = 1, dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
preds_1_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_1 = 1, dysphagia = as.factor("1")),type='link',se.fit=TRUE)
preds_1_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_1 = 1, abd_pain = as.factor("1")),type='link',se.fit=TRUE)
preds_1_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_1 = 1, gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_1_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_1 = 1, vomiting = as.factor("1")),type='link',se.fit=TRUE)
preds_1_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_1 = 1, barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_1_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_1 = 1, oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_1_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_1 = 1, hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_1_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_1 = 1, gord_bin = as.factor("1")),type='link',se.fit=TRUE)


preds_2_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = 1, dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
preds_2_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = 1, dysphagia = as.factor("1")),type='link',se.fit=TRUE)
preds_2_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = 1, abd_pain = as.factor("1")),type='link',se.fit=TRUE)
preds_2_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = 1, gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_2_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = 1, vomiting = as.factor("1")),type='link',se.fit=TRUE)
preds_2_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = 1, barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_2_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = 1, oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_2_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = 1, hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_2_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_2 = 1, gord_bin = as.factor("1")),type='link',se.fit=TRUE)

preds_3_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_3 = 1, dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
preds_3_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_3 = 1, dysphagia = as.factor("1")),type='link',se.fit=TRUE)
preds_3_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_3 = 1, abd_pain = as.factor("1")),type='link',se.fit=TRUE)
preds_3_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_3 = 1, gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_3_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_3 = 1, vomiting = as.factor("1")),type='link',se.fit=TRUE)
preds_3_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_3 = 1, barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_3_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_3 = 1, oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_3_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_3 = 1, hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_3_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_3 = 1, gord_bin = as.factor("1")),type='link',se.fit=TRUE)

preds_4_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_4 = 1, dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
preds_4_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_4 = 1, dysphagia = as.factor("1")),type='link',se.fit=TRUE)
preds_4_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_4 = 1, abd_pain = as.factor("1")),type='link',se.fit=TRUE)
preds_4_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_4 = 1, gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_4_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_4 = 1, vomiting = as.factor("1")),type='link',se.fit=TRUE)
preds_4_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_4 = 1, barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_4_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_4 = 1, oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_4_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_4 = 1, hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_4_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_4 = 1, gord_bin = as.factor("1")),type='link',se.fit=TRUE)

preds_5_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_5 = 1, dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
preds_5_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_5 = 1, dysphagia = as.factor("1")),type='link',se.fit=TRUE)
preds_5_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_5 = 1, abd_pain = as.factor("1")),type='link',se.fit=TRUE)
preds_5_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_5 = 1, gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_5_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_5 = 1, vomiting = as.factor("1")),type='link',se.fit=TRUE)
preds_5_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_5 = 1, barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_5_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_5 = 1, oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_5_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_5 = 1, hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_5_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_5 = 1, gord_bin = as.factor("1")),type='link',se.fit=TRUE)

preds_6_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_6 = 1, dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
preds_6_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_6 = 1, dysphagia = as.factor("1")),type='link',se.fit=TRUE)
preds_6_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_6 = 1, abd_pain = as.factor("1")),type='link',se.fit=TRUE)
preds_6_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_6 = 1, gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_6_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_6 = 1, vomiting = as.factor("1")),type='link',se.fit=TRUE)
preds_6_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_6 = 1, barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_6_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_6 = 1, oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_6_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_6 = 1, hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_6_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_6 = 1, gord_bin = as.factor("1")),type='link',se.fit=TRUE)

preds_7_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_7 = 1, dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
preds_7_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_7 = 1, dysphagia = as.factor("1")),type='link',se.fit=TRUE)
preds_7_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_7 = 1, abd_pain = as.factor("1")),type='link',se.fit=TRUE)
preds_7_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_7 = 1, gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_7_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_7 = 1, vomiting = as.factor("1")),type='link',se.fit=TRUE)
preds_7_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_7 = 1, barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_7_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_7 = 1, oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_7_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_7 = 1, hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_7_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_7 = 1, gord_bin = as.factor("1")),type='link',se.fit=TRUE)

preds_8_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_8 = 1, dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
preds_8_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_8 = 1, dysphagia = as.factor("1")),type='link',se.fit=TRUE)
preds_8_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_8 = 1, abd_pain = as.factor("1")),type='link',se.fit=TRUE)
preds_8_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_8 = 1, gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_8_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_8 = 1, vomiting = as.factor("1")),type='link',se.fit=TRUE)
preds_8_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_8 = 1, barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_8_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_8 = 1, oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_8_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_8 = 1, hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_8_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_8 = 1, gord_bin = as.factor("1")),type='link',se.fit=TRUE)

preds_9_link_dysp <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_9 = 1, dyspepsia = as.factor("1")),type='link',se.fit=TRUE)
preds_9_link_dysph <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_9 = 1, dysphagia = as.factor("1")),type='link',se.fit=TRUE)
preds_9_link_abd <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_9 = 1, abd_pain = as.factor("1")),type='link',se.fit=TRUE)
preds_9_link_gast <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_9 = 1, gastritis_duodenitis_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_9_link_vom <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_9 = 1, vomiting = as.factor("1")),type='link',se.fit=TRUE)
preds_9_link_barretts <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_9 = 1, barretts_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_9_link_oes <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_9 = 1, oesoph_ulc_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_9_link_hern <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_9 = 1, hernia_abdo_bin = as.factor("1")),type='link',se.fit=TRUE)
preds_9_link_gord <- predict(spline_mod2,pred_data_agg %>% mutate(cohort_9 = 1, gord_bin = as.factor("1")),type='link',se.fit=TRUE)

#---------------------
# random_data <- cbind(pred_data,preds_0_link$fit,row.names = NULL)
# names(random_data)[37] <- "Risk"
# random_data <- as.data.frame(random_data)
# 
# random_data <- cbind(random_data,preds_0_link$se.fit)
# names(random_data)[38] <- "SE"
# 
# random_data <- random_data %>% 
#   select(c(age,gender,Risk,SE)) %>%
#   arrange(gender) %>%
#   mutate(Baseline = "Random sample")# %>%
#   #mutate(Risk = Risk * 100)
# 
# levels(random_data$gender) <- c("Men","Women")
#--------------------------------------

get_fits <- function(preds_data_bar,preds_data_hern,preds_data_dysp,preds_data_dysph,preds_data_gast,preds_data_gord,preds_data_oes,preds_data_abd,preds_data_vom,
         cohort_var) {
fit_tab <- as.data.frame(rbind(unlist(preds_data_bar),
                 unlist(preds_data_hern),
                 unlist(preds_data_dysp),
                 unlist(preds_data_dysph),
                 unlist(preds_data_gast),
                 unlist(preds_data_gord),
                 unlist(preds_data_oes),
                 unlist(preds_data_abd),
                 unlist(preds_data_vom)))

fit_tab <- fit_tab %>%
  select(-residual.scale) %>%
  mutate(Category = c(paste0(cohort_var," with Barrett's oesophagus"),
                      paste0(cohort_var," with diaphragmatic hernia"),
                             paste0(cohort_var," with dyspepsia"),
                                    paste0(cohort_var," with dysphagia"),
                                           paste0(cohort_var," with gastritis/duodenitis"),
                      paste0(cohort_var," with GORD"),
                                                  paste0(cohort_var," with oesophageal ulcer"),
                                                         paste0(cohort_var," with upper abdominal pain"),
                                                                paste0(cohort_var," with vomiting")))

out1 <- fit_tab %>% 
  select(fit.1,se.fit.1,Category) %>%
  mutate(lb = fit.1 - 1.96*se.fit.1) %>%
  mutate(ub = fit.1 + 1.96*se.fit.1) %>%
  mutate(fit_exp = exp(fit.1)) %>%
  mutate(lb_exp = exp(lb)) %>%
  mutate(ub_exp = exp(ub)) %>%
  select(Category,fit_exp,lb_exp,ub_exp)

out2 <- fit_tab %>% 
  select(fit.2,se.fit.2,Category) %>%
  mutate(lb = fit.2 - 1.96*se.fit.2) %>%
  mutate(ub = fit.2 + 1.96*se.fit.2) %>%
  mutate(fit_exp = exp(fit.2)) %>%
  mutate(lb_exp = exp(lb)) %>%
  mutate(ub_exp = exp(ub)) %>%
  select(Category,fit_exp,lb_exp,ub_exp)

return(list(out1,out2))

}

cohort_var <- "GORD"
gord_out <- get_fits(preds_1_link_barretts,preds_1_link_hern,preds_1_link_dysp,preds_1_link_dysph,preds_1_link_gast,preds_1_link_gord,
                      preds_1_link_oes,preds_1_link_abd,preds_1_link_vom,cohort_var)

cohort_var <- "Oesophageal ulcer"
oesoph_out <- get_fits(preds_2_link_barretts,preds_2_link_hern,preds_2_link_dysp,preds_2_link_dysph,preds_2_link_gast,preds_2_link_gord,
                      preds_2_link_oes,preds_2_link_abd,preds_2_link_vom,cohort_var)

cohort_var <- "Gastritis/duodenitis"
gas_out <- get_fits(preds_3_link_barretts,preds_3_link_hern,preds_3_link_dysp,preds_3_link_dysph,preds_3_link_gast,preds_3_link_gord,
                      preds_3_link_oes,preds_3_link_abd,preds_3_link_vom,cohort_var)

cohort_var <- "Diaphragmatic hernia"
hern_out <- get_fits(preds_4_link_barretts,preds_4_link_hern,preds_4_link_dysp,preds_4_link_dysph,preds_4_link_gast,preds_4_link_gord,
                      preds_4_link_oes,preds_4_link_abd,preds_4_link_vom,cohort_var)

cohort_var <- "Barrett's oesophagus"
barr_out <- get_fits(preds_5_link_barretts,preds_5_link_hern,preds_5_link_dysp,preds_5_link_dysph,preds_5_link_gast,preds_5_link_gord,
                      preds_5_link_oes,preds_5_link_abd,preds_5_link_vom,cohort_var)

cohort_var <- "Dysphagia"
dysph_out <- get_fits(preds_6_link_barretts,preds_6_link_hern,preds_6_link_dysp,preds_6_link_dysph,preds_6_link_gast,preds_6_link_gord,
                      preds_6_link_oes,preds_6_link_abd,preds_6_link_vom,cohort_var)

cohort_var <- "Dyspepsia"
dysp_out <- get_fits(preds_7_link_barretts,preds_7_link_hern,preds_7_link_dysp,preds_7_link_dysph,preds_7_link_gast,preds_7_link_gord,
                      preds_7_link_oes,preds_7_link_abd,preds_7_link_vom,cohort_var)

cohort_var <- "Upper abdominal pain"
abd_pain_out <- get_fits(preds_8_link_barretts,preds_8_link_hern,preds_8_link_dysp,preds_8_link_dysph,preds_8_link_gast,preds_8_link_gord,
                      preds_8_link_oes,preds_8_link_abd,preds_8_link_vom,cohort_var)

cohort_var <- "Vomiting"
vom_out <- get_fits(preds_9_link_barretts,preds_9_link_hern,preds_9_link_dysp,preds_9_link_dysph,preds_9_link_gast,preds_9_link_gord,
                      preds_9_link_oes,preds_9_link_abd,preds_9_link_vom,cohort_var)


all_gord_f <- as.data.frame(gord_out[2])
all_oesoph_ulc_f <- as.data.frame(oesoph_out[2])
all_gastritis_duodenitis_f <- as.data.frame(gas_out[2])
all_barretts_f <- as.data.frame(barr_out[2])
all_hernia_abdo_f <- as.data.frame(hern_out[2])
all_dyspepsia_f <- as.data.frame(dysp_out[2])
all_dysphagia_f <- as.data.frame(dysph_out[2])
all_abd_pain_f <- as.data.frame(abd_pain_out[2])
all_vomiting_f <- as.data.frame(vom_out[2])

all_gord_m <- as.data.frame(gord_out[1])
all_oesoph_ulc_m <- as.data.frame(oesoph_out[1])
all_gastritis_duodenitis_m <- as.data.frame(gas_out[1])
all_barretts_m <- as.data.frame(barr_out[1])
all_hernia_abdo_m <- as.data.frame(hern_out[1])
all_dyspepsia_m <- as.data.frame(dysp_out[1])
all_dysphagia_m <- as.data.frame(dysph_out[1])
all_abd_pain_m <- as.data.frame(abd_pain_out[1])
all_vomiting_m <- as.data.frame(vom_out[1])


#------------------------------------------------------------
##OVERALL 
#For women
all_data_ordered_f <- rbind(all_barretts_f[2,],
                            all_hernia_abdo_f[1,],
                            all_barretts_f[3,],
                            all_dyspepsia_f[1,],
                            all_barretts_f[4,],
                            all_dysphagia_f[1,],
                            all_barretts_f[5,],
                            all_gastritis_duodenitis_f[1,],
                            all_barretts_f[6,],
                            all_gord_f[1,],
                            all_barretts_f[7,],
                            all_oesoph_ulc_f[1,],
                            all_barretts_f[8,],
                            all_abd_pain_f[1,],
                            all_barretts_f[9,],
                            all_vomiting_f[1,],
                            
                            all_hernia_abdo_f[3,],
                            all_dyspepsia_f[2,],
                            all_hernia_abdo_f[4,],
                            all_dysphagia_f[2,],
                            all_hernia_abdo_f[5,],
                            all_gastritis_duodenitis_f[2,],
                            all_hernia_abdo_f[6,],
                            all_gord_f[2,],
                            all_hernia_abdo_f[7,],
                            all_oesoph_ulc_f[2,],
                            all_hernia_abdo_f[8,],
                            all_abd_pain_f[2,],
                            all_hernia_abdo_f[9,],
                            all_vomiting_f[2,],
                            
                            all_dyspepsia_f[4,],
                            all_dysphagia_f[3,],
                            all_dyspepsia_f[5,],
                            all_gastritis_duodenitis_f[3,],
                            all_dyspepsia_f[6,],
                            all_gord_f[3,],
                            all_dyspepsia_f[7,],
                            all_oesoph_ulc_f[3,],
                            all_dyspepsia_f[8,],
                            all_abd_pain_f[3,],
                            all_dyspepsia_f[9,],
                            all_vomiting_f[3,],
                            
                            all_dysphagia_f[5,],
                            all_gastritis_duodenitis_f[4,],
                            all_dysphagia_f[6,],
                            all_gord_f[4,],
                            all_dysphagia_f[7,],
                            all_oesoph_ulc_f[4,],
                            all_dysphagia_f[8,],
                            all_abd_pain_f[4,],
                            all_dysphagia_f[9,],
                            all_vomiting_f[4,],
                            
                            all_gastritis_duodenitis_f[6,],
                            all_gord_f[5,],
                            all_gastritis_duodenitis_f[7,],
                            all_oesoph_ulc_f[5,],
                            all_gastritis_duodenitis_f[8,],
                            all_abd_pain_f[5,],
                            all_gastritis_duodenitis_f[9,],
                            all_vomiting_f[5,],
                            
                            all_gord_f[7,],
                            all_oesoph_ulc_f[6,],
                            all_gord_f[8,],
                            all_abd_pain_f[6,],
                            all_gord_f[9,],
                            all_vomiting_f[6,],
                            
                            all_oesoph_ulc_f[8,],
                            all_abd_pain_f[7,],
                            all_oesoph_ulc_f[9,],
                            all_vomiting_f[7,],
                            
                            all_abd_pain_f[9,],
                            all_vomiting_f[8,]
)


all_data_ordered_f$Category <- factor(all_data_ordered_f$Category,levels=all_data_ordered_f$Category)

all_data_ordered_f <- all_data_ordered_f %>%
  mutate(across(where(is.character),as.numeric))

ggplot(data=all_data_ordered_f,mapping=aes(fit_exp*100,Category))+
  geom_point()+
  geom_errorbar(data=all_data_ordered_f,aes(y=Category,
                                            xmin = lb_exp*100,
                                            xmax = ub_exp*100),
                alpha = 0.5) +
  scale_x_continuous(expand=c(0,0),limits=c(0,12))+
  xlab("Risk of OG Cancer (%)") + ylab("Pairwise combination of index features") +
  geom_hline(yintercept=2.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=4.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=6.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=8.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=10.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=12.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=14.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=16.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=18.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=20.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=22.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=24.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=26.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=28.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=30.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=32.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=34.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=36.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=38.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=40.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=42.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=44.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=46.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=48.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=50.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=52.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=54.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=56.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=58.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=60.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=62.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=64.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=66.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=68.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=70.5,linetype='dashed',alpha=0.5)+
  theme_bw()

ggsave(path=filepath,filename="Risk order women drop df.png",height = 12, width = 10,dpi=200)
#For men
all_data_ordered_m <- rbind(all_barretts_m[2,],
                            all_hernia_abdo_m[1,],
                            all_barretts_m[3,],
                            all_dyspepsia_m[1,],
                            all_barretts_m[4,],
                            all_dysphagia_m[1,],
                            all_barretts_m[5,],
                            all_gastritis_duodenitis_m[1,],
                            all_barretts_m[6,],
                            all_gord_m[1,],
                            all_barretts_m[7,],
                            all_oesoph_ulc_m[1,],
                            all_barretts_m[8,],
                            all_abd_pain_m[1,],
                            all_barretts_m[9,],
                            all_vomiting_m[1,],
                            
                            all_hernia_abdo_m[3,],
                            all_dyspepsia_m[2,],
                            all_hernia_abdo_m[4,],
                            all_dysphagia_m[2,],
                            all_hernia_abdo_m[5,],
                            all_gastritis_duodenitis_m[2,],
                            all_hernia_abdo_m[6,],
                            all_gord_m[2,],
                            all_hernia_abdo_m[7,],
                            all_oesoph_ulc_m[2,],
                            all_hernia_abdo_m[8,],
                            all_abd_pain_m[2,],
                            all_hernia_abdo_m[9,],
                            all_vomiting_m[2,],
                            
                            all_dyspepsia_m[4,],
                            all_dysphagia_m[3,],
                            all_dyspepsia_m[5,],
                            all_gastritis_duodenitis_m[3,],
                            all_dyspepsia_m[6,],
                            all_gord_m[3,],
                            all_dyspepsia_m[7,],
                            all_oesoph_ulc_m[3,],
                            all_dyspepsia_m[8,],
                            all_abd_pain_m[3,],
                            all_dyspepsia_m[9,],
                            all_vomiting_m[3,],
                            
                            all_dysphagia_m[5,],
                            all_gastritis_duodenitis_m[4,],
                            all_dysphagia_m[6,],
                            all_gord_m[4,],
                            all_dysphagia_m[7,],
                            all_oesoph_ulc_m[4,],
                            all_dysphagia_m[8,],
                            all_abd_pain_m[4,],
                            all_dysphagia_m[9,],
                            all_vomiting_m[4,],
                            
                            all_gastritis_duodenitis_m[6,],
                            all_gord_m[5,],
                            all_gastritis_duodenitis_m[7,],
                            all_oesoph_ulc_m[5,],
                            all_gastritis_duodenitis_m[8,],
                            all_abd_pain_m[5,],
                            all_gastritis_duodenitis_m[9,],
                            all_vomiting_m[5,],
                            
                            all_gord_m[7,],
                            all_oesoph_ulc_m[6,],
                            all_gord_m[8,],
                            all_abd_pain_m[6,],
                            all_gord_m[9,],
                            all_vomiting_m[6,],
                            
                            all_oesoph_ulc_m[8,],
                            all_abd_pain_m[7,],
                            all_oesoph_ulc_m[9,],
                            all_vomiting_m[7,],
                            
                            all_abd_pain_m[9,],
                            all_vomiting_m[8,])


all_data_ordered_m$Category <- factor(all_data_ordered_m$Category,levels=all_data_ordered_m$Category)

all_data_ordered_m <- all_data_ordered_m %>%
  mutate(across(where(is.character),as.numeric))

ggplot(data=all_data_ordered_m,mapping=aes(fit_exp*100,Category))+
  geom_point()+
  geom_errorbar(data=all_data_ordered_m,aes(y=Category,
                                            xmin = lb_exp*100,
                                            xmax = ub_exp*100),
                alpha = 0.5) +
  scale_x_continuous(expand=c(0,0),limits=c(0,12))+
  xlab("Risk of OG Cancer (%)") + ylab("Pairwise combination of index features")+
  geom_hline(yintercept=2.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=4.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=6.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=8.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=10.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=12.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=14.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=16.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=18.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=20.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=22.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=24.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=26.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=28.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=30.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=32.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=34.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=36.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=38.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=40.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=42.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=44.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=46.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=48.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=50.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=52.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=54.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=56.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=58.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=60.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=62.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=64.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=66.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=68.5,linetype='dashed',alpha=0.5)+
  geom_hline(yintercept=70.5,linetype='dashed',alpha=0.5)+
  theme_bw()

ggsave(path=filepath,filename="Risk order men drop df.png",height = 12, width = 10,dpi=200)


# 
# #the main cohort data function to process for each dataframe
# order_fun <- function(pred_data_agg,preds,random_data,cohort_var){
#     predict_data <- cbind(pred_data_agg,preds$fit,row.names = NULL)
# names(predict_data)[37] <- "pred"
# predict_data <- as.data.frame(predict_data)
# 
# predict_data <- cbind(predict_data,preds$se.fit)
# names(predict_data)[38] <- "SE"
# 
# predict_data <- predict_data %>% 
#   select(c(age,gender,pred,SE)) %>%
#   arrange(gender)
# 
# predict_data <- predict_data %>%
#   mutate(pred_exp = exp(pred)) %>%
#   #mutate(risk = pred_exp/(1+pred_exp)) %>%
#   mutate(ci_lwr = pred - 1.96 * SE) %>%
#   mutate(ci_upr = pred + 1.96 * SE) %>%
#   mutate(ci_lwr_exp = exp(ci_lwr)) %>%
#   mutate(ci_upr_exp = exp(ci_upr)) %>%
#   select(-c(ci_lwr,ci_upr,pred))
#   
# generate_multiplication_table <- function(df1, column1, df2, column2){
#   multiplication_table <- as.matrix(df1[[column1]]) %*% t(as.matrix(df2[[column2]]))
#   colnames(multiplication_table) <- df2$variable
#   final_table <- cbind(predict_data,multiplication_table)
#   return(final_table)
# }
# 
# out <- tidy(spline_mod)
# out$estimate_e <- round(exp(out$estimate),3)
# 
# out$ci_lwr <- out$estimate - (1.96 * out$std.error)
# out$ci_upr <- out$estimate + (1.96 * out$std.error)
# out$ci_lwr_exp <- exp(out$ci_lwr)
# out$ci_upr_exp <- exp(out$ci_upr)
# 
# out_fil <- as.data.frame(cbind(out$term,out$estimate_e,out$p.value,out$ci_lwr_exp,out$ci_upr_exp))
# names(out_fil) <- c("variable","estimate_e","p_val","lwr","upr")
# 
# out_fil2 <- out_fil[5:40,]
# 
# out_fil2$estimate_e <- as.numeric(out_fil2$estimate_e)
# out_fil2$lwr <- as.numeric(out_fil2$lwr)
# out_fil2$upr <- as.numeric(out_fil2$upr)
# 
# #xlsx::write.xlsx(out_fil2,file=".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Regression risk outputs combined model.xlsx",sheetName = oddssheetname,append =TRUE)
# 
# tab_odds <- generate_multiplication_table(predict_data,"pred_exp",out_fil2,"estimate_e")
# tab_lwr <- generate_multiplication_table(predict_data,"ci_lwr_exp",out_fil2,"lwr")
# tab_upr <- generate_multiplication_table(predict_data,"ci_upr_exp",out_fil2,"lwr")
# 
# risk_tab <- cbind(tab_odds[,1:2],(tab_odds[,21:ncol(tab_odds)]/(1+tab_odds[21:ncol(tab_odds)])))
# lwr_tab <- cbind(tab_lwr[,1:2],(tab_lwr[,21:ncol(tab_lwr)]/(1+tab_lwr[21:ncol(tab_lwr)])))
# upr_tab <- cbind(tab_upr[,1:2],(tab_upr[,21:ncol(tab_upr)]/(1+tab_upr[21:ncol(tab_upr)])))
# 
# #aggregating risk over all ages
# #risk_tab_avg <- aggregate(cbind(dyspepsia1,dysphagia1,vomiting1,abd_pain1,barretts_bin1,oesoph_ulc_bin1,gord_bin1,gastritis_duodenitis_bin1,hernia_abdo_bin1)~gender,data=risk_tab,FUN=mean)
# risk_tab_avg_t <- t(risk_tab)
# risk_tab_avg_t<-as.data.frame(risk_tab_avg_t)
# risk_tab_avg_t <- risk_tab_avg_t[2:nrow(risk_tab_avg_t),]
# 
# #lwr_tab_avg <- aggregate(cbind(dyspepsia1,dysphagia1,vomiting1,abd_pain1,barretts_bin1,oesoph_ulc_bin1,gord_bin1,gastritis_duodenitis_bin1,hernia_abdo_bin1)~gender,data=lwr_tab,FUN=mean)
# lwr_tab_avg_t <- t(lwr_tab)
# lwr_tab_avg_t<-as.data.frame(lwr_tab_avg_t)
# lwr_tab_avg_t <- lwr_tab_avg_t[2:nrow(lwr_tab_avg_t),]
# 
# #upr_tab_avg <- aggregate(cbind(dyspepsia1,dysphagia1,vomiting1,abd_pain1,barretts_bin1,oesoph_ulc_bin1,gord_bin1,gastritis_duodenitis_bin1,hernia_abdo_bin1)~gender,data=upr_tab,FUN=mean)
# upr_tab_avg_t <- t(upr_tab)
# upr_tab_avg_t<-as.data.frame(upr_tab_avg_t)
# upr_tab_avg_t <- upr_tab_avg_t[2:nrow(upr_tab_avg_t),]
# 
# men <- cbind(risk_tab_avg_t[1],lwr_tab_avg_t[1],upr_tab_avg_t[1])
# colnames(men) = c("og_cancer","lower","higher")
# men <- men %>%
#   slice(-1) %>%
#   mutate(Category = c("dyspepsia","dysphagia","vomiting","upper abdominal pain","Barrett's oesophagus","oesophageal ulcer","GORD","gastritis/duodenitis","diaphragmatic hernia"))%>%
#   mutate(Category = paste0(cohort_var," with ",Category)) %>%
#   mutate(lowcase=tolower(Category)) %>%
#   arrange(lowcase) %>%
#   select(-lowcase)
# rownames(men) <- NULL
# 
# women <- cbind(risk_tab_avg_t[2],lwr_tab_avg_t[2],upr_tab_avg_t[2])
# colnames(women) = c("og_cancer","lower","higher")
# women <- women %>%
#   mutate(Category = c("dyspepsia","dysphagia","vomiting","upper abdominal pain","Barrett's oesophagus","oesophageal ulcer","GORD","gastritis/duodenitis","diaphragmatic hernia"))%>%
#   mutate(Category = paste0(cohort_var," with ",Category)) %>%
#   mutate(lowcase=tolower(Category)) %>%
#   arrange(lowcase)%>%
#   select(-lowcase)
# rownames(women) <- NULL
# 
# return(list(men,women))
# }
# #----------------------------------
# 
# preds <- preds_1_link
# cohort_var <- "GORD"
# gord_out <- order_fun(pred_data_agg,preds,random_data,cohort_var)
# 
# preds <- preds_2_link
# cohort_var <- "Oesophageal ulcer"
# oesoph_out <- order_fun(pred_data_agg,preds,random_data,cohort_var)
# 
# preds <- preds_3_link
# cohort_var <- "Gastrtiis/duodenitis"
# gas_out <- order_fun(pred_data_agg,preds,random_data,cohort_var)
# 
# preds <- preds_4_link
# cohort_var <- "Diaphragmatic hernia"
# hern_out <- order_fun(pred_data_agg,preds,random_data,cohort_var)
# 
# preds <- preds_5_link
# cohort_var <- "Barrett's oesophagus"
# barr_out <- order_fun(pred_data_agg,preds,random_data,cohort_var)
# 
# preds <- preds_6_link
# cohort_var <- "Dysphagia"
# dysph_out <- order_fun(pred_data_agg,preds,random_data,cohort_var)
# 
# preds <- preds_7_link
# cohort_var <- "Dyspepsia"
# dysp_out <- order_fun(pred_data_agg,preds,random_data,cohort_var)
# 
# preds <- preds_8_link
# cohort_var <- "Upper abdominal pain"
# abd_pain_out <- order_fun(pred_data_agg,preds,random_data,cohort_var)
# 
# preds <- preds_9_link
# cohort_var <- "Vomiting"
# vom_out <- order_fun(pred_data_agg,preds,random_data,cohort_var)
# 
# 
# all_gord_f <- as.data.frame(gord_out[2])
# all_oesoph_ulc_f <- as.data.frame(oesoph_out[2])
# all_gastritis_duodenitis_f <- as.data.frame(gas_out[2])
# all_barretts_f <- as.data.frame(barr_out[2])
# all_hernia_abdo_f <- as.data.frame(hern_out[2])
# all_dyspepsia_f <- as.data.frame(dysp_out[2])
# all_dysphagia_f <- as.data.frame(dysph_out[2])
# all_abd_pain_f <- as.data.frame(abd_pain_out[2])
# all_vomiting_f <- as.data.frame(vom_out[2])
# 
# all_gord_m <- as.data.frame(gord_out[1])
# all_oesoph_ulc_m <- as.data.frame(oesoph_out[1])
# all_gastritis_duodenitis_m <- as.data.frame(gas_out[1])
# all_barretts_m <- as.data.frame(barr_out[1])
# all_hernia_abdo_m <- as.data.frame(hern_out[1])
# all_dyspepsia_m <- as.data.frame(dysp_out[1])
# all_dysphagia_m <- as.data.frame(dysph_out[1])
# all_abd_pain_m <- as.data.frame(abd_pain_out[1])
# all_vomiting_m <- as.data.frame(vom_out[1])
# 
# 
# 
# #also plotting now all the predicted values from all cohorts
# preds_0 <- data.frame(preds_0)
# preds_1 <- data.frame(preds_1)
# preds_2 <- data.frame(preds_2)
# preds_3 <- data.frame(preds_3)
# preds_4 <- data.frame(preds_4)
# preds_5 <- data.frame(preds_5)
# preds_6 <- data.frame(preds_6)
# preds_7 <- data.frame(preds_7)
# preds_8 <- data.frame(preds_8)
# preds_9 <- data.frame(preds_9)
# 
# plot_data <- as.data.frame(cbind(preds_0[1],preds_1[1],preds_2[1],preds_3[1],preds_4[1],preds_5[1],preds_6[1],preds_7[1],preds_8[1],preds_9[1]))
# 
# cycle <- c(rep("Men",71),rep("Women",71))
# plot_data$gender <- cycle
# plot_data$age <- seq(30, 100, length.out = 71)
# 
# names(plot_data) <- c("preds_0","preds_1","preds_2","preds_3","preds_4","preds_5","preds_6","preds_7","preds_8","preds_9","gender","age")
# 
# long_plot_data <- pivot_longer(plot_data,cols=-c(age,gender),names_to="Cohort",values_to="Risk")
# 
# long_plot_data <- long_plot_data %>%
#   mutate(Cohort = case_when(
#     Cohort == "preds_0"~ "Random sample",
#     Cohort == "preds_1"~"GORD",
#     Cohort == "preds_2"~"Oesophageal ulcer",
#     Cohort == "preds_3"~"Gastritis/duodenitis",
#     Cohort == "preds_4"~"Diaphgramatic hernia",
#     Cohort == "preds_5"~"Barrett's oesophagus",
#     Cohort == "preds_6"~"Dysphagia",
#     Cohort == "preds_7"~"Dyspepsia",
#     Cohort == "preds_8"~"Upper abdominal pain",
#     Cohort == "preds_9"~"Vomiting",
#     TRUE ~Cohort
#     
#   )) %>%
#   mutate(Cohort = factor(Cohort,levels=c("Random sample","Barrett's oesophagus","Diaphgramatic hernia","Dysphagia","Dyspepsia",
#                                             "Gastritis/duodenitis","GORD","Oesophageal ulcer","Upper abdominal pain","Vomiting")))
# 
# #------------------------------------------
# #get the CIs
# preds_1 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor("1")),type='link',se.fit=TRUE))
# preds_2 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort_2 = as.factor("1")),type='link',se.fit=TRUE))
# preds_3 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(3)),type='link',se.fit=TRUE))
# preds_4 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(4)),type='link',se.fit=TRUE))
# preds_5 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(5)),type='link',se.fit=TRUE))
# preds_6 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(6)),type='link',se.fit=TRUE))
# preds_7 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(7)),type='link',se.fit=TRUE))
# preds_8 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(8)),type='link',se.fit=TRUE))
# preds_9 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(9)),type='link',se.fit=TRUE))
# preds_0 <- as.data.frame(predict(spline_mod_cohort_drop,pred_data %>% mutate(cohort = as.factor(0)),type='link',se.fit=TRUE))

spline_mod_men <- readRDS('S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\spline_model_men.rda')
spline_mod_women <- readRDS('S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\spline_model_women.rda')

preds_1 <- predict(spline_mod_men,pred_data %>% mutate(cohort_1 = 1),type='link',se.fit=TRUE)
preds_2 <- predict(spline_mod_men,pred_data %>% mutate(cohort_2 = 1),type='link',se.fit=TRUE)
preds_3 <- predict(spline_mod_men,pred_data %>% mutate(cohort_3 = 1),type='link',se.fit=TRUE)
preds_4 <- predict(spline_mod_men,pred_data %>% mutate(cohort_4 = 1),type='link',se.fit=TRUE)
preds_5 <- predict(spline_mod_men,pred_data %>% mutate(cohort_5 = 1),type='link',se.fit=TRUE)
preds_6 <- predict(spline_mod_men,pred_data %>% mutate(cohort_6 = 1),type='link',se.fit=TRUE)
preds_7 <- predict(spline_mod_men,pred_data %>% mutate(cohort_7 = 1),type='link',se.fit=TRUE)
preds_8 <- predict(spline_mod_men,pred_data %>% mutate(cohort_8 = 1),type='link',se.fit=TRUE)
preds_9 <- predict(spline_mod_men,pred_data %>% mutate(cohort_9 = 1),type='link',se.fit=TRUE)
preds_0 <- predict(spline_mod_men,pred_data %>% mutate(cohort_0 = 1),type='link',se.fit=TRUE)

plot_data_logit <- as.data.frame(cbind(as.data.frame(preds_0[1:2]),as.data.frame(preds_1[1:2]),as.data.frame(preds_2[1:2]),
                                       as.data.frame(preds_3[1:2]),as.data.frame(preds_4[1:2]),as.data.frame(preds_5[1:2]),
                                       as.data.frame(preds_6[1:2]),as.data.frame(preds_7[1:2]),as.data.frame(preds_8[1:2]),as.data.frame(preds_9[1:2])))

#cycle <- c(rep("Men",71),rep("Women",71))
#sex specific version
cycle <- c(rep("Men",71))
plot_data_logit$gender <- cycle
plot_data_logit$age <- seq(30, 100, length.out = 71)

names(plot_data_logit) <- c("preds_0","se_0","preds_1","se_1","preds_2","se_2","preds_3","se_3","preds_4","se_4",
                         "preds_5","se_5","preds_6","se_6","preds_7","se_7","preds_8","se_8","preds_9","se_9","gender","age")

long_plot_data_all <- pivot_longer(plot_data_logit,cols=-c(age,gender),names_to="Cohort",values_to="SE")

long_plot_data_pred <- long_plot_data_all[seq(1,nrow(long_plot_data_all),2),]
long_plot_data_pred <- long_plot_data_pred %>% rename("Pred"="SE")

long_plot_data_pred <- long_plot_data_pred %>%
  mutate(Cohort = case_when(
    Cohort == "preds_0"~ "Random sample",
    Cohort == "preds_1"~"GORD",
    Cohort == "preds_2"~"Oesophageal ulcer",
    Cohort == "preds_3"~"Gastritis/duodenitis",
    Cohort == "preds_4"~"Diaphgramatic hernia",
    Cohort == "preds_5"~"Barrett's oesophagus",
    Cohort == "preds_6"~"Dysphagia",
    Cohort == "preds_7"~"Dyspepsia",
    Cohort == "preds_8"~"Upper abdominal pain",
    Cohort == "preds_9"~"Vomiting",
    TRUE ~Cohort

  )) %>%
  mutate(Cohort = factor(Cohort,levels=c("Random sample","Barrett's oesophagus","Diaphgramatic hernia","Dysphagia","Dyspepsia",
                                         "Gastritis/duodenitis","GORD","Oesophageal ulcer","Upper abdominal pain","Vomiting")))

long_plot_data_se <- long_plot_data_all[seq(2,nrow(long_plot_data_all),2),]

long_plot_data_se <- long_plot_data_se %>%
  mutate(Cohort = case_when(
    Cohort == "se_0"~ "Random sample",
    Cohort == "se_1"~"GORD",
    Cohort == "se_2"~"Oesophageal ulcer",
    Cohort == "se_3"~"Gastritis/duodenitis",
    Cohort == "se_4"~"Diaphgramatic hernia",
    Cohort == "se_5"~"Barrett's oesophagus",
    Cohort == "se_6"~"Dysphagia",
    Cohort == "se_7"~"Dyspepsia",
    Cohort == "se_8"~"Upper abdominal pain",
    Cohort == "se_9"~"Vomiting",
    TRUE ~Cohort

  )) %>%
  mutate(Cohort = factor(Cohort,levels=c("Random sample","Barrett's oesophagus","Diaphgramatic hernia","Dysphagia","Dyspepsia",
                                         "Gastritis/duodenitis","GORD","Oesophageal ulcer","Upper abdominal pain","Vomiting")))

tid <- tidy(spline_mod_men)
long_plot_data_join <- inner_join(long_plot_data_pred,long_plot_data_se)

long_plot_data_join <-  long_plot_data_join %>%
  mutate(lb_log_odds = Pred - 1.96*SE,
         ub_log_odds = Pred + 1.96*SE,
         risk = 1/(1+exp(-Pred)),
         lb_risk = 1/(1+exp(-lb_log_odds)),
         ub_risk = 1/(1+exp(-ub_log_odds)),
         test = plogis(lb_log_odds)) #%>% filter((ub_risk) < 0.10)



colour_vec <- c("black","firebrick","#692000","#FF9900","yellow2","palegreen4",'chartreuse4',"skyblue3","royalblue4","#660099")

ggplot()+
  geom_line(long_plot_data_join %>% filter(gender=="Men"),mapping=aes(age,risk*100,colour=Cohort),linewidth=0.9)+
  labs(fill='Cohort') +
  geom_ribbon(long_plot_data_join %>% filter(gender == "Men"),mapping=aes(age,ymin=lb_risk*100,ymax=ub_risk*100,fill=Cohort),alpha=0.2)+
  geom_hline(yintercept=3,linetype='twodash',color='red') +
  ylim(c(0,8))+
  xlim(c(40,90))+
  #facet_grid(c('gender'))+
  ylab("Risk (%)")+#+geom_point(aes(shape=long_df$bold))+
  scale_colour_manual(values=colour_vec)+
  scale_fill_manual(values=colour_vec)+
  theme_bw()#name=long_df$`Co-occurring feature`,
ggsave(path=filepath,filename="combined_cohort_comparison_men_separate_model.png",width=10,height=10,dpi=200)

#-----------------------------------------
preds_1 <- predict(spline_mod_women,pred_data %>% mutate(cohort_1 = 1),type='link',se.fit=TRUE)
preds_2 <- predict(spline_mod_women,pred_data %>% mutate(cohort_2 = 1),type='link',se.fit=TRUE)
preds_3 <- predict(spline_mod_women,pred_data %>% mutate(cohort_3 = 1),type='link',se.fit=TRUE)
preds_4 <- predict(spline_mod_women,pred_data %>% mutate(cohort_4 = 1),type='link',se.fit=TRUE)
preds_5 <- predict(spline_mod_women,pred_data %>% mutate(cohort_5 = 1),type='link',se.fit=TRUE)
preds_6 <- predict(spline_mod_women,pred_data %>% mutate(cohort_6 = 1),type='link',se.fit=TRUE)
preds_7 <- predict(spline_mod_women,pred_data %>% mutate(cohort_7 = 1),type='link',se.fit=TRUE)
preds_8 <- predict(spline_mod_women,pred_data %>% mutate(cohort_8 = 1),type='link',se.fit=TRUE)
preds_9 <- predict(spline_mod_women,pred_data %>% mutate(cohort_9 = 1),type='link',se.fit=TRUE)
preds_0 <- predict(spline_mod_women,pred_data %>% mutate(cohort_0 = 1),type='link',se.fit=TRUE)

plot_data_logit <- as.data.frame(cbind(as.data.frame(preds_0[1:2]),as.data.frame(preds_1[1:2]),as.data.frame(preds_2[1:2]),
                                       as.data.frame(preds_3[1:2]),as.data.frame(preds_4[1:2]),as.data.frame(preds_5[1:2]),
                                       as.data.frame(preds_6[1:2]),as.data.frame(preds_7[1:2]),as.data.frame(preds_8[1:2]),as.data.frame(preds_9[1:2])))

#cycle <- c(rep("women",71),rep("Wowomen",71))
#sex specific version
cycle <- c(rep("Women",71))
plot_data_logit$gender <- cycle
plot_data_logit$age <- seq(30, 100, length.out = 71)

names(plot_data_logit) <- c("preds_0","se_0","preds_1","se_1","preds_2","se_2","preds_3","se_3","preds_4","se_4",
                            "preds_5","se_5","preds_6","se_6","preds_7","se_7","preds_8","se_8","preds_9","se_9","gender","age")

long_plot_data_all <- pivot_longer(plot_data_logit,cols=-c(age,gender),names_to="Cohort",values_to="SE")

long_plot_data_pred <- long_plot_data_all[seq(1,nrow(long_plot_data_all),2),]
long_plot_data_pred <- long_plot_data_pred %>% rename("Pred"="SE")

long_plot_data_pred <- long_plot_data_pred %>%
  mutate(Cohort = case_when(
    Cohort == "preds_0"~ "Random sample",
    Cohort == "preds_1"~"GORD",
    Cohort == "preds_2"~"Oesophageal ulcer",
    Cohort == "preds_3"~"Gastritis/duodenitis",
    Cohort == "preds_4"~"Diaphgramatic hernia",
    Cohort == "preds_5"~"Barrett's oesophagus",
    Cohort == "preds_6"~"Dysphagia",
    Cohort == "preds_7"~"Dyspepsia",
    Cohort == "preds_8"~"Upper abdominal pain",
    Cohort == "preds_9"~"Vomiting",
    TRUE ~Cohort
    
  )) %>%
  mutate(Cohort = factor(Cohort,levels=c("Random sample","Barrett's oesophagus","Diaphgramatic hernia","Dysphagia","Dyspepsia",
                                         "Gastritis/duodenitis","GORD","Oesophageal ulcer","Upper abdominal pain","Vomiting")))

long_plot_data_se <- long_plot_data_all[seq(2,nrow(long_plot_data_all),2),]

long_plot_data_se <- long_plot_data_se %>%
  mutate(Cohort = case_when(
    Cohort == "se_0"~ "Random sample",
    Cohort == "se_1"~"GORD",
    Cohort == "se_2"~"Oesophageal ulcer",
    Cohort == "se_3"~"Gastritis/duodenitis",
    Cohort == "se_4"~"Diaphgramatic hernia",
    Cohort == "se_5"~"Barrett's oesophagus",
    Cohort == "se_6"~"Dysphagia",
    Cohort == "se_7"~"Dyspepsia",
    Cohort == "se_8"~"Upper abdominal pain",
    Cohort == "se_9"~"Vomiting",
    TRUE ~Cohort
    
  )) %>%
  mutate(Cohort = factor(Cohort,levels=c("Random sample","Barrett's oesophagus","Diaphgramatic hernia","Dysphagia","Dyspepsia",
                                         "Gastritis/duodenitis","GORD","Oesophageal ulcer","Upper abdominal pain","Vomiting")))

tid <- tidy(spline_mod_women)
long_plot_data_join <- inner_join(long_plot_data_pred,long_plot_data_se)

long_plot_data_join <-  long_plot_data_join %>%
  mutate(lb_log_odds = Pred - 1.96*SE,
         ub_log_odds = Pred + 1.96*SE,
         risk = 1/(1+exp(-Pred)),
         lb_risk = 1/(1+exp(-lb_log_odds)),
         ub_risk = 1/(1+exp(-ub_log_odds)),
         test = plogis(lb_log_odds)) #%>% filter((ub_risk) < 0.10)

ggplot()+
  geom_line(long_plot_data_join %>% filter(gender=="Women"),mapping=aes(age,risk*100,colour=Cohort),linewidth=0.9)+
  labs(fill='Cohort') +
  geom_ribbon(long_plot_data_join %>% filter(gender == "Women"),mapping=aes(age,ymin=lb_risk*100,ymax=ub_risk*100,fill=Cohort),alpha=0.2)+
  geom_hline(yintercept=3,linetype='twodash',color='red') +
  ylim(c(0,8))+
  xlim(c(40,90))+
  #facet_grid(c('gender'))+
  ylab("Risk (%)")+#+geom_point(aes(shape=long_df$bold))+
  scale_colour_manual(values=colour_vec)+
  scale_fill_manual(values=colour_vec)+
  theme_bw()#name=long_df$`Co-occurring feature`,
ggsave(path=filepath,filename="combined_cohort_comparison_women_separate_model.png",width=10,height=10,dpi=200)
