rm(list=ls())
#### Loading Packages ####
library(pacman)

pacman::p_load(tidyverse, tidyr,lubridate, purr,
               RMySQL, DBI,broom,readxl,janitor,xlsx)

#----------------------------------------------------------------------------------------
##### Database connection #####
db <- dbConnect(MySQL(), host = "*", user = "*", password = "*", 
                dbname = "*", port = *)

#-----------------------------------------------------------------
## Read in data from excel risk spreadsheet
path <- "S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\"
dysphagia <- read_excel(paste0(path,"Regression risk outputs v6.xlsx"),sheet="Risk table Dysphagia")
dysphagia <- dysphagia %>% select(-c("...1"))

vomiting <- read_excel(paste0(path,"Regression risk outputs v6.xlsx"),sheet="Risk table Vomiting")
vomiting <- vomiting %>% select(-c('...1'))

dyspepsia <- read_excel(paste0(path,"Regression risk outputs v6.xlsx"),sheet="Risk table Dyspepsia")
dyspepsia <- dyspepsia %>% select(-c('...1'))

abd_pain <- read_excel(paste0(path,"Regression risk outputs v6.xlsx"),sheet="Risk table Abd pain")
abd_pain <- abd_pain %>% select(-c('...1'))

gord <- read_excel(paste0(path,"Regression risk outputs v6.xlsx"),sheet="Risk table GORD")
gord$gord_bin1 <- NA
gord <- gord %>% select(-c('...1'))

gastritis_duodenitis <- read_excel(paste0(path,"Regression risk outputs v6.xlsx"),sheet="Risk table gastritis")
gastritis_duodenitis$gastritis_duodenitis1 <- NA
gastritis_duodenitis <- gastritis_duodenitis %>% select(-c('...1'))

barretts <- read_excel(paste0(path,"Regression risk outputs v6.xlsx"),sheet="Risk table Barretts")
barretts$barretts_bin1 <- NA
barretts <- barretts %>% select(-c('...1'))

oesoph_ulc <- read_excel(paste0(path,"Regression risk outputs v6.xlsx"),sheet="Risk table oesophageal ulcer")
oesoph_ulc$oesoph_ulc_bin1 <- NA
oesoph_ulc <- oesoph_ulc %>% select(-c('...1'))

hernia_abdo <- read_excel(paste0(path,"Regression risk outputs v6.xlsx"),sheet="Risk table hernia abdo")
hernia_abdo$hernia_abdo_bin1 <- NA
hernia_abdo <- hernia_abdo %>% select(-c('...1'))

dataframes <- list(dysphagia,vomiting,dyspepsia,abd_pain,gord,gastritis_duodenitis,barretts,oesoph_ulc,hernia_abdo)
#----------------------------------
#function to find the index of the first instance meeting the threshold
first_thresh_index <- function(column,threshold1){
  index1 <- which(column>=threshold1)[1]
  if(is.na(index1)){
    return(NA)
  } else {
    return(index1)
  }
}

second_thresh_index <- function(column,threshold2){
  index2 <- which(column>=threshold2)[1]
  if(is.na(index2)){
    return(NA)
  } else {
    return(index2)
  }
}

third_thresh_index <- function(column,threshold3){
  index3 <- which(column>=threshold3)[1]
  if(is.na(index3)){
    return(NA)
  } else {
    return(index3)
  }
}
#----------------------------
## A function to output the risk estimates above each threshold

thresh_min_foo <- function(dataframe){
  threshold1 <- 0.03
  threshold2 <- 0.06
  threshold3 <- 0.1

  #formatting the data
  dataframe <- dataframe %>%
    filter(cat == 1) %>%
    select(-c('odds','cat'))
  
  #set the df
  data_m <- dataframe %>% filter(gender == 0)
  df_m <- data_m[,3:29]
  
  data_f <- dataframe %>% filter(gender == 1)
  df_f <- data_f[,3:29]
  
#iterate over each column of the dataframe
#apply_function_to_df <- function(dataframe){
  
  #initialise an empty dataframe to store results
  df_result <- data.frame(variable = character(),
                          age_at_3_m=numeric(),age_at_6_m = numeric(),age_at_10_m = numeric(),
                          age_at_3_f=numeric(),age_at_6_f = numeric(),age_at_10_f = numeric(),
                          stringsAsFactors = FALSE)
  
  for (col in names(dataframe)){

  #find the index of first instance meeting the threshold
  index1 <- first_thresh_index(df_m[[col]],threshold1)
  index2 <- second_thresh_index(df_m[[col]],threshold2)
  index3 <- third_thresh_index(df_m[[col]],threshold3)
  
  index4 <- first_thresh_index(df_f[[col]],threshold1)
  index5 <- second_thresh_index(df_f[[col]],threshold2)
  index6 <- third_thresh_index(df_f[[col]],threshold3)
    
  #if threshold met, store the result in the result dataframe
    df_result <- rbind(df_result,data.frame(variable=as.character(col),
                                            age_at_3_m = (index1 + 29),
                                            age_at_6_m = (index2 + 29),
                                            age_at_10_m = (index3 + 29),
                                            age_at_3_f = (index4 + 29),
                                            age_at_6_f = (index5 + 29),
                                            age_at_10_f = (index6 + 29),
                                            stringsAsFactors = FALSE))
  
  }
  #}
return(df_result[3:29,])
}


result_list <- lapply(dataframes,thresh_min_foo)

names(result_list) <- c("Dysphagia","Vomiting","Dyspepsia","Upper abdominal pain","GORD","Gastritis/duodenitis","Barrett's","Oesophageal ulcer","Diaphragmatic hernia")

dysphagia <- data.frame(result_list[1])
vomiting <- data.frame(result_list[2])
dyspepsia <- data.frame(result_list[3])
abd_pain <- data.frame(result_list[4])
gord <- data.frame(result_list[5])
gastritis <- data.frame(result_list[6])
barretts <- data.frame(result_list[7])
oesoph_ulc <- data.frame(result_list[8])
hernia_abdo <- data.frame(result_list[9])

col_names=c("variable","age_at_3_m",'age_at_6_m','age_at_10_m','age_at_3_f','age_at_6_f','age_at_10_f')

names(dysphagia) <- col_names
names(vomiting) <- col_names
names(dyspepsia) <- col_names
names(abd_pain) <- col_names
names(gord) <- col_names
names(gastritis) <- col_names
names(barretts) <- col_names
names(oesoph_ulc) <- col_names
names(hernia_abdo) <- col_names

df_list <- list(dysphagia,dyspepsia,vomiting,abd_pain,gord,barretts,oesoph_ulc,gastritis,hernia_abdo)

process <- function(dataframe){
dataframe <- dataframe %>%
  mutate(variable = factor(variable, levels = c("pred",
                                                                    "bmi_cat1","bmi_cat2","bmi_cat3",
                                                                    "alc_status_comb2","alc_status_comb3",
                                                                    "final_smoking_status2","final_smoking_status4",
                                                                    "elix_band(1,3]","elix_band(3,5]","elix_band(5,25]",
                                                                    "h_pylori_flag1","low_hb1","raise_platelets1","raise_wbc1","cough1","fatigue1","weight_loss1",
                                                                    "dyspepsia1","dysphagia1","abd_pain1","vomiting1",
                                                                    "barretts_bin1","hernia_abdo_bin1","gastritis_duodenitis_bin1","gord_bin1","oesoph_ulc_bin1")))%>%
  arrange(variable)
}

dysphagia <- process(dysphagia)
dyspepsia <- process(dyspepsia)
vomiting <- process(vomiting)
abd_pain <- process(abd_pain)
barretts <- process(barretts)
gord <- process(gord)
gastritis <- process(gastritis)
oesoph_ulc <- process(oesoph_ulc)
hernia_abdo <- process(hernia_abdo)

results <- rbind(dyspepsia,dysphagia,abd_pain,vomiting,barretts,hernia_abdo,gastritis,gord,oesoph_ulc)

write.xlsx(results,file = "*",append=TRUE)


#--------------------------------------------------------------------------------------------------------------------------------------------
#Repeating the same thing for the combined model
#-----------------------------------------------------------------
## Read in data from excel risk spreadsheet
path <- "*"
dysphagia <- read_excel(paste0(path,"Regression risk outputs combined model.xlsx"),sheet="Dysphagia risk")
dysphagia <- dysphagia %>% select(-c("...1"))

vomiting <- read_excel(paste0(path,"Regression risk outputs combined model.xlsx"),sheet="Vomiting risk")
vomiting <- vomiting %>% select(-c('...1'))

dyspepsia <- read_excel(paste0(path,"Regression risk outputs combined model.xlsx"),sheet="Dyspepsia risk")
dyspepsia <- dyspepsia %>% select(-c('...1'))

abd_pain <- read_excel(paste0(path,"Regression risk outputs combined model.xlsx"),sheet="abdominal pain risk")
abd_pain <- abd_pain %>% select(-c('...1'))

gord <- read_excel(paste0(path,"Regression risk outputs combined model.xlsx"),sheet="GORD risk")
gord$gord_bin1 <- NA
gord <- gord %>% select(-c('...1'))

gastritis_duodenitis <- read_excel(paste0(path,"Regression risk outputs combined model.xlsx"),sheet="Gastritis risk")
gastritis_duodenitis$gastritis_duodenitis1 <- NA
gastritis_duodenitis <- gastritis_duodenitis %>% select(-c('...1'))

barretts <- read_excel(paste0(path,"Regression risk outputs combined model.xlsx"),sheet="Barrett risk")
barretts$barretts_bin1 <- NA
barretts <- barretts %>% select(-c('...1'))

oesoph_ulc <- read_excel(paste0(path,"Regression risk outputs combined model.xlsx"),sheet="oesoph_ulc risk")
oesoph_ulc$oesoph_ulc_bin1 <- NA
oesoph_ulc <- oesoph_ulc %>% select(-c('...1'))

hernia_abdo <- read_excel(paste0(path,"Regression risk outputs combined model.xlsx"),sheet="hernia risk")
hernia_abdo$hernia_abdo_bin1 <- NA
hernia_abdo <- hernia_abdo %>% select(-c('...1'))

dataframes <- list(dysphagia,vomiting,dyspepsia,abd_pain,gord,gastritis_duodenitis,barretts,oesoph_ulc,hernia_abdo)
#----------------------------------
#function to find the index of the first instance meeting the threshold
first_thresh_index <- function(column,threshold1){
  index1 <- which(column>=threshold1)[1]
  if(is.na(index1)){
    return(NA)
  } else {
    return(index1)
  }
}

second_thresh_index <- function(column,threshold2){
  index2 <- which(column>=threshold2)[1]
  if(is.na(index2)){
    return(NA)
  } else {
    return(index2)
  }
}

third_thresh_index <- function(column,threshold3){
  index3 <- which(column>=threshold3)[1]
  if(is.na(index3)){
    return(NA)
  } else {
    return(index3)
  }
}
#----------------------------
## A function to output the risk estimates above each threshold
thresh_min_foo <- function(dataframe){
  threshold1 <- 0.03
  threshold2 <- 0.06
  threshold3 <- 0.1
  
  #formatting the data
  dataframe <- dataframe %>%
    select(-c(odds,cohort_1,cohort_2,cohort_3,cohort_4,cohort_5,cohort_6,cohort_7,cohort_8,cohort_8,cohort_9,gender1))
  
  #set the df
  data_m <- dataframe %>% filter(gender == 0)
  df_m <- data_m[,3:29]
  
  data_f <- dataframe %>% filter(gender == 1)
  df_f <- data_f[,2:29]

  #initialise an empty dataframe to store results
  df_result <- data.frame(variable = character(),
                          age_at_3_m=numeric(),age_at_6_m = numeric(),age_at_10_m = numeric(),
                          age_at_3_f=numeric(),age_at_6_f = numeric(),age_at_10_f = numeric(),
                          stringsAsFactors = FALSE)
  
  for (col in names(dataframe)){
    
    #find the index of first instance meeting the threshold
    index1 <- first_thresh_index(df_m[[col]],threshold1)
    index2 <- second_thresh_index(df_m[[col]],threshold2)
    index3 <- third_thresh_index(df_m[[col]],threshold3)
    
    index4 <- first_thresh_index(df_f[[col]],threshold1)
    index5 <- second_thresh_index(df_f[[col]],threshold2)
    index6 <- third_thresh_index(df_f[[col]],threshold3)
    
    #if threshold met, store the result in the result dataframe
    df_result <- rbind(df_result,data.frame(variable=as.character(col),
                                            age_at_3_m = (index1 + 29),
                                            age_at_6_m = (index2 + 29),
                                            age_at_10_m = (index3 + 29),
                                            age_at_3_f = (index4 + 29),
                                            age_at_6_f = (index5 + 29),
                                            age_at_10_f = (index6 + 29),
                                            stringsAsFactors = FALSE))
    
  }
  #}
  return(df_result[3:29,])
}


result_list <- lapply(dataframes,thresh_min_foo)

names(result_list) <- c("Dysphagia","Vomiting","Dyspepsia","Upper abdominal pain","GORD","Gastritis/duodenitis","Barrett's","Oesophageal ulcer","Diaphragmatic hernia")

dysphagia <- data.frame(result_list[1])
vomiting <- data.frame(result_list[2])
dyspepsia <- data.frame(result_list[3])
abd_pain <- data.frame(result_list[4])
gord <- data.frame(result_list[5])
gastritis <- data.frame(result_list[6])
barretts <- data.frame(result_list[7])
oesoph_ulc <- data.frame(result_list[8])
hernia_abdo <- data.frame(result_list[9])

col_names=c("variable","age_at_3_m",'age_at_6_m','age_at_10_m','age_at_3_f','age_at_6_f','age_at_10_f')

names(dysphagia) <- col_names
names(vomiting) <- col_names
names(dyspepsia) <- col_names
names(abd_pain) <- col_names
names(gord) <- col_names
names(gastritis) <- col_names
names(barretts) <- col_names
names(oesoph_ulc) <- col_names
names(hernia_abdo) <- col_names

df_list <- list(dysphagia,dyspepsia,vomiting,abd_pain,gord,barretts,oesoph_ulc,gastritis,hernia_abdo)

process <- function(dataframe){
  dataframe <- dataframe %>%
    mutate(variable = factor(variable, levels = c("pred",
                                                  "bmi_cat1","bmi_cat2","bmi_cat3",
                                                  "alc_status_comb2","alc_status_comb3",
                                                  "final_smoking_status2","final_smoking_status4",
                                                  "elix_band(1,3]","elix_band(3,5]","elix_band(5,25]",
                                                  "h_pylori_flag1","low_hb1","raise_platelets1","raise_wbc1","cough1","fatigue1","weight_loss1",
                                                  "dyspepsia1","dysphagia1","abd_pain1","vomiting1",
                                                  "barretts_bin1","hernia_abdo_bin1","gastritis_duodenitis_bin1","gord_bin1","oesoph_ulc_bin1")))%>%
    arrange(variable)
}

dysphagia <- process(dysphagia)
dyspepsia <- process(dyspepsia)
vomiting <- process(vomiting)
abd_pain <- process(abd_pain)
barretts <- process(barretts)
gord <- process(gord)
gastritis <- process(gastritis)
oesoph_ulc <- process(oesoph_ulc)
hernia_abdo <- process(hernia_abdo)

results <- rbind(dyspepsia,dysphagia,abd_pain,vomiting,barretts,hernia_abdo,gastritis,gord,oesoph_ulc)

write.xlsx(results,file = "*",append=TRUE)
