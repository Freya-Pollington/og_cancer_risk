rm(list=ls())
#### Loading Packages ####
#Install pacman so future packages can be installed and loaded at the same time
#install.packages("pacman")
library(pacman)

# Install & load packages at same time
# If it asks "install from sources that need compilation?" Hit "no"
pacman::p_load(tidyverse, tidyr,lubridate,splines,
               RMySQL, DBI, xlsx,glmnet,broom,readxl,RColorBrewer,janitor,scales,glmnet,writexl)

#----------------------------------------------------------------------------------------
##### Send query to retrieve initial table of upper GI symptoms within the study dates ####
db <- dbConnect(MySQL(), host = "dsh-00872msq01.idhs.ucl.ac.uk", user = "rmjlfpo", password = "Fre11_pol23", 
                dbname = "freya", port = 3306)

#-------------------------------------------------------------------------
#A function to format cohort dataframes

data_format_non_neo <- function(data,rand,cohort_var){
 # data <- gord
  data <- data %>%
    select(-c(count_non_neo,count_symptoms,s_or_o_cancer,stomach_c,oesophageal_c,weight,data_source)) %>%
    mutate(eventdate = as.Date(eventdate)) %>%
    mutate(cat = "1") %>%
    mutate(x = 0)%>%
    select(-contains('same_day')) %>%
    select(-contains('td_'))
    #rename(ever_obesity = obesity) %>%
    #rename(ever_alcohol_problems = alc_problems)
  
  names(data)[30] <- cohort_var
  
  #combining the ex-drinker variable with the drinker variable
  names(data) <- make.unique(names(data))
  
  data <- data %>%
    mutate(alc_status_comb = as.numeric(alc_status_comb)) %>%
    mutate(alc_status_comb=case_when(alc_status_comb == 5 ~3,#if drinker with alc problems then join with ex drinker with problems
                                    alc_status_comb == 4~2,
                                    TRUE ~ alc_status_comb)) %>%
    mutate(alc_status_comb = as.factor(alc_status_comb))
  
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
    mutate(vomiting = 0) %>%
    mutate(h_pylori_flag = 0) %>% 
    mutate(eventdate = as.Date(eventdate)) %>%
    mutate(oesoph_ulc = 0) %>%
    mutate(gord = 0) %>%
    mutate(barretts = 0) %>%
    mutate(hernia_abdo = 0) %>%
    mutate(gastritis_duodenitis = 0) %>%
    mutate(cat = "0") %>%
    rename(og_cancer = diag_1_yr)  %>%
    mutate(alc_status_comb = as.numeric(alc_status_comb)) %>%
    mutate(alc_status_comb=case_when(alc_status_comb == 5 ~3,#if drinker with alc problems then join with ex drinker with problems
                                     alc_status_comb == 4~2,
                                     TRUE ~ alc_status_comb)) %>%
    mutate(alc_status_comb = as.factor(alc_status_comb)) %>%
    select(-disease_name)

  #filtering out those in the rand sample from relevant cohort
  data <- data %>% anti_join(rand,by="e_patid")
  
  #union the dataframes
  uni <- union(data,rand)

  #changing gender to 0 and 1
  uni$gender <- ifelse(uni$gender == 2,1,0)
  
  uni2 <- uni[,3:30]
  data_out <- uni2 %>% select(-age)
  data_age <- uni2 %>% select(age)
  
  data_out[sapply(data_out,is.character)] <- lapply(data_out[sapply(data_out,is.character)],as.factor)
  data_out[sapply(data_out,is.numeric)] <- lapply(data_out[sapply(data_out,is.numeric)],as.factor)
  data_out$unw_score <- as.numeric(data_out$unw_score)
  
  data_out <- data_out %>%
    mutate(elix_band = as.factor(cut(data_out$unw_score,breaks = c(0,1,3,5,25),include.lowest=TRUE)))%>%
    mutate(oesoph_ulc_bin = as.factor(ifelse(oesoph_ulc == 0,0,1))) %>%
    mutate(gord_bin = as.factor(ifelse(gord == 0,0,1))) %>%
    mutate(barretts_bin = as.factor(ifelse(barretts == 0,0,1))) %>%
    mutate(gastritis_duodenitis_bin = as.factor(ifelse(gastritis_duodenitis == 0,0,1))) %>%
    mutate(hernia_abdo_bin = as.factor(ifelse(hernia_abdo == 0,0,1)))
  
  data_out_all <- cbind(data_out,data_age)
  return(as.data.frame(data_out_all))
}

#--------------------------------------------------------------------------------
### Function to send the data to excel for risk calculation and save risk plots based on the table values

model_foo <- function(model_data,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename){
  #REGRESSION WITH ALL VARIABLES AND AGE WITH KNOTS
  #scale age
  #model_data <- gord_out
  model_data <- model_data %>%
    mutate(age_idate = (age-60)/10)
  form <- non_neo_str
  form <- formula(form)
  spline_mod <- glm(form,
                    family = binomial(link="logit"), 
                    data = model_data)
  #summary(spline_mod)
  
  out <- tidy(spline_mod)
  out$estimate_e <- round(exp(out$estimate),3)
  #output odds ratios
  write.xlsx(out,file = ".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Regression risk outputs v6.xlsx",sheetName = sheet_name_odds,append=TRUE)
  
  # #specifying model variables
  # y <- model_data$og_cancer
  # 
  # x <- model_data %>% select(-c(og_cancer,age,unw_score,gord,barretts,oesoph_ulc,gastritis_duodenitis,hernia_abdo))
  # x <- data.matrix(x)
  # 
  # #running cv model
  # gord_mod <- glmnet(x,y,alpha=0,family='binomial')
  # 
  # lambda_min <- gord_mod$lambda.min
  # 
  # plot(gord_mod)
  # 
  # gord_best_mod <- glmnet(x,y,alpha=0,family='binomial',lambda=lambda_min)
  # 
  # coef(gord_best_mod)
  # 
  # exp_beta <- exp(gord_best_mod$beta)
  # 
  # plot(gord_best_mod,xvar='lambda')
  # 
  # preds <- predict(gord_best_mod,s=lambda_min,newx=x)
  
  #creating a table for each age, gender and non neo cat combination
  grouped_data =
    tibble(age = seq(30, 100, length.out = 71)) |>
    mutate(age_idate = (age-60)/10) |>
    mutate(gender = as.factor(0)) |>
    mutate(cat = as.factor(0)) |>
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
    )
  
  # Want to consider both genders, so duplicate
  grouped_data = grouped_data |>
    bind_rows(grouped_data |> mutate(gender = as.factor(1)))
  
  grouped_data = grouped_data |>
    bind_rows(grouped_data |> mutate(cat = as.factor(1)))  
  
  grouped_pred <- predict(spline_mod,grouped_data,type='response')
  
  grouped_data <- cbind(grouped_data,grouped_pred)
  names(grouped_data)[25] <- "pred"
  grouped_data <- as.data.frame(grouped_data)
  
  grouped_data <- grouped_data %>% 
    select(c(age,gender,cat,pred))%>%
    arrange(cat,gender)
  
  write.xlsx(grouped_data,file = ".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Regression risk outputs v6.xlsx",sheetName = sheet_name_risk,append=TRUE)

  #create the risk table for each disease group and pairwise combination
  out_fil <- as.data.frame(cbind(out$term,out$estimate_e,out$p.value))
  names(out_fil) <- c("variable","estimate_e","p_val")
  
  out_fil2 <- out_fil[7:31,]
  
  out_fil2$estimate_e <- as.numeric(out_fil2$estimate_e)
  
  grouped_data <- grouped_data %>%
    mutate(odds = pred/(1-pred))
  
  generate_multiplication_table <- function(df1, column1, df2, column2){
    multiplication_table <- as.matrix(df1[[column1]]) %*% t(as.matrix(df2[[column2]]))
    colnames(multiplication_table) <- df2$variable
    final_table <- cbind(grouped_data,multiplication_table)
    return(final_table)
  }
  
  tab <- generate_multiplication_table(grouped_data,"odds",out_fil2,"estimate_e")
  
  risk_tab <- cbind(tab[,1:5],(tab[,6:ncol(tab)]/(1+tab[6:ncol(tab)])))
  
  write.xlsx(risk_tab,file = ".\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Regression risk outputs v6.xlsx",sheetName = risk_tab_name,append=TRUE)
  ## Creating a long table format for plotting
  
  risk_tab_simp <- risk_tab %>% select(-odds)#cbind(risk_tab[,1:4],risk_tab[,6:ncol(risk_tab)])
  
  long_df <- pivot_longer(risk_tab_simp,cols=-c(age,gender,cat),names_to = "Covariate",values_to = "Risk")
  
  levels(long_df$gender) <- c("Male","Female")
  levels(long_df$cat) <- c("Background risk",var_name)
  
  long_df$Risk <- long_df$Risk * 100
  
  # nb.cols <- 23
  # myColours <- colorRampPalette(brewer.pal(11,'Set3'),bias=2)(nb.cols)
  baseline_data <- long_df %>% filter(Covariate == "pred")
  names(baseline_data)[4] <- "risk"
  names(baseline_data)[3] <- "Baseline"
  
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
                                                                                 ifelse(long_df$Covariate == "hernia_abdo_bin1","Abdominal hernia",
                                                                                        ifelse(long_df$Covariate == "barretts_bin1","Barretts",
                                                                                               ifelse(long_df$Covariate == "oesoph_ulc_bin1","Oesophageal ulcer",
                                                                                                      ifelse(long_df$Covariate == "h_pylori_flag1","H.pylori treatment",
                                                                                               ifelse(long_df$Covariate == "elix_band(1,3]","Elix score 1-2",
                                                                                                      ifelse(long_df$Covariate == "elix_band(3,5]","Elix score 3-4",
                                                                                                             ifelse(long_df$Covariate == "elix_band(5,25]","Elix score 5+",
                                                                                                                    ifelse(long_df$Covariate == "vomiting1","Vomiting",
                                                                                                                           ifelse(long_df$Covariate == "abd_pain1","Abdominal pain",
                                                                                                                                  ifelse(long_df$Covariate == "appetite_loss1","Appetite loss",
                                                                                                                                         ifelse(long_df$Covariate == "raise_platelets1","Raised platelets",
                                                                                                                                                ifelse(long_df$Covariate == "raise_wbc1","Raised WBCs",NA
                                                                                                                                                )))))))))))))))))))))))))))
  long_df <- long_df %>% 
    filter(!is.na(Covariate)) %>% 
    filter(cat != "Background risk") %>%
    mutate('Co-occurring feature' = Covariate)
  
  #creating a list of colours for each variable
  colour_vec <- c("red","#66cc99","#0066CC","#ffff00","#FF0066","#FF9900","#006600","#660000","#000099",
                  "#660099","#a3a500","#663300","pink","#0099cc","#669900","#f8766d","#003300","#ff00cc","#330055"
                  ,"#666990","#666615","#002355",'#ff5000','#ff0022','#692000','aquamarine2','seagreen','violet'
  )
  
  names(colour_vec) <- c("Dyspepsia","Dysphagia","Vomiting","Weight loss","Anaemia","Raised platelets","Raised WBCs","Abdominal pain",
                         "GORD","Gastritis/duodenitis","Barretts","Cough","Oesophageal ulcer","Abdominal hernia","Fatigue","Appetite loss",
                         "Ex-smoker","Current smoker","Elix score 1-2","Elix score 3-4","Elix score 5+","Underweight","Overweight",
                         "Obese","Ever drinker","Ever drinker with alc problems","H.pylori treatment")
  
  plot <- ggplot()+
    geom_line(long_df,mapping=aes(age,Risk,colour=`Co-occurring feature`))+
    labs(fill='Co-occurring feature') +
    geom_line(data=baseline_data,mapping=aes(age,Risk,linetype=Baseline,group=Baseline)) +
    geom_hline(yintercept=3,linetype='twodash',color='red') +
    facet_grid(c('gender'))+
    ylab("Risk (%)")+#+geom_point(aes(shape=long_df$bold))+
    scale_colour_manual(values=colour_vec)
  ggsave(plot,path=filepath,filename=filename,width=8,height=6,dpi=200)
  }


#-----------------------------------------------------------------------------
#Loading random sample data
query <- 'SELECT * FROM freya.random_sample_final_v2;'
rs <- dbSendQuery(db, query)
rand <- fetch(rs, n=-1)

#-------------------------------
#Flagging patients who form part of the main cohorts - ACCIDENTALLY REMOVED THIS COLUMN IN BMI CALCULATION FUNCTION, WILL RE-RUN LATER, TEMP FIX FOR NOW

query <- 'SELECT * FROM disease_index_gastritis_duodenitis_all_flags_sm_fin_elix;'
rs <- dbSendQuery(db, query)
gastritis <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_gord_all_flags_sm_fin_elix;'
rs <- dbSendQuery(db, query)
gord <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_barretts_all_flags_sm_fin_elix;'
rs <- dbSendQuery(db, query)
barretts <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_oesoph_ulc_all_flags_sm_fin_elix;'
rs <- dbSendQuery(db, query)
oesoph_ulc <- fetch(rs, n=-1)

query <- 'SELECT * FROM disease_index_hernia_abdo_all_flags_sm_fin_elix;'
rs <- dbSendQuery(db, query)
hernia_abdo <- fetch(rs, n=-1)

all_cohorts <- cbind(bind_rows(gastritis[1],gord[1],barretts[1],oesoph_ulc[1],hernia_abdo[1]),
                     bind_rows(gastritis[3],gord[3],barretts[3],oesoph_ulc[3],hernia_abdo[3]))

rand <- rand %>%
  left_join(all_cohorts) %>%
  distinct()
#---------
#the filepath to save the plots
filepath <- "S:/ECHO_IHI_CPRD/Freya/Project 1/Count Tables & Graphs/Study 1 outputs/Results 21042025"
#----------------------------------------
#GORD cohort
query <- 'SELECT * FROM freya.cohort_gord_final;'
rs <- dbSendQuery(db, query)
gord <- fetch(rs, n=-1)

#data formatting
cohort_var <- 'gord'
gord_out <- data_format_non_neo(gord,rand,cohort_var)

#running model and saving results to excel
cohort_column <- as.name('gord')
non_neo_str <- "og_cancer ~ ns(age_idate, df=3)+cat+gender+alc_status_comb+final_smoking_status+low_hb+
  dyspepsia+dysphagia+cough+elix_band+vomiting+abd_pain+bmi_cat+h_pylori_flag+fatigue+gastritis_duodenitis_bin+
  raise_platelets+raise_wbc+hernia_abdo_bin+barretts_bin+oesoph_ulc_bin+weight_loss"
sheet_name_odds <- "Spline model output-GORD"
sheet_name_risk <- "Risk outputs-GORD"
risk_tab_name <- "Risk table GORD"
var_name <- "GORD"
filename <- "gord_risk_v5.png"

model_foo(gord_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

#----------------------------------------
#Barretts cohort
query <- 'SELECT * FROM freya.cohort_barretts_final;'
rs <- dbSendQuery(db, query)
barretts <- fetch(rs, n=-1)

#data formatting
cohort_var <- 'barretts'

barretts_out <- data_format_non_neo(barretts,rand,cohort_var)

#running model and saving results to excel
cohort_column <- as.name('barretts')
#removing appetite loss as all 0s
non_neo_str <- "og_cancer ~ ns(age_idate, df=3)+cat+gender+alc_status_comb+final_smoking_status+low_hb+bmi_cat+h_pylori_flag+
  weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+hernia_abdo_bin+gastritis_duodenitis_bin+gord_bin+oesoph_ulc_bin"
sheet_name_odds <- "Spline model output-barretts"
sheet_name_risk <- "Risk outputs-barretts"
risk_tab_name <- "Risk table Barretts"
var_name <- "Barretts"
filename <- "barretts_risk_v5.png"

model_foo(barretts_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

#----------------------------------------
#oesoph_ulc cohort
query <- 'SELECT * FROM freya.cohort_oesoph_ulc_final;'
rs <- dbSendQuery(db, query)
oesoph_ulc <- fetch(rs, n=-1)

#data formatting
cohort_var <- 'oesoph_ulc'

oesoph_ulc_out <- data_format_non_neo(oesoph_ulc,rand,cohort_var)

#running model and saving results to excel
cohort_column <- as.name('oesoph_ulc')
non_neo_str <- "og_cancer ~ ns(age_idate, df=3)+cat+gender+alc_status_comb+final_smoking_status+low_hb+bmi_cat+h_pylori_flag+
  weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+hernia_abdo_bin+gastritis_duodenitis_bin+gord_bin+barretts_bin"
sheet_name_odds <- "Spline modeloutput-oesoph_ulc"
sheet_name_risk <- "Risk outputs-oesoph_ulc"
risk_tab_name <- "Risk table oesophageal ulcer"
var_name <- "Oesophageal ulcer"
filename <- "oesoph_ulc_risk_v5.png"

model_foo(oesoph_ulc_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

#----------------------------------------
#gastritis_duodenitis cohort
query <- 'SELECT * FROM freya.cohort_gastritis_duodenitis_final;'
rs <- dbSendQuery(db, query)
gastritis_duodenitis <- fetch(rs, n=-1)

#data formatting
cohort_var <- 'gastritis_duodenitis'

gastritis_duodenitis_out <- data_format_non_neo(gastritis_duodenitis,rand,cohort_var)

#running model and saving results to excel
cohort_column <- as.name('gastritis_duodenitis')
non_neo_str <- "og_cancer ~ ns(age_idate, df=3)+cat+gender+alc_status_comb+final_smoking_status+low_hb+bmi_cat+h_pylori_flag+
  weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+hernia_abdo_bin+oesoph_ulc_bin+gord_bin+barretts_bin"
sheet_name_odds <- "Spline model output-gastritis"
sheet_name_risk <- "Risk outputs-gastritis"
risk_tab_name <- "Risk table gastritis"
var_name <- "Gastritis/duodenitis"
filename <- "gastritis_duodenitis_risk_v5.png"

model_foo(gastritis_duodenitis_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

#----------------------------------------
#hernia_abdo cohort
query <- 'SELECT * FROM freya.cohort_hernia_abdo_final;'
rs <- dbSendQuery(db, query)
hernia_abdo <- fetch(rs, n=-1)

#data formatting
cohort_var <- 'hernia_abdo'

hernia_abdo_out <- data_format_non_neo(hernia_abdo,rand,cohort_var)

#running model and saving results to excel
cohort_column <- as.name('hernia_abdo')
non_neo_str <- "og_cancer ~ ns(age_idate,df=3)*cat+gender+alc_status_comb+final_smoking_status+low_hb+bmi_cat+h_pylori_flag+
  weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin"
sheet_name_odds <- "Spline model output-hernia"
sheet_name_risk <- "Risk outputs-hernia_abdo"
risk_tab_name <- "Risk table hernia abdo"
var_name <- "Diaphragmatic hernia"
filename <- "hernia_abdo_risk_v5.png"

model_foo(hernia_abdo_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

#---------------------------------------------------------------------
##A function to plot only the significant risk estimates

#### EDITING TO ONLY PLOT THE RANDOM SAMPLE BASELINE ON THE MALE AND FEMALE PLOTS

model_foo <- function(model_data,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename){
  #REGRESSION WITH ALL VARIABLES AND AGE WITH KNOTS
  #scale age
 #model_data <- hernia_abdo_out
  model_data <- model_data %>%
    mutate(age_idate = (age-60)/10)
  form <- non_neo_str
  form <- formula(form)
  spline_mod <- glm(form,
                    family = binomial, 
                    data = model_data)


  #creating a table for each age, gender and non neo cat combination
  grouped_data =
    tibble(age = seq(30, 100, length.out = 71)) |>
    mutate(age_idate = (age-60)/10) |>
    mutate(gender = as.factor(0)) |>
    mutate(cat = as.factor(0)) |>
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
    )
  
  # Want to consider both genders, so duplicate
  grouped_data = grouped_data |>
    bind_rows(grouped_data |> mutate(gender = as.factor(1)))
  
  grouped_data = grouped_data |>
    bind_rows(grouped_data |> mutate(cat = as.factor(1)))  
  
  grouped_pred <- predict(spline_mod,grouped_data,type='response')
  
  grouped_data <- cbind(grouped_data,grouped_pred)
  names(grouped_data)[25] <- "pred"
  grouped_data <- as.data.frame(grouped_data)
  
  grouped_data <- grouped_data %>% 
    select(c(age,gender,cat,pred))%>%
    arrange(cat,gender)
  
  #create the risk table for each disease group and pairwise combination
  out <- tidy(spline_mod)
  out$estimate_e <- round(exp(out$estimate),3)
  
  out_fil <- as.data.frame(cbind(out$term,out$estimate_e,out$p.value))
  names(out_fil) <- c("variable","estimate_e","p_val")
  
  out_fil2 <- out_fil[7:32,]
  
  out_fil2$estimate_e <- as.numeric(out_fil2$estimate_e)
  
  #keeping only significant values
  out_fil2$p_val <- as.numeric(out_fil2$p_val)
  out_fil2$sig <- ifelse(round(out_fil2$p_val,2) <= 0.05,1,0)
  # out_fil2 <- out_fil2 %>%
  #   filter(sig == 1)
  
  #changing this to only plotting the combinations where sample size is greater than 100
  sample_data <- colSums(model_data == 1)
  sample_data <- as.data.frame(sample_data)
  sample_data <- rownames_to_column(sample_data,var="variable")
  sample_data <- sample_data %>% rename(n = sample_data) %>%
    mutate(variable = paste0(variable,"1"))
  
  out_fil2 <- left_join(out_fil2,sample_data,by="variable")
  
  elix <- as.data.frame(table(model_data$elix_band))
  elix <- elix %>%
    mutate(Var1 = paste0("elix_band",Var1))
  
  smoking <- as.data.frame(table(model_data$final_smoking_status))
  smoking <- smoking %>%
    mutate(Var1 = paste0("final_smoking_status",Var1))
  
  alc <- as.data.frame(table(model_data$alc_status_comb))
  alc <- alc %>%
    mutate(Var1 = paste0("alc_status_comb",Var1))
  
  bmi <- as.data.frame(table(model_data$bmi_cat))
  bmi <- bmi %>%
    mutate(Var1 = paste0("bmi_cat",Var1))

  out_fil2 <- out_fil2 %>%
    left_join(elix,join_by("variable"=="Var1"))%>%
    left_join(smoking,join_by("variable"=="Var1"))%>%
    left_join(alc,join_by("variable"=="Var1"))%>%
    left_join(bmi,join_by("variable"=="Var1"))
  
  out_fil2 <- out_fil2 %>%
    mutate(combined = coalesce(n,Freq.x,Freq.y,Freq.x.x,Freq.y.y)) %>%
    select(-c(n,Freq.x,Freq.y,Freq.x.x,Freq.y.y)) %>%
    filter(combined > 100)
   
  grouped_data <- grouped_data %>%
    mutate(odds = pred/(1-pred))
  
  generate_multiplication_table <- function(df1, column1, df2, column2){
    multiplication_table <- as.matrix(df1[[column1]]) %*% t(as.matrix(df2[[column2]]))
    colnames(multiplication_table) <- df2$variable
    final_table <- cbind(grouped_data,multiplication_table)
    return(final_table)
  }
  
  tab <- generate_multiplication_table(grouped_data,"odds",out_fil2,"estimate_e")
  
  risk_tab <- cbind(tab[,1:5],(tab[,6:ncol(tab)]/(1+tab[6:ncol(tab)])))
  
  ## Creating a long table format for plotting
  
  risk_tab_simp <- risk_tab %>% select(-odds)#cbind(risk_tab[,1:4],risk_tab[,6:ncol(risk_tab)])
  
  long_df <- pivot_longer(risk_tab_simp,cols=-c(age,gender,cat),names_to = "Covariate",values_to = "Risk")
  
  levels(long_df$gender) <- c("Men","Women")
  levels(long_df$cat) <- c("Background risk",var_name)
  
  long_df$Risk <- long_df$Risk * 100

  baseline_data <- long_df %>% filter(Covariate == "pred")
  names(baseline_data)[4] <- "risk"
  names(baseline_data)[3] <- "Baseline"
  
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
                                                                                        ifelse(long_df$Covariate == "alc_status_comb3","Ever drinker with alcohol problems",
                                                                                               ifelse(long_df$Covariate == "gord_bin1","GORD",
                                                                                                      ifelse(long_df$Covariate == "gastritis_duodenitis_bin1","Gastritis/Duodenitis",
                                                                                                             ifelse(long_df$Covariate == "hernia_abdo_bin1","Diaphragmatic hernia",
                                                                                                                    ifelse(long_df$Covariate == "oesoph_ulc_bin1","Oesophageal ulcer",
                                                                                                                    ifelse(long_df$Covariate == "barretts_bin1","Barretts",
                                                                                                                           ifelse(long_df$Covariate == "h_pylori_flag","H.pylori treatment",
                                                                                                                           ifelse(long_df$Covariate == "elix_band(1,3]","Elix score 1-2",
                                                                                                                                  ifelse(long_df$Covariate == "elix_band(3,5]","Elix score 3-4",
                                                                                                                                         ifelse(long_df$Covariate == "elix_band(5,25]","Elix score 5+",
                                                                                                                                                ifelse(long_df$Covariate == "vomiting1","Vomiting",
                                                                                                                                                       ifelse(long_df$Covariate == "abd_pain1","Upper abdominal pain",
                                                                                                                                                              ifelse(long_df$Covariate == "appetite_loss1","Appetite loss",
                                                                                                                                                                     ifelse(long_df$Covariate == "raise_platelets1","Raised platelets",
                                                                                                                                                                            ifelse(long_df$Covariate == "raise_wbc1","Raised WBCs",NA
                                                                                                                                                                            )))))))))))))))))))))))))))
  long_df <- long_df %>% 
    filter(!is.na(Covariate)) %>% 
    filter(cat != "Background risk") %>%
    mutate('Co-occurring feature' = Covariate)
  
  #creating a list of colours for each variable
  # colour_vec <- c("red","#66cc99","#0066CC","#ffff00","#FF0066","#FF9900","#006600","#660000","#000099",
  #                 "#660099","#a3a500","#663300","pink","#0099cc","#669900","#f8766d","#003300","#ff00cc","#330055"
  #                 ,"#666990","#666615","#002355",'#ff5000','#ff0022','#692000','aquamarine2','seagreen','violet'
  # )
  
  colour_vec <- c("#0066CC","#E31A1C","green4","#6A3D9A","#FF7F00","gold","skyblue2","#FB9A99","palegreen","#CAB2D6",
                  "#FDBF6F","gray70","khaki2","maroon","orchid1","deeppink1","yellow4","yellow3","darkorange4","brown",
                  "#330055","#002355","#003300",'aquamarine2','seagreen',"#660000",'#ff5000')
  
  names(colour_vec) <- c("Dyspepsia","Dysphagia","Vomiting","Weight loss","Anaemia","Raised platelets","Raised WBCs","Upper abdominal pain",
                         "GORD","Gastritis/duodenitis","Barretts","Cough","Oesophageal ulcer","Diaphragmatic hernia","Fatigue","Appetite loss",
                         "Ex-smoker","Current smoker","Elix score 1-2","Elix score 3-4","Elix score 5+","Underweight","Overweight",
                         "Obese","Ever drinker","Ever drinker with alcohol problems","H.pylori treatment")
  
  plot <- ggplot()+
    geom_line(long_df,mapping=aes(age,Risk,colour=`Co-occurring feature`))+
    labs(fill='Co-occurring feature') +
    geom_line(data=baseline_data,mapping=aes(age,Risk,linetype=Baseline,group=Baseline)) +
    geom_hline(yintercept=3,linetype='twodash',color='red') +
    facet_grid(c('gender'))+
    ylab("Risk (%)")+#+geom_point(aes(shape=long_df$bold))+
    scale_colour_manual(values=colour_vec)+
    theme_bw()
  ggsave(plot,path=filepath,filename=filename,width=8,height=8,dpi=200)
}

#----------------------------------------
### Running the function on each cohort

#running model and saving results to excel
cohort_column <- as.name('gord')
non_neo_str <- "og_cancer ~ ns(age_idate, df=3)+cat+gender+alc_status_comb+final_smoking_status+low_hb+
  dyspepsia+dysphagia+cough+elix_band+vomiting+abd_pain+bmi_cat+h_pylori_flag+weight_loss+h_pylori_flag+
  raise_platelets+raise_wbc+hernia_abdo_bin+barretts_bin+oesoph_ulc_bin+gastritis_duodenitis_bin"
sheet_name_odds <- "Spline model output-GORD"
sheet_name_risk <- "Risk outputs-GORD"
risk_tab_name <- "Risk table GORD"
var_name <- "GORD"
filename <- "gord_risk_n_3.png"

model_foo(gord_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

#----------------------------------------
#Barretts cohort

#running model and saving results to excel
cohort_column <- as.name('barretts')
#removing appetite loss as all 0s
non_neo_str <- "og_cancer ~ ns(age_idate, df=3)+cat+gender+alc_status_comb+final_smoking_status+low_hb+bmi_cat+h_pylori_flag+
  weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+hernia_abdo_bin+gastritis_duodenitis_bin+gord_bin+oesoph_ulc_bin"
sheet_name_odds <- "Spline model output-barretts"
sheet_name_risk <- "Risk outputs-barretts"
risk_tab_name <- "Risk table Barretts"
var_name <- "Barretts"
filename <- "barretts_risk_n_2.png"

model_foo(barretts_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

#----------------------------------------
#oesoph_ulc cohort

#running model and saving results to excel
cohort_column <- as.name('oesoph_ulc')
non_neo_str <- "og_cancer ~ ns(age_idate, df=3)+cat+gender+alc_status_comb+final_smoking_status+low_hb+bmi_cat+h_pylori_flag+
  weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+hernia_abdo_bin+gastritis_duodenitis_bin+gord_bin+barretts_bin"
sheet_name_odds <- "Spline modeloutput-oesoph_ulc"
sheet_name_risk <- "Risk outputs-oesoph_ulc"
risk_tab_name <- "Risk table oesophageal ulcer"
var_name <- "Oesophageal ulcer"
filename <- "oesoph_ulc_risk_n_3.png" # 3 is removing appetite loss

model_foo(oesoph_ulc_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

#----------------------------------------
#gastritis_duodenitis cohort

#running model and saving results to excel
cohort_column <- as.name('gastritis_duodenitis')
non_neo_str <- "og_cancer ~ ns(age_idate,df=3)+cat+gender+alc_status_comb+final_smoking_status+low_hb+bmi_cat+h_pylori_flag+
  weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+hernia_abdo_bin+oesoph_ulc_bin+gord_bin+barretts_bin"
sheet_name_odds <- "Spline model output-gastritis"
sheet_name_risk <- "Risk outputs-gastritis"
risk_tab_name <- "Risk table gastritis"
var_name <- "Gastritis/duodenitis"
filename <- "gastritis_duodenitis_risk_n_3.png"

model_foo(gastritis_duodenitis_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

#----------------------------------------
#hernia_abdo cohort

#running model and saving results to excel
cohort_column <- as.name('hernia_abdo')
non_neo_str <- "og_cancer ~ ns(age_idate,df=3)*cat+gender+alc_status_comb+final_smoking_status+low_hb+bmi_cat+h_pylori_flag+
  weight_loss+dyspepsia+dysphagia+fatigue+cough+elix_band+vomiting+abd_pain+raise_platelets+raise_wbc+oesoph_ulc_bin+barretts_bin+gord_bin+gastritis_duodenitis_bin"
sheet_name_odds <- "Spline model output -hernia_abdo"
sheet_name_risk <- "Risk outputs -hernia_abdo"
risk_tab_name <- "Risk table hernia abdo"
var_name <- "Diaphragmatic hernia"
filename <- "hernia_abdo_risk_n_3.png"

model_foo(hernia_abdo_out,cohort_column,non_neo_str,sheet_name_odds,sheet_name_risk,risk_tab_name,var_name,filepath,filename)

