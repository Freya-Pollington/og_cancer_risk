rm(list=ls())
#### Loading Packages ####
library(pacman)

# Install & load packages at same time
pacman::p_load(tidyverse,readxl,ggplot2, broom)

data <- read_excel("*",sheet="Covariate comparison raw values")
data <- as.data.frame(data)
data$Odds <- as.numeric(data$Odds)

data <- data %>%
  select(c(`Co-occurring feature`,Odds,Cohort,std.error)) %>%
  mutate(`Co-occurring feature` = ifelse(`Co-occurring feature` == "Abdominal pain", "Upper abdominal pain",
                                         ifelse(`Co-occurring feature`=="Abdominal hernia","Diaphragmatic hernia", `Co-occurring feature`))) %>%
  mutate(lb_odds = exp((Odds) - 1.96*std.error),
         ub_odds = exp((Odds + 1.96*std.error))) %>%
  mutate(Odds = exp(Odds)) %>%
  select(-std.error)

mod <- readRDS("*")

out <- mod %>% 
  tidy() %>%
  mutate(lb=estimate - 1.96*std.error) %>%
  mutate(ub = estimate + 1.96*std.error) %>%
  mutate(or = round(exp(estimate),3)) %>%
  mutate(lb_odds = round(exp(lb),3)) %>%
  mutate(ub_odds = round(exp(ub),3))

out = out %>%
  select(term,or,lb_odds,ub_odds) %>%
  rename(`Co-occurring feature` = term) %>%
  rename(Odds = or) %>%
  mutate(Cohort = "Combined")

out = out[17:42,]

out$`Co-occurring feature` = 
  ifelse(out$`Co-occurring feature` == "final_smoking_status2","Ex-smoker",
         ifelse(out$`Co-occurring feature` == "final_smoking_status4","Current smoker",
                ifelse(out$`Co-occurring feature` == "bmi_cat1","Underweight",
                       ifelse(out$`Co-occurring feature` == "bmi_cat2","Overweight",
                              ifelse(out$`Co-occurring feature` == "bmi_cat3","Obese",
                                     ifelse(out$`Co-occurring feature` == "low_hb1","Anaemia",
                                            ifelse(out$`Co-occurring feature` == "weight_loss1","Weight loss",
                                                   ifelse(out$`Co-occurring feature` == "dyspepsia1","Dyspepsia",
                                                          ifelse(out$`Co-occurring feature` == "dysphagia1","Dysphagia",
                                                                 ifelse(out$`Co-occurring feature` == "fatigue1","Fatigue",
                                                                        ifelse(out$`Co-occurring feature` == "cough1","Cough",
                                                                               ifelse(out$`Co-occurring feature` == "alc_status_comb2","Ever drinker",
                                                                                      ifelse(out$`Co-occurring feature` == "alc_status_comb3","Ever drinker with alc problems",
                                                                                             ifelse(out$`Co-occurring feature` == "gord_bin1","GORD",
                                                                                                    ifelse(out$`Co-occurring feature` == "gastritis_duodenitis_bin1","Gastritis/duodenitis",
                                                                                                           ifelse(out$`Co-occurring feature` == "hernia_abdo_bin1","Diaphragmatic hernia",
                                                                                                                  ifelse(out$`Co-occurring feature` == "barretts_bin1","Barrett's oesophagus",
                                                                                                                         ifelse(out$`Co-occurring feature` == "h_pylori_flag1","H.pylori test",
                                                                                                                                ifelse(out$`Co-occurring feature` == "elix_band(1,3]","Elixhauser score 1-2",
                                                                                                                                       ifelse(out$`Co-occurring feature` == "elix_band(3,5]","Elixhauser score 3-4",
                                                                                                                                              ifelse(out$`Co-occurring feature` == "elix_band(5,25]","Elixhauser score 5+",
                                                                                                                                                     ifelse(out$`Co-occurring feature` == "vomiting1","Vomiting",
                                                                                                                                                            ifelse(out$`Co-occurring feature` == "abd_pain1","Upper abdominal pain",
                                                                                                                                                                   ifelse(out$`Co-occurring feature` == "appetite_loss1","Appetite loss",
                                                                                                                                                                          ifelse(out$`Co-occurring feature` == "symp_n","Symptom count",
                                                                                                                                                                                 ifelse(out$`Co-occurring feature` == "raise_platelets1","Raised platelets",
                                                                                                                                                                                        ifelse(out$`Co-occurring feature` == "raise_wbc1","Raised WBCs",
                                                                                                                                                                                               ifelse(out$`Co-occurring feature` == "oesoph_ulc_bin1","Oesophageal ulcer",
                                                                                                                                                                                                      NA
                                                                                                                                                                                               ))))))))))))))))))))))))))))


data2 <- union(data,out)

data2 <- data2 %>%
  mutate(Cohort = ifelse(Cohort == "Abdominal pain", "Upper abdominal pain",
                ifelse(Cohort=="Abdominal hernia","Diaphragmatic hernia", Cohort))) %>%
  filter(Odds > 0)

data2$Cohort <- factor(data2$Cohort, levels = c("Combined","Barrett's oesophagus","Diaphragmatic hernia",
                "Dyspepsia", "Dysphagia","Gastritis/duodenitis","GORD","Oesophageal ulcer","Upper abdominal pain","Vomiting"))

data2  <- data2 %>% rename("Model" = "Cohort") 

covars <- unique(data2$`Co-occurring feature`)

covars <- sort(covars)

colour_vec <- c("black","firebrick","#692000","#FF9900","yellow2","palegreen4",'chartreuse4',"skyblue3","royalblue4","#660099")

data2 <- data2 %>%
 filter(ub_odds < 50) %>%
  mutate(Odds = round(Odds,3),
         lb_odds = round(lb_odds,3),
         ub_odds = round(ub_odds,3))

#splitting the data in half to plot twice on two pages in report
ggplot()+
  geom_point(data2 %>% filter(`Co-occurring feature` %in% covars[1:13]) %>% arrange(desc(Model)),mapping=aes(x=Odds,y=`Co-occurring feature`,colour=Model),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(data2%>% filter(`Co-occurring feature` %in% covars[1:13])%>% arrange(desc(Model)),mapping=aes(xmin=lb_odds,xmax=ub_odds,y=`Co-occurring feature`,colour=Model),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec) +
  scale_x_log10() +
  labs(x=expression(paste("Odds ratio ","(log"[10],")")))+
  geom_vline(xintercept=1,linetype='dashed')+
  geom_hline(yintercept=1.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=2.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=3.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=4.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=5.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=6.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=7.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=8.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=9.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=10.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=11.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=12.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=13.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=14.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=15.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=16.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=17.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=18.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=19.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=20.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=21.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=22.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=23.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=24.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=25.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=26.5,linetype='dashed',alpha=0.6)+
  theme_bw()


ggsave("*",width = 8,height=12,dpi=200)

ggplot()+
  geom_point(data2 %>% filter(`Co-occurring feature` %in% covars[14:27]) %>% arrange(desc(Model)),mapping=aes(x=Odds,y=`Co-occurring feature`,colour=Model),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(data2%>% filter(`Co-occurring feature` %in% covars[14:27])%>% arrange(desc(Model)),mapping=aes(xmin=lb_odds,xmax=ub_odds,y=`Co-occurring feature`,colour=Model),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec) +
  scale_x_log10() +
  labs(x=expression(paste("Odds ratio ","(log"[10],")")))+
  geom_vline(xintercept=1,linetype='dashed')+
  geom_hline(yintercept=1.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=2.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=3.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=4.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=5.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=6.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=7.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=8.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=9.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=10.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=11.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=12.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=13.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=14.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=15.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=16.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=17.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=18.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=19.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=20.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=21.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=22.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=23.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=24.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=25.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=26.5,linetype='dashed',alpha=0.6)+
  theme_bw()

ggsave("S:\\ECHO_IHI_CPRD\\Freya\\Project 1\\Count Tables & Graphs\\Study 1 outputs\\Results 21042025\\Covariate value comparison first half drop df.png",width = 8,height=12,dpi=200)

#--------------------------------------------------------------
#Comparing men to women output of combined model
mod <- readRDS("*")

out <- mod %>% 
  tidy() %>%
  mutate(lb=estimate - 1.96*std.error) %>%
  mutate(ub = estimate + 1.96*std.error) %>%
  mutate(or = round(exp(estimate),3)) %>%
  mutate(lb_odds = round(exp(lb),3)) %>%
  mutate(ub_odds = round(exp(ub),3))

out = out %>%
  select(term,or,lb_odds,ub_odds) %>%
  rename(`Co-occurring feature` = term) %>%
  rename(Odds = or) %>%
  mutate(Cohort = "Men")

out = out[16:41,]

out$`Co-occurring feature` = 
  ifelse(out$`Co-occurring feature` == "final_smoking_status2","Ex-smoker",
         ifelse(out$`Co-occurring feature` == "final_smoking_status4","Current smoker",
                ifelse(out$`Co-occurring feature` == "bmi_cat1","Underweight",
                       ifelse(out$`Co-occurring feature` == "bmi_cat2","Overweight",
                              ifelse(out$`Co-occurring feature` == "bmi_cat3","Obese",
                                     ifelse(out$`Co-occurring feature` == "low_hb1","Anaemia",
                                            ifelse(out$`Co-occurring feature` == "weight_loss1","Weight loss",
                                                   ifelse(out$`Co-occurring feature` == "dyspepsia1","Dyspepsia",
                                                          ifelse(out$`Co-occurring feature` == "dysphagia1","Dysphagia",
                                                                 ifelse(out$`Co-occurring feature` == "fatigue1","Fatigue",
                                                                        ifelse(out$`Co-occurring feature` == "cough1","Cough",
                                                                               ifelse(out$`Co-occurring feature` == "alc_status_comb2","Ever drinker",
                                                                                      ifelse(out$`Co-occurring feature` == "alc_status_comb3","Ever drinker with alc problems",
                                                                                             ifelse(out$`Co-occurring feature` == "gord_bin1","GORD",
                                                                                                    ifelse(out$`Co-occurring feature` == "gastritis_duodenitis_bin1","Gastritis/duodenitis",
                                                                                                           ifelse(out$`Co-occurring feature` == "hernia_abdo_bin1","Diaphragmatic hernia",
                                                                                                                  ifelse(out$`Co-occurring feature` == "barretts_bin1","Barrett's oesophagus",
                                                                                                                         ifelse(out$`Co-occurring feature` == "h_pylori_flag1","H.pylori test",
                                                                                                                                ifelse(out$`Co-occurring feature` == "elix_band(1,3]","Elixhauser score 1-2",
                                                                                                                                       ifelse(out$`Co-occurring feature` == "elix_band(3,5]","Elixhauser score 3-4",
                                                                                                                                              ifelse(out$`Co-occurring feature` == "elix_band(5,25]","Elixhauser score 5+",
                                                                                                                                                     ifelse(out$`Co-occurring feature` == "vomiting1","Vomiting",
                                                                                                                                                            ifelse(out$`Co-occurring feature` == "abd_pain1","Upper abdominal pain",
                                                                                                                                                                   ifelse(out$`Co-occurring feature` == "appetite_loss1","Appetite loss",
                                                                                                                                                                          ifelse(out$`Co-occurring feature` == "symp_n","Symptom count",
                                                                                                                                                                                 ifelse(out$`Co-occurring feature` == "raise_platelets1","Raised platelets",
                                                                                                                                                                                        ifelse(out$`Co-occurring feature` == "raise_wbc1","Raised WBCs",
                                                                                                                                                                                               ifelse(out$`Co-occurring feature` == "oesoph_ulc_bin1","Oesophageal ulcer",
                                                                                                                                                                                                      NA
                                                                                                                                                                                               ))))))))))))))))))))))))))))


mod2 <- readRDS("*")

out2 <- mod2 %>% 
  tidy() %>%
  mutate(lb=estimate - 1.96*std.error) %>%
  mutate(ub = estimate + 1.96*std.error) %>%
  mutate(or = round(exp(estimate),3)) %>%
  mutate(lb_odds = round(exp(lb),3)) %>%
  mutate(ub_odds = round(exp(ub),3))

out2 = out2 %>%
  select(term,or,lb_odds,ub_odds) %>%
  rename(`Co-occurring feature` = term) %>%
  rename(Odds = or) %>%
  mutate(Cohort = "Women")

out2 = out2[16:41,]


out2$`Co-occurring feature` = 
  ifelse(out2$`Co-occurring feature` == "final_smoking_status2","Ex-smoker",
         ifelse(out2$`Co-occurring feature` == "final_smoking_status4","Current smoker",
                ifelse(out2$`Co-occurring feature` == "bmi_cat1","Underweight",
                       ifelse(out2$`Co-occurring feature` == "bmi_cat2","Overweight",
                              ifelse(out2$`Co-occurring feature` == "bmi_cat3","Obese",
                                     ifelse(out2$`Co-occurring feature` == "low_hb1","Anaemia",
                                            ifelse(out2$`Co-occurring feature` == "weight_loss1","Weight loss",
                                                   ifelse(out2$`Co-occurring feature` == "dyspepsia1","Dyspepsia",
                                                          ifelse(out2$`Co-occurring feature` == "dysphagia1","Dysphagia",
                                                                 ifelse(out2$`Co-occurring feature` == "fatigue1","Fatigue",
                                                                        ifelse(out2$`Co-occurring feature` == "cough1","Cough",
                                                                               ifelse(out2$`Co-occurring feature` == "alc_status_comb2","Ever drinker",
                                                                                      ifelse(out2$`Co-occurring feature` == "alc_status_comb3","Ever drinker with alc problems",
                                                                                             ifelse(out2$`Co-occurring feature` == "gord_bin1","GORD",
                                                                                                    ifelse(out2$`Co-occurring feature` == "gastritis_duodenitis_bin1","Gastritis/duodenitis",
                                                                                                           ifelse(out2$`Co-occurring feature` == "hernia_abdo_bin1","Diaphragmatic hernia",
                                                                                                                  ifelse(out2$`Co-occurring feature` == "barretts_bin1","Barrett's oesophagus",
                                                                                                                         ifelse(out2$`Co-occurring feature` == "h_pylori_flag1","H.pylori test",
                                                                                                                                ifelse(out2$`Co-occurring feature` == "elix_band(1,3]","Elixhauser score 1-2",
                                                                                                                                       ifelse(out2$`Co-occurring feature` == "elix_band(3,5]","Elixhauser score 3-4",
                                                                                                                                              ifelse(out2$`Co-occurring feature` == "elix_band(5,25]","Elixhauser score 5+",
                                                                                                                                                     ifelse(out2$`Co-occurring feature` == "vomiting1","Vomiting",
                                                                                                                                                            ifelse(out2$`Co-occurring feature` == "abd_pain1","Upper abdominal pain",
                                                                                                                                                                   ifelse(out2$`Co-occurring feature` == "appetite_loss1","Appetite loss",
                                                                                                                                                                          ifelse(out2$`Co-occurring feature` == "symp_n","Symptom count",
                                                                                                                                                                                 ifelse(out2$`Co-occurring feature` == "raise_platelets1","Raised platelets",
                                                                                                                                                                                        ifelse(out2$`Co-occurring feature` == "raise_wbc1","Raised WBCs",
                                                                                                                                                                                               ifelse(out2$`Co-occurring feature` == "oesoph_ulc_bin1","Oesophageal ulcer",
                                                                                                                                                                                                      NA
                                                                                                                                                                                               ))))))))))))))))))))))))))))



data2 <- union(out,out2)

data2 <- data2 %>%
  mutate(Cohort = ifelse(Cohort == "Abdominal pain", "Upper abdominal pain",
                         ifelse(Cohort=="Abdominal hernia","Diaphragmatic hernia", Cohort))) %>%
  filter(Odds > 0)

data2$Cohort <- factor(data2$Cohort, levels = c("Men","Women"))

data2  <- data2 %>% rename("Model" = "Cohort") 

covars <- unique(data2$`Co-occurring feature`)

covars <- sort(covars)

colour_vec <- c("firebrick","royalblue4")
data2 <- data2 %>%
  filter(ub_odds < 50) %>%
  mutate(Odds = round(Odds,3),
         lb_odds = round(lb_odds,3),
         ub_odds = round(ub_odds,3))

#splitting the data in half to plot twice on two pages in report
ggplot()+
  geom_point(data2 %>% filter(`Co-occurring feature` %in% covars[1:13]) %>% arrange(desc(Model)),mapping=aes(x=Odds,y=`Co-occurring feature`,colour=Model),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(data2%>% filter(`Co-occurring feature` %in% covars[1:13])%>% arrange(desc(Model)),mapping=aes(xmin=lb_odds,xmax=ub_odds,y=`Co-occurring feature`,colour=Model),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec) +
  scale_x_log10() +
  labs(x=expression(paste("Odds ratio ","(log"[10],")")))+
  geom_vline(xintercept=1,linetype='dashed')+
  geom_hline(yintercept=1.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=2.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=3.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=4.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=5.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=6.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=7.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=8.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=9.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=10.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=11.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=12.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=13.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=14.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=15.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=16.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=17.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=18.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=19.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=20.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=21.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=22.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=23.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=24.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=25.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=26.5,linetype='dashed',alpha=0.6)+
  theme_bw() 

ggsave("*",width = 8,height=12,dpi=200)


ggplot()+
  geom_point(data2 %>% filter(`Co-occurring feature` %in% covars[14:26]) %>% arrange(desc(Model)),mapping=aes(x=Odds,y=`Co-occurring feature`,colour=Model),alpha=0.75,position=position_dodge(width=1))+
  geom_errorbar(data2%>% filter(`Co-occurring feature` %in% covars[14:26])%>% arrange(desc(Model)),mapping=aes(xmin=lb_odds,xmax=ub_odds,y=`Co-occurring feature`,colour=Model),width=0.5,alpha=0.5,position=position_dodge(width=1))+
  scale_colour_manual(values=colour_vec) +
  scale_x_log10() +
  labs(x=expression(paste("Odds ratio ","(log"[10],")")))+
  geom_vline(xintercept=1,linetype='dashed')+
  geom_hline(yintercept=1.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=2.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=3.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=4.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=5.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=6.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=7.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=8.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=9.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=10.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=11.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=12.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=13.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=14.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=15.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=16.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=17.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=18.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=19.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=20.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=21.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=22.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=23.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=24.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=25.5,linetype='dashed',alpha=0.6)+
  geom_hline(yintercept=26.5,linetype='dashed',alpha=0.6)+
  theme_bw()

ggsave("*",width = 8,height=12,dpi=200)
