#### May 24, 2022 ####




# Arterial thrombus 0=no, 1=yes  rare event



#readxl::read_xlsx() For reading excel data
#dplyr::recode for recoding the value
#replace  for similar function as recode
#broom::tidy for converting data to a tidy table and confident interval
#broom::augment for augmenting data with information from an object
#broom::glance for constructing a single row summary "glance" of a model, fit, or other object
#broom::as_flextable for outputing regression results
#summarytools::dfSummary for descriptive statistics of all varialbe
##summarytools::view to show dfSummary results
#compareGroups::compareGroups for creating the quick relationships between variables
#compareGroups::createTable building a "compact" and "nice" table with the descriptives by groups
#flextable::flextable for creating report table
#flextable::autofit  whentable widths and heights should automatically be adjusted
#flextable::save_as_docx for outputing flexalbe to docx file


#stats::shapiro.test for Shapiro-Wilk test regarding normal distribution
#car::leveneTest for Levene Test regarding equal variance

#stats::kruskal.test for Kruskal Test (non normal anova)
#FSA::dunnTest for Dunn Post Hoc test after Kruskal Test
#stats::oneway.test for Welch's anova (non equal variance anova)
#rstatix::games_howell_test for Games-Howell Post Hoc Test after Welch's anova test










# Reading the dataset
rm(list=ls())
pacman::p_load(survival,tidyverse,summarytools,compareGroups,readr,flextable,broom,gtools )
setwd("G:\\My Drive\\PublicHealthProject\\Covid")

options("scipen"=100, "digits"=4)


covid <- read_csv("Covid.csv")



#check the missing values


view(dfSummary(covid, varnumbers=FALSE, display.labels=FALSE,graph.col=FALSE,na.col=TRUE,valid.col=FALSE),file="missing.html")






# data cleaning


covid_temp <- covid %>%
              mutate(`Sex (1=female, 0=male)`=dplyr::recode(
                     `Sex (1=female, 0=male)`,`1`=1L,`0`=0L,.default=NULL),
                      white=dplyr::recode(Race,WHITE=1L,White=1L,.default=0L,.missing=NULL),
              `Peripheral Vascular disease YES=1 NO=0`=dplyr::recode(`Peripheral Vascular disease YES=1 NO=0`,
              O=0L,"0"=0L,"1"=1L,.default=NULL,.missing=NULL),
              `Cancer YES=1 NO=0`= dplyr::recode(`Cancer YES=1 NO=0`, `0`=0L,`1`=1L,.default=NULL),
              `Antiplatelet medication YES=1 NO=0`=dplyr::recode(`Antiplatelet medication YES=1 NO=0`,
              "0"=0L, "1"=1L, N=0L,O=0L,.default = NULL),
              BMI=as.numeric(BMI),
              `SIRS criteria positive YES=1 NO=0` = dplyr::recode(`SIRS criteria positive YES=1 NO=0`,
              `0`=0L,`1`=1L,.default=NULL),
              `Heart rate (1=Tachycardia >90 beats/minute, 0=<90 beats/minute) YES=1 NO=0`= dplyr::recode(
              `Heart rate (1=Tachycardia >90 beats/minute, 0=<90 beats/minute) YES=1 NO=0`, `0`=0L,
              `1`=1L,`81`=0L,.default = NULL),
              `Respiratory rate (1=Tachypnea >20 breaths/minute, 0=<20) YES=1 NO=0`=dplyr::recode(
              `Respiratory rate (1=Tachypnea >20 breaths/minute, 0=<20) YES=1 NO=0`,
              `0`=0L,`1`=1L,`18`=0L,.default = NULL),
              `Serum Creatinine:`=as.numeric(`Serum Creatinine:`),
              `Sodium`=as.numeric(`Sodium`))

col.n <- c("Sex (1=female, 0=male)",
           "white","Congestive heart failure YES=1 NO=0",
           "Hypertension YES=1 NO=0",
           "Coronary artery disease YES=1 NO=0",
           "Myocardial infarction YES=1 NO=0",
           "Diabetes Mellitus  YES=1 NO=0",
           "Chronic Kidney Disease YES=1 NO=0",
           "Peripheral Vascular disease YES=1 NO=0",
           "Ischemic Stroke   YES=1 NO=0",
           "Tra0sie0t ischemic attack* YES=1 0O=0",
           "NEUROLOGIC DISORDER (ALZHEIMER, ALS, DEMENTIA, PARKINSON, ETC) YES=1 NO=0",
           "Cancer YES=1 NO=0",
           "Symptomatic Intracerebral hemorrhage  YES=1 NO=0",
           "Atrial fibrillation* YES=1 NO=0",
           "Hyperlipidemia YES=1 NO=0",
           "Transplant recipient YES=1 NO=0",
           "Hepatitis B infection YES=1 NO=0",
           "Hepatitis C infection YES=1 NO=0",
           "HIV infection YES=1 NO=0",
           "Antihypertensive medication YES=1 NO=0",
           "Lipid lowering medication YES=1 NO=0",
           "Antiplatelet medication YES=1 NO=0",
           "Intubated (NO=0, YES=1)",
           "SIRS criteria positive YES=1 NO=0",
           "Temperature ( 1= fever >38.0°C or hypothermia <36.0°C, 0=36-38C) YES=1 NO=0",
           "Heart rate (1=Tachycardia >90 beats/minute, 0=<90 beats/minute) YES=1 NO=0",
           "Respiratory rate (1=Tachypnea >20 breaths/minute, 0=<20) YES=1 NO=0")

col.c <- c("Age",
           "CHADS2Vasc Score - Calculated",
           "Heart Rate",
           "Systolic Blood Pressure",
           "Diastolic Blood Pressure",
           "Temperature",
           "Respiratory Rate",
           "SpO2",
           "Weight",
           "Height",
           "BMI",
           "White Blood Cell Count",
           "Hemoglobin",
           "Mean Corpuscular Volume (MCV)",
           "Hematocrit",
           "Platelets",
           "Albumin",
           "ALT",
           "AST",
           "Serum Creatinine:",
           "Blood Urea Nitrogen (BUN)",
           "Sodium",
           "Potassium",
           "Random Blood Glucose",
           "D Dimer")






covid_temp[col.n] <- lapply(covid_temp[col.n], as.numeric)
covid_temp[col.c] <- lapply(covid_temp[col.c], as.factor)
covid_final <- covid_temp[,c(col.n,col.c,"Arterial thrombus 0=no, 1=yes")]



#check missing again
view(dfSummary(covid_final, varnumbers=FALSE, display.labels=FALSE,graph.col=FALSE,na.col=TRUE,valid.col=FALSE))




#Descriptive Statistics
#descript <- final %>%
#            group_by(Group) %>% 
#            summarize(n=n(),
#            min = min(NFL),
#            q1 = quantile(NFL, 0.25),
#            median = median(NFL),
#            mean = mean(NFL),
#            q3 = quantile(NFL, 0.75),
#            max = max(NFL))%>%
#            mutate_if(is.numeric,round,3)%>%
#            flextable()




#group comparison





