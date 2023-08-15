#Merge data 
library(rofi)
data <- rofi::import_data()
names <- c("swetrau","fmp","atgarder","problem","kvalgranskning2014.2017")
names(data) <- names
combined.dataset <- rofi::merge_data(data)

## Create OFI column
combined.dataset$ofi <- rofi::create_ofi(combined.dataset)

#packages 
library(dplyr)
library(ggplot2)
library(gtsummary)
library(skimr)
library(tidyverse)
library(broom.mixed)
library(boot)
library(table1)
library(ISLR)
library(nnet)
library(descr)
library(labelled)
library(knitr)


## Fix formating and remove wrong values like 999
source("clean_all_predictors.R")
combined.dataset <- clean_all_predictors(combined.dataset)
source("clean_audit_filters.R")
## clean Audit filters
combined.dataset <- clean_audit_filters(combined.dataset)

# Clean data 
source("clean_all_predictors.R")


## clean Audit filters
combined.dataset <- clean_audit_filters(combined.dataset)

# Clean data 
source("clean_all_predictors.R")


## Separate and store cases without unknown outcome (OFI)
missing.outcome <- is.na(combined.dataset$ofi)
combined.dataset <- combined.dataset[!missing.outcome,]

## remove patients < 15 years
#combined.dataset <- combined.dataset[combined.dataset$pt_age_yrs > 14,]



## Create cohorts 
source("create_cohorts.R")
new.dataset <- create_cohorts(combined.dataset)

## Creating "other cohort" for patients who does not fit into other cohort
new.dataset$cohort <- as.character(new.dataset$cohort)
new.dataset[is.na(new.dataset$cohort), "cohort"] <- "other cohort"




# Creating column with categories of OFIs based on different areas of improvement 
new.dataset$OFI_categories <- ifelse(new.dataset$Problemomrade_.FMP %in% c("Handläggning", "Handläggning/logistik", 
                                                                           "kompetensbrist","kompetens brist", "Vårdnivå", 
                                                                           "Triage på akm", "Triage på akutmottagningen"), 
                                     "Clinical judgement error", 
                                     
                                     ifelse(new.dataset$Problemomrade_.FMP %in% c("Missad skada", "Lång tid till DT"), 
                                            "Missed diagnosis", 
                                            
                                            ifelse(new.dataset$Problemomrade_.FMP %in% c("Lång tid till op"), 
                                                   "Delay in treatment", 
                                                   
                                                   ifelse(new.dataset$Problemomrade_.FMP %in% c("Logistik/teknik"), 
                                                          "Technical errors",
                                                          
                                                          ifelse(new.dataset$Problemomrade_.FMP %in% c("Traumakriterier/styrning", "Dokumentation","Dokumetation", "Kommunikation", "Tertiär survey",
                                                                                                       "Bristande rutin","bristande rutin", "Neurokirurg","Resurs"), 
                                                                 "Other",
                                                                 
                                                                 
                                                                 
                                                                 
                                                                 ifelse(new.dataset$ofi %in% c("No"), 
                                                                        "No ofi", 
                                                                        ifelse(new.dataset$month_surv %in% c("dead") & new.dataset$preventable_death %in% c("possibly preventable"), 
                                                                               "Possibly preventable", 
                                                                               "Other"
                                                                        )))))))




#merge all cohorts that are not blunt multisystem without TBI for table 1
new.dataset <- new.dataset %>%
  mutate(cohort = ifelse(cohort != "blunt multisystem without TBI", "other cohort", cohort))

  
##  Formate columns for table1
source("Format_table_folak2.R")
  
## Create table1 
source("table1_folak2.R")


## create dataset with only BM without TBI
bm_without_tbi <- subset(new.dataset, cohort == "blunt multisystem without TB")



## regession model 1, 
# judgment_error=age + gender 

## regression model 2
# resu
## regression model 3 


