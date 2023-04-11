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
library(skimr)
library(tidyverse)
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
#table(new.dataset$cohort)

#Creating column where possibly preventable death and preventable deaths are merged 
new.dataset$preventable_death <- ifelse(new.dataset$Fr1.14 == 2 | new.dataset$Fr1.14 == 3, "possibly preventable", "non-preventable")
#new.dataset$preventable_death[is.na(new.dataset$preventable_death)] <- "other"
new.dataset$preventable_death <- ifelse(is.na(new.dataset$preventable_death) == TRUE & new.dataset$res_survival == 2, "survived", new.dataset$preventable_death)
new.dataset$preventable_death <- ifelse(is.na(new.dataset$preventable_death) == TRUE & new.dataset$res_survival == 1, "Not reviewed", new.dataset$preventable_death)
#table(new.dataset$preventable_death)

#Creating coulmn for 30-day survival
new.dataset$month_surv <- ifelse(new.dataset$res_survival == 2,  "alive", "dead")

#new.dataset$month_surv <- ifelse(is.na(new.dataset$month_surv) == TRUE, "other", new.dataset$month_surv)


# Creating column witg categories of OFIs based on different areas of improvement 
new.dataset$OFI_categories <- ifelse(new.dataset$Problemomrade_.FMP %in% c("Handläggning", "Handläggning/logistik", 
                                                                           "kompetensbrist","kompetens brist", "Vårdnivå", 
                                                                           "Triage på akm", "Triage på akutmottagningen"), 
                                     "Judgement error", 
                                     
                                     ifelse(new.dataset$Problemomrade_.FMP %in% c("Missad skada", "Lång tid till DT"), 
                                            "Diagnosis", 
                                            
                                            ifelse(new.dataset$Problemomrade_.FMP %in% c("Lång tid till op"), 
                                                   "Delays", 
                                                   
                                                   ifelse(new.dataset$Problemomrade_.FMP %in% c("Logistik/teknik"), 
                                                          "Technical",
                                                          
                                                          ifelse(new.dataset$Problemomrade_.FMP %in% c("Traumakriterier/styrning", "Dokumentation","Dokumetation", "Kommunikation", "Tertiär survey",
                                                                                                       "Bristande rutin","bristande rutin", "Neurokirurg","Resurs"), 
                                                                 "Other",
                                                                 
                                                                 ifelse(new.dataset$ofi %in% c("No"), 
                                                                        "No ofi", 
                                                                        ifelse(new.dataset$month_surv %in% c("dead") & new.dataset$preventable_death %in% c("possibly preventable"), 
                                                                               "Preventable?", 
                                                                               "Other"
                                                                        )))))))
#table(new.dataset$OFI_categories)


################
#Create table1##
################


# Get the subset of your combined dataset that includes only the columns needed for the table
# JA: Man måste inte ta ett dataset med bara de relevanta kolumnerna när man gör table 1 via det paketet.
#table_cols <- c("OFI_categories", "pt_age_yrs", "Gender", "severe_head_injury", "low_GCS", 
#                "ed_gcs_sum", "intub", "pre_gcs_sum", "inj_dominant", "Severe_penetrating", "cohort", "OFI_categories", "preventable_death", "month_surv")
#table_dataset <- new.dataset[, table_cols]

# Remove rows with missing values only for the columns included in the table
## JA: Ok, förslag är att ta complete case på age, gender, och antingen ISS eller NISS samt om kolumnen OFI är NA

## MM:

# Select the columns to include in the table
selected_cols <- c( "pt_age_yrs", "Gender", "OFI_categories", "ed_gcs_sum", "intub", "NISS", "severe_head_injury", "intub")

#Subset the dataset to only include complete cases for the selected columns
table_dataset <- new.dataset[complete.cases(new.dataset[, selected_cols]), ]


# Create a new variable with shortened cohort names
table_dataset <- table_dataset %>% 
  mutate(cohort_short = recode(cohort,
                               "blunt multisystem without TBI" = "Blunt without TBI",
                               "blunt multisystem with TBI" = "Blunt with TBI",
                               "Isolated severe TBI" = "TBI",
                               "severe penetrating " = "Penetrating",
                               "other cohort" = "Other cohort",
      
                               .default = cohort))



# Create the table with table_dataset

colnames(table_dataset)[which(names(table_dataset) == "severe_head_injury")] <- "Head_injury"
colnames(table_dataset)[which(names(table_dataset) == "pt_age_yrs")] <- "Age"
colnames(table_dataset)[which(names(table_dataset) == "ed_gcs_sum")] <- "ed_gcs"
colnames(table_dataset)[which(names(table_dataset) == "cohort_short")] <- "Cohort"
colnames(table_dataset)[which(names(table_dataset) == "intub")] <- "Intubated"
colnames(table_dataset)[which(names(table_dataset) == "NISS")] <- "NISS"
colnames(table_dataset)[which(names(table_dataset) == "Gender")] <- "Gender"
colnames(table_dataset)[which(names(table_dataset) == "OFI_categories")] <- "OFI"


#Print table 
pt_demographics <- table1(~ Cohort + Age + Gender + Head_injury + ed_gcs + Intubated + NISS | OFI , data=table_dataset, caption="\\textbf{Demographics}", overall = FALSE)


#install.packages("kableExtra")
#library("kableExtra")
#kable(pt_demographics, format = "latex", booktabs = TRUE) %>% kable_styling(latex_options = c("striped", "hold_position"), font_size = 12)






