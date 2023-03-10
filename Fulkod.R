#Merge data 
library(rofi)
data <- rofi::import_data(test = TRUE)
names <- c("swetrau","fmp","atgarder","problem","kvalgranskning2014.2017")
names(data) <- names
combined.dataset <- rofi::merge_data(data)

## Create OFI column
combined.dataset$ofi <- rofi::create_ofi(combined.dataset)
combined.dataset <- clean_all_predictors(combined.dataset)

#packages 
library(dplyr)
library(ggplot2)
library(skimr)
library(tidyverse)
library(boot)
library(table1)


## Separate and store cases without unknown outcome (OFI)
missing.outcome <- is.na(combined.dataset$ofi)
combined.dataset <- combined.dataset[!missing.outcome,]

## remove patients < 15 years
combined.dataset <- combined.dataset[combined.dataset$pt_age_yrs > 14,]

## Fix formating and remove wrong values like 999
source("clean_all_predictors.R")
combined.dataset <- clean_all_predictors(combined.dataset)


## clean Audit filters
combined.dataset <- clean_audit_filters(combined.dataset)

# Clean data 
source("clean_all_predictors.R")


## clean Audit filters
combined.dataset <- clean_audit_filters(combined.dataset)

# Clean data 
source("clean_all_predictors.R")


# Sorterar bort alla som inte har NISS över 15 eller ISS över 
Combined_dataset1<- combined.dataset[(combined.dataset$"NISS" >= 15 | combined.dataset$"ISS" >= 9), ]


# Skapa kohorter
# AIS code, first number in row is region, last severity 

pat <- c("123457.8","223457.2","523457.7", "323457.8")


#Penetrating trauma: At least one AIS ≥ 3 injury in any of the following AIS body regions: neck, thorax, and abdomen.

## A function for severity, that holds true if the patient has an AIS severity score of >=3
is_serious <- function(code) {
  severity <- substr(as.character(code), 8, 8)
  as.numeric(severity) >= 3  
}


# A function specifying the relevant regions (neck, chest, abdomen) for penetrating trauma 
neck_chest_abdomen_region <- function(code) {
  code <- code[!is.na(code)]
  region <- substr(as.character(code), 1, 1)  
  is.element(region,3:5)
 
  }
  
# A function that combines the function for specific region of penetrating trauma and the function for serious injury 
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    sum(neck_chest_abdomen_region(code[serious_injury]) == TRUE )
  
 

# Creatig cohort dataset where all rows lacking information regarding dominating injury, intubation and GCS are removed ( necessary at this stage?)

  cohorts <-Combined_dataset1[!is.na(Combined_dataset1$inj_dominant), ] 
  
  gcs_and_intub_is_na <- with(cohorts, is.na(ed_gcs_sum) & is.na(pre_intub_type))    
  cohorts <- cohorts[!gcs_and_intub_is_na,] 
  

# Creating a column "penetrating" in cohorts 
  cohorts$penetrating <- NA
  
  
# fills "penetrating" using the serious_neck_chest_injury function   
# penetrating <- TRUE/FALSE 
  for (i in 1:nrow(cohorts)) {
    v = 0+i
    if(serious_neck_chest_abdomen_injury(cohorts[v,grepl( "AISCode" , names(cohorts))]) >= 1 && cohorts[v, "inj_dominant"] == 2) {
      cohorts[v,"penetrating"] <- TRUE
    } else {
      cohorts[v,"penetrating"] <- FALSE
    }
  }
    

  

  
# SEVERE TBI
# Function that includes rows where patients have an AIS code with first number = 1
  head_region <- function(code) {
    region <- substr(as.character(code), 1, 1)
    is.element(region, 1)
  }
# Function that combines serious injury score with head injury 
  has_a_serious_head_injury <- function(code) {
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    head_region(code[serious_injury]) 
  }
  
# Function that specifies the number of injured regions  
  number_of_regions <- function(code) {
    region <- substr(as.character(code), 1, 1)
    length(unique(region))
  }

# Uses the function above to select only those who are injured in a single region
  only_one_region <- function(code){
    number_of_regions(code) == 1
  }

# Combines function for only one injured region and serious injury
  only_one_serious_injury_region <- function(code) {
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    only_one_region(code[serious_injury])
  }

  
 # Creates column "gcs_below_9"
  cohorts&gcs_below_9<- NA
  
#  Creates a new variable gcs_below_9, which is true if  the patient had an ed_gcs_sum <9 or if the patient was intubated prehosp. Converts NA to false
  cohorts$gcs_below_9 <- with(cohorts, ifelse(ed_gcs_sum<=8 | pre_intub_type==1, TRUE, FALSE))
  cohorts$gcs_below_9 <- with(cohorts, ifelse(is.na(gcs_below_9) | isFALSE(gcs_below_9), FALSE, TRUE))
  
# creates column "isolated_severe_tbi" using the has_a_serious_head_injury function and only_one_serious_injury_region
cohorts$isolated_severe_tbi <- NA
  for (i in 1:nrow(cohorts)) {
    v = 0+i
    if (has_a_serious_head_injury(cohorts[v, grepl("AISCode", names( cohorts))]) == TRUE && only_one_serious_injury_region(cohorts[v, grepl("AISCode", names(cohorts))]) == TRUE &&   cohorts[v, "gcs_below_9"] == TRUE) {
      cohorts [v,"isolated_severe_tbi"] <- TRUE
    } else {
      cohorts[v,"isolated_severe_tbi"] <- FALSE
    }
  }
  

    
    
    
    
  
# Blunt multisystem trauma 
  
  # Function selecting rows with more than one serious injury
  has_more_than_one_serious_injury <- function(code) {
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    number_of_regions(code[serious_injury]) >= 2
    
  }

# create an empty column blunt_multisystem 
  cohorts$blunt_multisystem <- NA
  
  
# Fills the column with Yes for patients who have more than one serious injury based on AIScode and false otherwise 
  
  for (i in 1:nrow(cohorts)) {
    v = 0+i
    if (has_more_than_one_serious_injury(cohorts[v,grepl( "AISCode" , names(cohorts))]) == TRUE && cohorts[v,"inj_dominant"] == 1) {
      cohorts[v,"blunt_multisystem"] <- TRUE
    } else {
      cohorts[v,"blunt_multisystem"] <- FALSE
    }
  }
  

# create empty colum, severe_tbi (not isolated)  
cohorts$severe_tbi<- NA

for (i in 1:nrow(cohorts)) {
  v = 0+i
  if (has_a_serious_head_injury(cohorts[v,grepl( "AISCode" , names(cohorts))]) == TRUE && cohorts[v,"inj_dominant"] == 1) {
    cohorts[v,"severe_tbi"] <- TRUE
  } else {
    cohorts[v,"severe_tbi"] <- FALSE
  }
}
 

#Create column with cohort multisystem_with_tbi
cohorts$multisystem_with_tbi <- ifelse(cohorts$severe_tbi == TRUE & cohorts$blunt_multisystem == TRUE, TRUE, FALSE)


#Create colum with cohort multisystem_without_tbi
cohorts$multisystem_without_tbi <- ifelse(cohorts$severe_tbi == FALSE & cohorts$blunt_multisystem == TRUE, TRUE, 
                                          ifelse(cohorts$severe_tbi == FALSE & cohorts$blunt_multisystem == FALSE, FALSE, TRUE))


# creating common variable cohort
cohorts$cohort  <- ifelse(cohorts$penetrating == "TRUE", "penetrating",
                                           ifelse(cohorts$isolated_severe_tbi == "TRUE", "Isolated_TBI",
                                                  ifelse(cohorts$multisystem_with_tbi == "TRUE", "multisystem_with_tbi",
                                                         ifelse(cohorts$multisystem_without_tbi == "TRUE","multisystem_without_tbi", NA))))
                          
                         
                          
#Creating column where possibly preventable death anf preventable deaths are merged 
cohorts$preventable_death <- ifelse(cohorts$Fr1.14 == 2 | cohorts$Fr1.14 == 3, "preventable", "non-preventable")


cohorts$OFI_categories <- ifelse(cohorts$Problemomrade_.FMP %in% c("Handläggning", "Handläggning/logistik", 
                                                                   "kompetensbrist", "Teritiär survey", 
                                                                   "bristande rutin", "Neurokirurg"), 
                                 "Medical procedures", "",
                                 
                                 ifelse(cohorts$Problemomrade_.FMP %in% c("Logistik/teknik", "Traumakriterier/styrning", 
                                                                          "Dokumentation", "kommunikation"), 
                                        "Logistics", ""),
                                 
                                 ifelse(cohorts$Problemomrade_.FMP %in% c("lång tid till DT", "lång tid till op", 
                                                                          "Triage på akm"), 
                                        "Delay to procedure", ""),
                                 
                                 ifelse(cohorts$Problemomrade_.FMP %in% c("Resurs", "vårdnivå"), 
                                        "Lacking resources",""))




pt_demographics <- table1(~ ofi + pt_age_yrs + Gender + ed_gcs_sum  + ed_sbp_value + NISS + res_survival | cohort, data=cohorts, caption="\\textbf{Cohort demographics}", overall = FALSE)



  