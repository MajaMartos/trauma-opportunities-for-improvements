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
  
# A function that combines the function for specific region of penetrating trauma and the function for serious injury and then sums the the patients for which both holds true  
  serious_neck_chest_abdomen_injury <- function(code) {
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    sum(neck_chest_abdomen_region(code[serious_injury]) == TRUE )
  }
 
# Removing all data from combined_dataset1 that lack information regarding dominating injury and storig it in a new dataset, penetrating_injury
  penetrating_injury <-Combined_dataset1[!is.na(Combined_dataset1$inj_dominant), ] 
  


# Creating a column with NA in the penetrating_injury dataset 
  penetrating_injury$penetrating <- NA
  
  
# Returns output from the "serious_neck_chest_abdomen_injury" function to the known_dominant_injury_pen dataset for all columns containing "AIScode" AND patients who have penetrating trauma as their dominant injury. A Column that holds TRUE if the patient has a severe penetrating injury is created 
# penetrating <- TRUE/FALSE 
  for (i in 1:nrow(penetrating_injury)) {
    v = 0+i
    if(serious_neck_chest_abdomen_injury(penetrating_injury[v,grepl( "AISCode" , names(penetrating_injury))]) >= 1 && penetrating_injury[v, "inj_dominant"] == 2) {
      penetrating_injury[v,"penetrating"] <- TRUE
    } else {
      penetrating_injury[v,"penetrating"] <- FALSE
    }
  }
    
# Create penetrating cohort 
  penetrating_cohort <- filter(penetrating_injury, penetrating == "TRUE")
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
#1. removes row if na in both on ed_gcs_sum and pre_intub. 2. create a dataset without rows with missing data in both columns 
  gcs_and_intub_is_na <- with(Combined_dataset1, is.na(ed_gcs_sum) & is.na(pre_intub_type))    
  Isolated_TBI <- Combined_dataset1[!gcs_and_intub_is_na,]  
  
 # Create an empty column (NA) in isolated_TBI dataset
  Isolated_TBI&gcs_below_9<- NA
  
#  Creates a new variable gcs_below_9, which is true if  the patient had an ed_gcs_sum <9 or if the patient was intubated prehosp. Converts NA to false
  Isolated_TBI$gcs_below_9 <- with(Isolated_TBI, ifelse(ed_gcs_sum<=8 | pre_intub_type==1, TRUE, FALSE))
  Isolated_TBI$gcs_below_9 <- with(Isolated_TBI, ifelse(is.na(gcs_below_9) | isFALSE(gcs_below_9), FALSE, TRUE))
  

    Isolated_TBI$severe_tbi <- NA
  for (i in 1:nrow(Isolated_TBI)) {
    v = 0+i
    if (has_a_serious_head_injury(Isolated_TBI[v, grepl("AISCode", names( Isolated_TBI))]) == TRUE && only_one_serious_injury_region(Isolated_TBI[v, grepl("AISCode", names(Isolated_TBI))]) == TRUE &&   Isolated_TBI[v, "gcs_below_9"] == TRUE) {
      Isolated_TBI [v,"severe_tbi"] <- TRUE
    } else {
      Isolated_TBI[v,"severe_tbi"] <- FALSE
    }
  }
  
# create severe TBI cohort 
    severe_tbi_cohort$Cohort <- "Severe TBI"
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  
  
# Blunt multisystem trauma with and without brain injury
  
  # Function selecting rows with more than one serious injury
  has_more_than_one_serious_injury <- function(code) {
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    number_of_regions(code[serious_injury]) >= 2
    
  }
  
# Removes all rows where data for inj_dominant is missing in dataet known_dominant_injury
  known_dominant_injury <-Combined_dataset1[!is.na(Combined_dataset1$inj_dominant), ] 
  
# create an empty column with NA in known_dominant injury 
  known_dominant_injury$blunt_multisystem <- NA
  
  
# Fills the column with Yes for patients who have more than one serious injury based on AIScod and false otherwise 
  
  for (i in 1:nrow(known_dominant_injury)) {
    v = 0+i
    if (has_more_than_one_serious_injury(known_dominant_injury[v,grepl( "AISCode" , names(known_dominant_injury))]) == TRUE && known_dominant_injury[v,"inj_dominant"] == 1) {
      known_dominant_injury[v,"blunt_multisystem"] <- TRUE
    } else {
      known_dominant_injury[v,"blunt_multisystem"] <- FALSE
    }
  }
  
  
  # We now want to create a columns which holds true if patients with multisystem trauma have a brain injury and false if not
  # C
  
  blunt_multisystem_cohort <- filter(known_dominant_injury, blunt_multisystem == "TRUE")
  
# create empty colum, serious_TBI  
 blunt_multisystem_cohort$Serious_TBI <- NA
  

 # HÄR ÄR NÅGOT INTE BRA FÅR 50 VARNINGAR :)))) 
  for (i in 1:nrow(known_dominant_injury)) {
    v = 0+i
    if (has_a_serious_head_injury(blunt_multisystem_cohort[v,grepl( "AISCode" , names(blunt_multisystem_cohort))]) == TRUE && blunt_multisystem_cohort[v,"inj_dominant"] == 1) {
      blunt_multisystem_cohort[v,"Serious_TBI"] <- TRUE
    } else {
      blunt_multisystem_cohort[v,"Serious_TBI"] <- FALSE
    }
  }
  
 
  
# Cohort for blunt multisystem trauma with brain injury 
 Blunt_multi_with_brain <-filter(blunt_multisystem_cohort, Serious_TBI == "TRUE")
 
 # Cohort for blunt multisystem trauma with brain injury 
 Blunt_multi_without_brain <-filter(blunt_multisystem_cohort, Serious_TBI == "FALSE") 
  



  