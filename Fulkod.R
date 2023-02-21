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
  
  for (i in 1:nrow(penetrating_injury)) {
    v = 0+i
    if(serious_neck_chest_abdomen_injury(penetrating_injury[v,grepl( "AISCode" , names(penetrating_injury))]) >= 1 && penetrating_injury[v, "inj_dominant"] == 2) {
      penetrating_injury[v,"penetrating"] <- TRUE
    } else {
      penetrating_injury[v,"penetrating"] <- FALSE
    }
  }
    
#penetrating <- TRUE/FALSE
  
# Counts the number of injured regions by assesing the number of unique AIS code for each row (patient)
  number_of_regions <- function(code) {
  region <- substr(as.character(code), 1, 1)
  length(unique(region))
}
  
  
  #SEVERE TBI
  head_region <- function(code) {
    region <- substr(as.character(code), 1, 1)
    is.element(region, 1)
  }
  #returns true if body region == 1
  has_a_serious_head_injury <- function(code) {
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    head_region(code[serious_injury]) 
  }
  #acts on vectors in "serious_injury", chooses those which have region == 1
  only_one_region <- function(code){
    number_of_regions(code) == 1
  }
  #generates TRUE/FALSE
  only_one_serious_injury_region <- function(code) {
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    only_one_region(code[serious_injury])
  }
  #generates TRUE/FALSE
  gcs_and_intub_is_na <- with(Combined_dataset1, is.na(ed_gcs_sum) & is.na(pre_intub_type))    
  known_gcs_or_intub_type <- Combined_dataset1[!gcs_and_intub_is_na,]  
  
  #remove row if na in both prehospital intub type and gcs
  
  known_gcs_or_intub_type$gcs_below_9 <- NA
  known_gcs_or_intub_type$gcs_below_9 <- with(known_gcs_or_intub_type, ifelse(ed_gcs_sum<=8 | pre_intub_type==1, TRUE, FALSE))
  known_gcs_or_intub_type$gcs_below_9 <- with(known_gcs_or_intub_type, ifelse(is.na(gcs_below_9) | isFALSE(gcs_below_9), FALSE, TRUE))
  
  #makes new variable containing those with a GCS <9 and those intubated in a prehospital setting. Converts na to false
  known_gcs_or_intub_type$severe_tbi <- NA
  for (i in 1:nrow(known_gcs_or_intub_type)) {
    v = 0+i
    if (has_a_serious_head_injury(known_gcs_or_intub_type[v, grepl("AISCode", names(known_gcs_or_intub_type))]) == TRUE && only_one_serious_injury_region(known_gcs_or_intub_type[v, grepl("AISCode", names(known_gcs_or_intub_type))]) == TRUE && known_gcs_or_intub_type[v, "gcs_below_9"] == TRUE) {
      known_gcs_or_intub_type[v,"severe_tbi"] <- TRUE
    } else {
      known_gcs_or_intub_type[v,"severe_tbi"] <- FALSE
    }
  }
  
  
  
  # Blunt multisystem trauma without brain injury
  
  is_serious <- function(code) {
    severity <- substr(as.character(code), 8, 8)
    as.numeric(severity) >= 3  
  }
  
  number_of_regions <- function(code) {
    region <- substr(as.character(code), 1, 1)
    length(unique(region))
  }
  #identifying number of regions
  has_more_than_one_serious_injury <- function(code) {
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    number_of_regions(code[serious_injury]) >= 2
  }
  ## combining them
  blunt.multisystem <- apply( has_more_than_one_serious_injury, 1, function(row) {
    code <- row[grep("AISCode", names(row))]
    row
  })
  fewer_variables$blunt_multisystem <- NA
  
  #creating column blunt_multisystem
  known_dominant_injury <-fewer_variables[!is.na(fewer_variables$inj_dominant), ] 
  
  
  
  # Define function to check if a row contains blunt multisystem trauma
  has_blunt_multisystem <- function(row) {
    code <- row[grep("AISCode", names(row))] 
    has_more_than_one_serious_injury(code) && row["inj_dominant"] == "1"
  }
  
  # Define function to check if a row contains brain injury
  has_brain_injury <- function(code) {
    region <- substr(as.character(code), 1, 1)
    is.element(region, 1)
  }
  
  # Create logical vectors to identify rows with blunt multisystem trauma and without brain injury
  blunt_multisystem <- apply(known_dominant_injury, 1, has_blunt_multisystem)
  no_brain_injury <- apply(known_dominant_injury, 1, function(row) {
    code <- row[grep("AISCode", names(row))]
    !has_brain_injury(code)
  })
  
  # Combine the two logical vectors using the & (and) operator
  blunt_multisystem_no_brain <- known_dominant_injury[blunt_multisystem & no_brain_injury, ]
=======
>>>>>>> fea5ed9768f012228561f96b2649867d87ebed77

  