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


# Sorterar bort alla som inte har NISS över 15 eller ISS över 
Combined_dataset1<- combined.dataset[(combined.dataset$"NISS" >= 15 | combined.dataset$"ISS" >= 9), ]

#Gör om alla 999 till NA med apply 


# Skapa kohorter

#Blunt multisystem trauma: 1.	Blunt multisystem trauma: Blunt trauma with injuries of Abbreviated Injury Score (AIS) ≥ 3 in at least two of the following AIS body regions: head, face, neck, thorax, abdomen, spine, or upper and lower extremities
#AIS >3, mer än 2 regioner 
Blunt_multisystem <- subset(Combined_dataset_inclusion, AIS > 30 & sex == "male")

df$age_gt_30_label <- ifelse(df$age_gt_30, "yes", "no")


#Penetrating trauma: At least one AIS ≥ 3 injury in any of the following AIS body regions: neck, thorax, and abdomen.

is_serious <- function(code) {
  severity <- substr(as.character(code), 8, 8)
  as.numeric(severity) >= 3  
}
#if severity level is 3 or higher, the injury is considered serious
number_of_regions <- function(code) {
  region <- substr(as.character(code), 1, 1)
  length(unique(region))
}

neck_chest_abdomen_region <- function(code) {
  code <- code[!is.na(code)]
  region <- substr(as.character(code), 1, 1)  
  is.element(region,3:5)
 
  }
  
#generates TRUE/FALSE
  serious_neck_chest_abdomen_injury <- function(code) {
    code <- code[!is.na(code)]
    serious_injury <- is_serious(code)
    sum(neck_chest_abdomen_region(code[serious_injury]) == TRUE )
  
  }
 
#generates numerical value
  known_dominant_injury_pen <-Combined_dataset1[!is.na(Combined_dataset1$inj_dominant), ] 
  known_dominant_injury_pen$penetrating <- NA
  
#removing na in dominant injury, new df
  for (i in 1:nrow(known_dominant_injury_pen)) {
    v = 0+i
    if(serious_neck_chest_abdomen_injury(known_dominant_injury_pen[v,grepl( "AISCode" , names(known_dominant_injury_pen))]) >= 1 && known_dominant_injury_pen[v, "inj_dominant"] == 2) {
      known_dominant_injury_pen[v,"penetrating"] <- TRUE
    } else {
      known_dominant_injury_pen[v,"penetrating"] <- FALSE
    }
  }
 
#penetrating <- TRUE/FALSE
  
  
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
  

