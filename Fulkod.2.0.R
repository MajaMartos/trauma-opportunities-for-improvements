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





create_cohorts <- function(dataset) {
  
  ######
  # BM #
  ######
  get_severe_injuries <- function(aiscodes) {
    aiscodes <- na.omit(aiscodes)
    severe_codes <- grep("\\.[3-6]$", aiscodes, value = TRUE)
    return(severe_codes)
  }
  
  count_unique_regions <- function(ais_codes) {
    regions <- substr(ais_codes, 1, 1)
    regions <- regions[!is.na(regions) & regions %in% 1:8]
    num_regions <- length(unique(regions))
    return(num_regions)
  }
  
  count_severe_regions <- function(dataset) {
    # Extract relevant AIS codes using grep()
    codes <- unlist(as.vector(dataset[grep("^AIS", names(dataset))]))
    # Extract severe injuries using get_severe_injuries()
    severe_injuries <- get_severe_injuries(codes)
    # Count unique regions using count_unique_regions()
    num_regions <- count_unique_regions(severe_injuries)
    return(num_regions)
  }
  
  # Apply function to every row of dataset and save output in new column
  dataset$num_severe_regions <- apply(dataset,  1, count_severe_regions)
  
  dataset$BM <- (dataset$inj_dominant == 1) & (dataset$num_severe_regions >= 2)
  
  ##############
  # Severe TBI #
  ##############
  
  check_TBI_region <- function(ais_codes) {
    head_region <- any(substr(ais_codes, 1, 1) %in% c("1"))
    return(head_region)
  }
  
  # Check if severe head
  severe_head_injury <- function(dataset) {
    # Extract relevant AIS codes using grep()
    codes <- unlist(as.vector(dataset[grep("^AIS", names(dataset))])) 
    # Extract severe injuries using get_severe_injuries()
    severe_injuries <- get_severe_injuries(codes)
    # check i severe region is in the head
    severe_head_region <- check_TBI_region(severe_injuries)
    return(severe_head_region)
  }
  
  # apply to each row
  dataset$severe_head_injury <- apply(dataset,  1, severe_head_injury)
  
  # A severe TBI needs a GCS of < 9 , create a column that is TRUE if the ed GCS is less then 9 OR
  # if the patient is intubated prehospitaly:  check the pre hosp GCS instead
  
  dataset$low_GCS <- with(dataset, 
                          (ed_gcs_sum <= 8 | (is.na(ed_gcs_sum) & intub == 3 & pre_gcs_sum <= 8)))
  
  # If a low GCS and Severe head injury is present then its a severe TBI
  dataset$TBI <- (dataset$severe_head_injury == TRUE) & (dataset$low_GCS == TRUE) 
  
  ###############
  # Penetrating #
  ###############
  # Check if central area (3-5: neck, thorax, abdomen)
  check_pen_regions <- function(ais_codes) {
    has_region_345 <- any(substr(ais_codes, 1, 1) %in% c("3", "4", "5"))
    return(has_region_345)
  }
  
  # Check if severe in central area
  pt_regions <- function(dataset) {
    # Extract relevant AIS codes using grep()
    codes <- unlist(as.vector(dataset[grep("^AIS", names(dataset))]))
    # Extract severe injuries using get_severe_injuries()
    severe_injuries <- get_severe_injuries(codes)
    # Count unique regions using count_unique_regions()
    pt_region <- check_pen_regions(severe_injuries)
    return(pt_region)
  }
  
  # apply to each row
  dataset$pt_regions <- apply(dataset,  1, pt_regions)
  
  dataset$Severe_penetrating <- (dataset$inj_dominant == 2) & (dataset$pt_regions == TRUE)
  
  ##########################
  # Combine in new collumn #
  ##########################
  
  dataset$cohort <- NA
  
  dataset$cohort[dataset$Severe_penetrating == TRUE & dataset$inj_dominant == 2] <- "severe penetrating"
  dataset$cohort[dataset$TBI == TRUE & dataset$BM == TRUE & dataset$inj_dominant == 1] <- "blunt multisystem with TBI"
  dataset$cohort[dataset$TBI == FALSE & dataset$BM == TRUE & dataset$inj_dominant == 1] <- "blunt multisystem without TBI"
  dataset$cohort[dataset$TBI == TRUE & dataset$num_severe_regions < 2] <- "Isolated severe TBI"
  
  return(dataset)
}
new.dataset <- create_cohorts(combined.dataset)

#Creating column where possibly preventable death anf preventable deaths are merged 
cohorts$preventable_death <- ifelse(cohorts$Fr1.14 == 2 | cohorts$Fr1.14 == 3, "preventable", "non-preventable")


new.dataset$OFI_categories <- ifelse(new.dataset$Problemomrade_.FMP %in% c("Handl??ggning", "Handl??ggning/logistik", 
                                                                           "kompetensbrist", "Teriti??r survey", 
                                                                           "bristande rutin", "Neurokirurg"), 
                                     "Medical procedures", 
                                     ifelse(new.dataset$Problemomrade_.FMP %in% c("Logistik/teknik", "Traumakriterier/styrning", 
                                                                                  "Dokumentation", "kommunikation"), 
                                            "Logistics", 
                                            ifelse(new.dataset$Problemomrade_.FMP %in% c("l??ng tid till DT", "l??ng tid till op", 
                                                                                         "Triage p?? akm"), 
                                                   "Delay to procedure", 
                                                   ifelse(new.dataset$Problemomrade_.FMP %in% c("Resurs", "v??rdniv??"), 
                                                          "Lacking resources",
                                                          ""))))
