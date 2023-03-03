#
# A function to assign cohorts:
#   Blunt multisystem without TBI
#   Blunt multistystem with TBI
#   Isolated severe TBI
#   Severe penetrating 
#
# The input shold be "combined.dataset" as created by the rofi packages.
# Need to gave gone through the clean_all_predictors() and clean_audit_filters() function.
#
# How to use:
#   source(create_cohorts.R") # Source this file
#   new.dataset <- create_cohorts(combined.dataset) #apply function to combined.dataset and save in a new.dataset
#

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
  has_region_345 <- any(substr(ais_codes, 1, 1) %in% c("1"))
  return(has_region_345)
}

# Check if severe in central area
severe_head_injury <- function(dataset) {
  # Extract relevant AIS codes using grep()
  codes <- unlist(as.vector(dataset[grep("^AIS", names(dataset))]))
  # Extract severe injuries using get_severe_injuries()
  severe_injuries <- get_severe_injuries(codes)
  # Count unique regions using count_unique_regions()
  pt_region <- check_TBI_region(severe_injuries)
  return(pt_region)
}

# apply to each row
dataset$severe_head_injury <- apply(dataset,  1, severe_head_injury)


dataset$low_GCS <- with(dataset,
                                 (ed_gcs_sum <= 8 | (is.na(ed_gcs_sum) & intub == 3 & pre_gcs_sum <= 8)))

dataset$TBI <- (dataset$severe_head_injury == TRUE) & (dataset$low_GCS == TRUE) # & (dataset$inj_dominant == 2)

###############
# Penetrating #
###############
# Check if central area (3-5)
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
