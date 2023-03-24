#Merge data 
library(rofi)
data <- rofi::import_data(test = TRUE)
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


## Separate and store cases without unknown outcome (OFI)
missing.outcome <- is.na(combined.dataset$ofi)
combined.dataset <- combined.dataset[!missing.outcome,]

## remove patients < 15 years
combined.dataset <- combined.dataset[combined.dataset$pt_age_yrs > 14,]

## JA: Tillse att du laddar in funktioner innan du använder dem, så source() Bör vara tidigt. Mitt förslag är att lägga alla högst upp tillsammans med när du laddar paket. 

## Fix formating and remove wrong values like 999
source("clean_all_predictors.R")
combined.dataset <- clean_all_predictors(combined.dataset)


## clean Audit filters
combined.dataset <- clean_audit_filters(combined.dataset)

# Clean data 
source("clean_all_predictors.R")

source("clean_audit_filters.R")
## clean Audit filters
combined.dataset <- clean_audit_filters(combined.dataset)

# Clean data 
source("clean_all_predictors.R")



### Skippa denna funktion och kör istället bara source("create_cohorts.R") Är något galet med den och tar onödig plats.

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
  datset$num_severe_regions <- apply(dataset,  1, count_severe_regions)
  
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

source("create_cohorts.R")
new.dataset <- create_cohorts(combined.dataset)

## JA: Lägger till "other cohort" för att inte behöva exkludera. Får se om vi behåller.
new.dataset$cohort <- as.character(new.dataset$cohort)
new.dataset[is.na(new.dataset$cohort), "cohort"] <- "other cohort"

#table(new.dataset$cohort)
#Creating column where possibly preventable death and preventable deaths are merged 

# JA: Skulle kalla alla "possible preventable" istället för preventable.
# de få fall som är riktigt preventable är känsliga ärenden och vi bör verkligen förtydliga att vi inte har så många riktiga preventable
# Har även lagt till "survival" om patienterna överlevt, för annars har de NA i preventable 

# OBS: Det är något galet med summan "non-preventable" så jag måste titta igenom igen.
new.dataset$preventable_death <- ifelse(new.dataset$Fr1.14 == 2 | new.dataset$Fr1.14 == 3, "preventable", "non-preventable")

new.dataset$preventable_death <- ifelse(is.na(new.dataset$preventable_death) == TRUE & new.dataset$res_survival == 2, "survived", new.dataset$preventable_death)

table(new.dataset$preventable_death)

#Creating coulmn for 30-day survival
new.dataset$month_surv <- ifelse(new.dataset$res_survival == 2,  "alive", "dead")



## JA: Fixade preventable genom att stava "preventable" istället för "Preventable". Det var också 10 andra patienter som hade varianter av nedan som stavades annorlunda. Nu är det korrekt ;) 

### Detta verkar fungera bra, men obs: I riktiga analysen så ska preventable death in som ett fel i kolumnen OFI_categories och inte vara fristående. 
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
                                                                 
                                                                 ifelse(new.dataset$preventable_death %in% c("preventable"), 
                                                                        "Preventability",
              
                                                          ifelse(new.dataset$ofi %in% c("No"), 
                                                                 "No ofi",
                                                          "random")))))))

table(new.dataset$OFI_categories)



################
#Create table1##
################
#install.packages("descr")
library(descr)

library(labelled)

# Get the subset of your combined dataset that includes only the columns needed for the table

## JA: I slutgiltiga varianten bör vi justera detta, slå ihop ev ta bort något. Men fungerar nu.
## Har dock tagit bort pt_region (vilket bara sammanfattade antal alv skador centralt)
table_cols <- c("OFI_categories", "pt_age_yrs", "Gender", "severe_head_injury", "low_GCS", 
                "ed_gcs_sum", "intub", "pre_gcs_sum", "inj_dominant", "Severe_penetrating", "cohort", "OFI_categories", "preventable_death", "month_surv")
table_dataset <- new.dataset[, table_cols]

# Remove rows with missing values only for the columns included in the table

## JA: Innan du tar complete.cases() så måste man ha fyllt vissa NA med någon variabel. EXV så har alla som inte dör NA i preventable death columnen, de exkluderas nu.
## Förslag är att skippa detta nu så blir det tydligare med riktiga datan.
# table_dataset <- table_dataset[complete.cases(table_dataset),] 
table_dataset <- new.dataset

# JA: Detta genererar en relativt fin tabell. Fundera på rubriken.

# Create the table with the cleaned dataset
pt_demographics <- table1(~ cohort + pt_age_yrs + Gender + severe_head_injury + low_GCS + ed_gcs_sum + intub +  pre_gcs_sum + pt_regions + inj_dominant + Severe_penetrating + preventable_death + month_surv | OFI_categories , data=table_dataset, caption="\\textbf{Demographics}", overall = FALSE)

view(new.dataset[new.dataset$OFI_categories == "random",])


###########################
# create regression model #
###########################
# Convert categorical variables to factors
########
## JA, säkerställ att prev death ligger här inne och att alla övriga NA == other.
new.dataset$OFI_categories <- factor(new.dataset$OFI_categories)
## JA: Lägg till en Other column för de som är NA i cohorts.
new.dataset$cohort <- factor(new.dataset$cohort)

new.dataset$Gender <- factor(new.dataset$Gender)
##JA: Notera att du nu kör prev death som en oberoende variable men det ska vara en del av OFI_categories och alltså en del av din beroende variabel. 

new.dataset$preventable_death <- factor(new.dataset$preventable_death)
##JA: Om patienten lever så ska prev death vara 0.
new.dataset$month_surv <- factor(new.dataset$month_surv)

## JA: När du fyllt i ovan faktorer med värden istället för NA så kan du köra complete case bara på age, gender, low_GCS (inte övriga GCS).
 

# Re-code the OFI_categories variable so that "Exemplary treatment" is the new reference category
new.dataset$OFI_categories <- relevel(new.dataset$OFI_categories, ref = "No ofi")

# Creating the unadjusted logistic regression model for cohorts 
my_log_unad <- multinom (OFI_categories ~ cohort, data = new.dataset)

################################################
## Räkna ut p-värden och OR för icke justerade #
################################################

# Z värden
z_wout <- summary(my_log_unad)$coefficients/summary(my_log_unad)$standard.errors
z_wout

# p värden
p_values <- as.data.frame((1 - pnorm(abs(z_wout), 0, 1)) * 2)
p_values
colnames(p_values) <- paste(colnames(p_values), "_p_value", sep = "")

# funktion för att sätta bokstäver istället för siffror så det blir tydligare. Snodd från nätet.
sign_levels_df_letter <- function(df) {
  df <- ifelse(df >.80, "Z", ifelse(df >.50, "FFF",
                                    ifelse( df >.30, "FF",
                                            ifelse(df >.10 , "F", 
                                                   ifelse(df <= 0.0001, "AAA",
                                                          ifelse(df <= .0005,"AA+",
                                                                 ifelse(df <= .001,"AA",
                                                                        ifelse(df <= .005, "A+",
                                                                               ifelse (df<= .01, "A",
                                                                                       ifelse(df<= .05, "A-",
                                                                                              ifelse(df <=.07, "B",
                                                                                                     ifelse(df <=.10, "C", NA
                                                                                                            
                                                                                                     ))))))))))))
  
  
  return(df)
}


p_values_text = sign_levels_df_letter(p_values)



#install.packages("dplyr")
library(dplyr)



# Extract the coefficients
coef_values <- coef(my_log_unad)

# Exponentiate the coefficients to get the odds ratios
or <- as.data.frame(exp(coef_values))

# Print the odds ratios
## Combine OR and p-value
table <- cbind(or, p_values)

## Byt ordning så att OR följs av P-värde.

table <- table %>% select(
  "(Intercept)",
  "(Intercept)_p_value",
  "cohortblunt multisystem without TBI",
  "cohortblunt multisystem without TBI_p_value",
  "cohortIsolated severe TBI",
  "cohortIsolated severe TBI_p_value",
  "cohortother cohort",
  "cohortother cohort_p_value",
  "cohortsevere penetrating",
  "cohortsevere penetrating_p_value"
) # add this parenthesis

view(table)


#####

# Logistic regression,  preventable death 
my_log_preventable <- multinom (OFI_categories ~ preventable_death, data = new.dataset)

# Logistic regression, age 
my_log_age <- multinom (OFI_categories ~ pt_age_yrs, data = new.dataset)

#Logistic regression, Gender 
my_log_gender <- multinom (OFI_categories ~ Gender, data = new.dataset)

#Logistic regression, month_surv
my_log_surv <- multinom (OFI_categories ~ month_surv, data = new.dataset)


# Logistic regression 
my_log_prevent_res_surv <- multinom (preventable_death ~ month_surv, data = new.dataset)

  
# Create ajdusted multinomial logistic regression model
my_log_adj <- multinom( OFI_categories ~ cohort + pt_age_yrs + Gender + month_surv + preventable_death, data = new.dataset)





####################
# View the results##
####################

# Summary od adjusted model 
model <- summary(my_log_adj)

# Load the stargazer package for table creation
library(stargazer)

coef_model <- coef(model)

# Compute odds ratios 
odds_ratios <- exp(coef_model)

## and p-values
z_model <- model$coefficients/model$standard.errors
  
  # 2-tailed z test
  p_values <- (1 - pnorm(abs(z_model), 0, 1)) * 2


# Combine odds ratios and p-values into a data frame
results_df <- data.frame(odds_ratios, p_values)



#################
## Other tests ##
#################

# Load the dplyr package for data manipulation
library(dplyr)

# Create a summary table of preventable vs. non-preventable deaths
table_preventable<- new.dataset %>%
  group_by(preventable_death) %>%
  summarize( "Deaths within 30 days"= sum(month_surv == "dead", na.rm = TRUE))

# Print the summary table
print(table_preventable)


# Perform a chi-square test of preventable death vs. res_surv
chisq.test(chisq.test())

new.dataset %>% select(OFI_categories, cohort, pt_age_yrs, Gender, preventable_death) %>% skim()

library(vcd)

# create a contingency table between cohort and OFI_categories
cont_table <- table(new.dataset$cohort, new.dataset$OFI_categories)

# calculate the association statistics
assocstats(cont_table)

