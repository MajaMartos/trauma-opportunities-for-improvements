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
#table(new.dataset$OFI_categories)


################
#Create table1##
################


# Get the subset of your combined dataset that includes only the columns needed for the table
table_cols <- c("OFI_categories", "pt_age_yrs", "Gender", "severe_head_injury", "low_GCS", 
                "ed_gcs_sum", "intub", "pre_gcs_sum", "inj_dominant", "Severe_penetrating", "cohort", "OFI_categories", "preventable_death", "month_surv")
table_dataset <- new.dataset[, table_cols]

# Remove rows with missing values only for the columns included in the table
## JA: Innan du tar complete.cases() så måste man ha fyllt vissa NA med någon variabel. EXV så har alla som inte dör NA i preventable death columnen, de exkluderas nu.
## Förslag är att skippa detta nu så blir det tydligare med riktiga datan.
#table_dataset <- table_dataset[complete.cases(table_dataset),] 

table_dataset <- new.dataset

# JA: Detta genererar en relativt fin tabell. Fundera på rubriken.

# Create the table with the cleaned dataset
pt_demographics <- table1(~ cohort + pt_age_yrs + Gender + severe_head_injury + low_GCS + ed_gcs_sum + intub +  pre_gcs_sum + pt_regions + inj_dominant + Severe_penetrating + preventable_death + month_surv | OFI_categories , data=table_dataset, caption="\\textbf{Demographics}", overall = FALSE)


###################################
#Create Table 2 -  OFI categories #
###################################

# Create a new data frame with the categories and the count of patients
ofi_summary <- new.dataset %>% 
  group_by(OFI_categories, Problemomrade_.FMP) %>% 
  summarize(count = n()) %>% 
pivot_wider(names_from = OFI_categories, values_from = Problemomrade_.FMP, values_fill = 0) %>%
  rename(`Category of OFI` = OFI_categories, `Ofi` = Problemomrade_.FMP)

<<<<<<< HEAD
# Group the data by OFI category and OFI name, and count the occurrences
#ofi_summary <- new.dataset %>%
#  group_by(OFI_categories, ofi) %>%
#  summarise(count = sum(!is.na(ofi))) %>%
#  ungroup()

# Pivot the data to create a wide format summary table
#ofi_table <- ofi_summary %>%
#  pivot_wider(names_from = ofi, values_from = count, values_fill = 0)

# Rename the columns
#colnames(ofi_table)[1] <- "OFI categories"

# Print the summary table
#knitr::kable(ofi_table)


#########################################
#Create Table 2 -  exclusion/ inclusion #
#########################################

exclusion_criteria <- (new.dataset$pt_age_yrs <= 15 & (new.dataset$NISS <= 15 | new.dataset$ISS <= 9))
inclusion_criteria <- new.dataset$cohort %in% c("blunt multisystem with TBI", "blunt multisystem without TBI", "severe penetrating", "Isolated severe TBI")

selected_patients <- new.dataset[!exclusion_criteria & inclusion_criteria,]


# Create a table showing how many patients are lost at each step
n_all_patients <- nrow(new.dataset)
n_excluded_patients <- sum(exclusion_criteria)
n_included_patients <- sum(inclusion_criteria)
n_final_patients <- nrow(selected_patients)

loss_table <- table(
  c("All patients", "Exclusion criteria applied", "Inclusion criteria applied", "Final cohort selected"),
  c(n_all_patients, n_excluded_patients, n_included_patients, n_final_patients)
)

# Print loss table
#print(loss_table)


###################################
#Create Table 3 -  OFI categories #
###################################


# Group the data by OFI category and OFI name, and count the occurrences
ofi_summary <- new.dataset %>%
  group_by(OFI_categories, ofi) %>%
  summarise(count = sum(!is.na(ofi))) %>%
  ungroup()

# Pivot the data to create a wide format summary table
ofi_table <- ofi_summary %>%
  pivot_wider(names_from = ofi, values_from = count, values_fill = 0)

# Rename the columns
colnames(ofi_table)[1] <- "OFI categories"


=======

# Print the table
ofi_summary
>>>>>>> cf04008c1873920b0abc76652b4f2df84ca07681



###########################
# create regression model #
###########################

# Convert categorical variables to factors
# MM: har gjort om alla NA  till other för preventable death, ofis, low_GCS 

new.dataset$OFI_categories <- factor(new.dataset$OFI_categories)
new.dataset$cohort <- factor(new.dataset$cohort)
new.dataset$Gender <- factor(new.dataset$Gender)
new.dataset$month_surv <- factor(new.dataset$month_surv)
new.dataset$low_GCS <- factor(new.dataset$low_GCS)

## JA: När du fyllt i ovan faktorer med värden istället för NA så kan du köra complete case bara på age, gender, low_GCS (inte övriga GCS).
# MM Har fyltlt i NA med others, men då finns inget syfte med complete cases? Har missförstått något 
 
# Re-code the OFI_categories variable so that "Exemplary treatment" is the new reference category
new.dataset$OFI_categories <- relevel(new.dataset$OFI_categories, ref = "No ofi")

# Creating the unadjusted logistic regression model for cohorts 
my_log_unad <- multinom (OFI_categories ~ cohort, data = new.dataset)

################################################
## Räkna ut p-värden och OR för icke justerade #
################################################

# Z värden
z_wout <- summary(my_log_unad)$coefficients/summary(my_log_unad)$standard.errors
#z_wout

# p värden
p_values <- as.data.frame((1 - pnorm(abs(z_wout), 0, 1)) * 2)
#p_values
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

#view(table)


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











