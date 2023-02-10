#Merge data 
library(rofi)
data <- rofi::import_data(test = TRUE)
names <- c("swetrau","fmp","atgarder","problem","kvalgranskning2014.2017")
names(data) <- names
combined.dataset <- rofi::merge_data(datagli)
combined.dataset$ofi <- rofi::create_ofi(combined.dataset)

# Clean data 
source("clean_all_predictors.R")

#packages 
library(dplyr)
library(ggplot2)
library(skimr)
library(tidyverse)
library(boot)
library(table1)


#tar bort under 15 år
combined.dataset_older_than_14 <- combined.dataset[combined.dataset$"pt_age_yrs" >= 15, ]

# Sorterar bort alla som inte har NISS över 15 eller ISS över 9
Combined_dataset_inclusion <- combined.dataset_older_than_14[!(combined.dataset_older_than_14$"NISS" >= 15 | combined.dataset_older_than_14$"ISS" >= 9), ]

#Gör om alla 999 till NA med apply 
Combined_dataset_inclusion[Combined_dataset_inclusion == 999] <- NA


# Skapa kohorter

#Blunt multisystem trauma: 1.	Blunt multisystem trauma: Blunt trauma with injuries of Abbreviated Injury Score (AIS) ≥ 3 in at least two of the following AIS body regions: head, face, neck, thorax, abdomen, spine, or upper and lower extremities
#AIS >3, mer än 2 regioner 
Blunt_multisystem <- subset(Combined_dataset_inclusion, AIS > 30 & sex == "male")

df$age_gt_30_label <- ifelse(df$age_gt_30, "yes", "no")





