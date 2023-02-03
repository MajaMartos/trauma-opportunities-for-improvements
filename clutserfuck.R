# Help from Jonatan
library(rofi)
data <- rofi::import_data(test = TRUE)
names <- c("swetrau","fmp","atgarder","problem","kvalgranskning2014.2017")
names(data) <- names
combined.dataset <- rofi::merge_data(datagli)
combined.dataset$ofi <- rofi::create_ofi(combined.dataset)

#new data 
rofi::clean_all_predictors(combined.dataset)

#packages 
library(dplyr)
library(ggplot2)
library(skimr)
library(tidyverse)
library(boot)
library(table1)

#Looking at raw data  
#ta bort under 15 år, NA, saknar cohorter 

glimpse(combined.dataset)
new.data <- combined.dataset[,c("Gender", "pt_age_yrs", "ofi", "Problemomrade_.FMP", "DOB", "ISS_moore_15_trauma_3","Tr_Nivå", "NumberOfInjuries", "origin", "tra_DodsfallsanalysGenomford", "NISS")]    

