library(rofi)
data <- rofi::import_data(test = TRUE)
names <- c("swetrau","fmp","atgarder","problem","kvalgranskning2014.2017")
names(data) <- names
combined.dataset <- rofi::merge_data(data)
combined.dataset$ofi <- rofi::create_ofi(combined.dataset)
combined.dataset <- clean_all_predictors(combined.dataset)
mean.age<- round(mean(combined.dataset[,"pt_age_yrs"]), digits = 2)


