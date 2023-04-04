
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


# Re-code the OFI_categories variable so that "Exemplary treatment" is the new reference category
new.dataset$OFI_categories <- relevel(new.dataset$OFI_categories, ref = "No ofi")

# Creating the unadjusted logistic regression model for cohorts 
my_log_unad <- multinom (OFI_categories ~ cohort, data = new.dataset)

#install.packages("gtsummary")
library(gtsummary)
table.unadjust <-tbl_regression(my_log_unad, 
                                exponentiate = TRUE) %>% as_gt()


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

main.result <- table %>% select(
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

#view(main.result)


###################
# Justerad modell #
###################
#JA: snackade med martin, förslag för att minska lite:
# Två resultat, en helt ojusterad (ovan), och en helt justerad enl minimum:
# Förslag då att justera för ålder, kön och skadegrad (kan ev va NISS om du vill)
# Ytterligare justering kan vi göra i mån om tid/önskan men det blir stora tabeller osv.

# Create ajdusted multinomial logistic regression model
my_log_adj <- multinom( OFI_categories ~ cohort + pt_age_yrs + Gender + ISS, data = new.dataset)


#################################################
# Övriga modeller som vi gör om du får tid över #
#################################################
# Logistic regression,  preventable death 
#my_log_preventable <- multinom (OFI_categories ~ preventable_death, data = new.dataset)

# Logistic regression, age 
#my_log_age <- multinom (OFI_categories ~ pt_age_yrs, data = new.dataset)

#Logistic regression, Gender 
#my_log_gender <- multinom (OFI_categories ~ Gender, data = new.dataset)

#Logistic regression, month_surv
#my_log_surv <- multinom (OFI_categories ~ month_surv, data = new.dataset)

# Logistic regression 
#my_log_prevent_res_surv <- multinom (preventable_death ~ month_surv, data = new.dataset)

