
###########################
# create regression model #
###########################

# Convert categorical variables to factors
# MM: har gjort om alla NA  till other för preventable death, ofis, low_GCS 

table.dataset$OFI_categories <- factor(table.dataset$OFI_categories)
table.dataset$cohort <- factor(table.dataset$cohort)
table.dataset$Gender <- factor(table.dataset$Gender)
table.dataset$month_surv <- factor(table.dataset$month_surv)
table.dataset$low_GCS <- factor(table.dataset$low_GCS)


# Re-code the OFI_categories variable so that "Exemplary treatment" is the new reference category
table.dataset$cohort <- relevel(table.dataset$cohort, ref = "other cohort")
table.dataset$OFI_categories <- relevel(table.dataset$OFI_categories, ref = "No ofi")

# Creating the unadjusted logistic regression model for cohorts 

my_log_unad <- multinom(OFI_categories ~ cohort, data = new.dataset)


#install.packages("gtsummary")
library(gtsummary)

# Create table for unadjusted regression
table.unadjust <-tbl_regression(my_log_unad, 
                                exponentiate = TRUE) %>%as_gt()


######################################################################
## Räkna ut p-värden och OR för icke justerade för att kunna hänvisa #
######################################################################

# Z värden
z_wout <- summary(my_log_unad)$coefficients/summary(my_log_unad)$standard.errors

# p värden
p_values <- as.data.frame((1 - pnorm(abs(z_wout), 0, 1)) * 2)

# Namn på p-värdes kolumnen
colnames(p_values) <- paste(colnames(p_values), "_p_value", sep = "")

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
  "cohortsevere penetrating",
  "cohortsevere penetrating_p_value"
) # add this parenthesis

# Byt namn så att det är lättare att läsa
colnames(main.result) <- c(
  "Reference",
  "Reference p-value",
  "BM without TBI",
  "BM without TBI p-value",
  "Isolated TBI",
  "Isolated TBI p-value",
  "Severe penetrating",
  "Severe penetrating p-value"
) 

# Avrunda till 3
main.result <- round(main.result, digits = 3)



###################
# Justerad modell #
###################
#JA: snackade med martin, förslag för att minska lite:
# Två resultat, en helt ojusterad (ovan), och en helt justerad enl minimum:
# Förslag då att justera för ålder, kön och skadegrad (kan ev va NISS om du vill)
# Ytterligare justering kan vi göra i mån om tid/önskan men det blir stora tabeller osv.

# Create ajdusted multinomial logistic regression model
my_log_adj <- multinom( OFI_categories ~ cohort + pt_age_yrs + Gender + NISS, data = table.dataset)

# Create table for Adjusted regression
table.adjust <-tbl_regression(my_log_adj, 
                                exponentiate = TRUE) %>%
  as_gt()

#################################################
# Övriga modeller som vi gör om du får tid över #
#################################################
# Logistic regression,  preventable death 
#my_log_preventable <- multinom (OFI_categories ~ preventable_death, data = table.dataset)

# Logistic regression, age 
#my_log_age <- multinom (OFI_categories ~ pt_age_yrs, data = table.dataset)

#Logistic regression, Gender 
#my_log_gender <- multinom (OFI_categories ~ Gender, data = table.dataset)

#Logistic regression, month_surv
#my_log_surv <- multinom (OFI_categories ~ month_surv, data = table.dataset)

# Logistic regression 
#my_log_prevent_res_surv <- multinom (preventable_death ~ month_surv, data = table.dataset)

