
## create dataset with only BM without TBI
bm_without_tbi <- subset(new.dataset, cohort == "blunt multisystem without TB")

# Convert categorical variables to factors
bm_without_tbi$OFI_categories <- factor(bm_without_tbi$OFI_categories)
bm_without_tbi$cohort <- factor(bm_without_tbi$cohort)
bm_without_tbi$Gender <- factor(bm_without_tbi$Gender)
bm_without_tbi$month_surv <- factor(bm_without_tbi$month_surv)
bm_without_tbi$low_GCS <- factor(bm_without_tbi$low_GCS)
bm_without_tbi$resuscitation.procedures <- factor(bm_without_tbi$resuscitation.procedures)




# create two models
model1 <- glm(Judgement_error == "Clinical judgement error" ~ pt_Gender  + pt_age_yrs, data = bm_without_tbi, family = binomial())

model1.table <- as_kable(tbl_regression(model1, exponentiate = TRUE), format = "pandoc")
saveRDS(model1.table, "model1-table.Rds")
write(model1.table, "table2.Rmd")
rmarkdown::render("table2.Rmd", output_format = "word_document")


model2 <- glm(Judgement_error == "Clinical judgement error" ~ pt_Gender + res_survival + pt_age_yrs + ed_sbp_value + ed_rr_value + ed_gcs_sum + ISS, data = bm_without_tbi, family = binomial())

model2.table <- as_kable(tbl_regression(model2, exponentiate = TRUE), format = "pandoc")
saveRDS(model1.table, "model2-table.Rds")
write(model1.table, "table3.Rmd")
rmarkdown::render("table3.Rmd", output_format = "word_document")

model3 <- glm(Judgement_error == "Clinical judgement error" ~ pt_Gender + res_survival + pt_age_yrs + ed_sbp_value + ed_rr_value + ed_gcs_sum + ISS + resuscitation.procedures, data = bm_without_tbi, family = binomial())

model3.table <- as_kable(tbl_regression(model3, exponentiate = TRUE), format = "pandoc")
saveRDS(model1.table, "model1-table.Rds")
write(model1.table, "table4.Rmd")
rmarkdown::render("table4.Rmd", output_format = "word_document")


## combine the two models using rbind()
combined_model <- cbind(summary(model1)$coefficients, summary(model2)$coefficients, summary(model3)$coefficients)

## print the combined model
model_table <- tbl_regression(combined_model)
print(model_table)