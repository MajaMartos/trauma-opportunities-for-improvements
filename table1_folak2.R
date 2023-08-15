
## Get format for variables
new.dataset <- format_table(new.dataset)

#merge all cohorts that are not blunt multisystem withoit TBI for table 1
new.dataset <- new.dataset %>%
  mutate(cohort = ifelse(cohort != "blunt multisystem without TBI", "other cohort", cohort))

## Table stratified by bunt multisystem without tbi
#new.dataset$cohort <- factor(new.dataset$cohort, levels = c("Yes", "No"), labels = c("OFI", "No OFI"))

## Create table 1
table1 <- new.dataset %>%
  select(pt_age_yrs, 
         pt_Gender, 
         ISS, 
         ed_rr_value, 
         ed_gcs_sum, 
         ed_sbp_value, 
         resuscitation.procedures, 
         res_survival,
         cohort) %>%
  tbl_summary(by = "cohort") %>%
  add_p(test.args = all_categorical() ~ list(simulate.p.value = TRUE)) %>%
  add_overall() %>%
  bold_labels %>%
  as_tibble() %>%
  replace(is.na(.), "")


## Save to disk and compile as word document
saveRDS(table1, "table1.Rds")
write(kable(table1, format = "pandoc"), "table1.Rmd")
rmarkdown::render("table1.Rmd", output_format = "word_document")
