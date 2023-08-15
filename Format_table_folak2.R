library("labelled")
format_table1 <- function(data) {
  
  ## "res_survival"
  data$res_survival <- factor(
    data$res_survival,
    levels = c(1, 2),     
    labels = c("Dead", "Alive"))
  
  ##"pt_Gender"              
  data$pt_Gender <- factor(
    data$pt_Gender,
    levels = c(2, 1),
    labels = c("Female", "Male"))
  
  #  "hosp_dischg_dest"
  dpc$hosp_dischg_dest <- factor(
    dpc$hosp_dischg_dest,
    levels = c(1, 2, 3, 4, 5, 6, 7), 
    labels = c("Home",
               "Rehab",
               "Morgue",
               "ICU (higher care level)",
               "ICU (same care level)",
               "Other department",
               "psychiatric care"))
  
  # "host_vent_days_NotDone"
  dpc$host_vent_days_NotDone <- factor(
    dpc$host_vent_days_NotDone,
    levels = c(0, 1),
    labels = c("On ventilator", "Not on ventilator"))
  
  # "ed_be_art_NotDone"
  
  #  ######  OBS - Inkongruent/fel jmf med övrig BE kolumn  
  #  
  #  dpc$ed_be_art_NotDone <- factor(
  #    dpc$ed_be_art_NotDone,
  #    levels = c(0, 1),
  #    labels = c("Done", "Not done"))
  
  # "ed_inr_NotDone"   
  dpc$ed_inr_NotDone <- factor(
    dpc$ed_inr_NotDone,
    levels = c(0, 1),
    labels = c("Done", "Not done"))
  
  
  #"pt_asa_preinjury"       
  dpc$pt_asa_preinjury <- factor(
    dpc$pt_asa_preinjury,
    levels = c(1, 2, 3, 4), 
    labels = c("A healthy patient",
               "Mild systemic disease",
               "Severe systemic disease",
               "Life threatening disease"))
  
  #"pre_card_arrest" 
  dpc$pre_card_arrest <- factor(
    dpc$pre_card_arrest,
    levels = c(1, 2),
    labels = c("Yes", "No"))
  
  #"inj_dominant"
  dpc$inj_dominant <- factor(
    dpc$inj_dominant,
    levels = c(1, 2),
    labels = c("Blunt", "Penetrating"))
  
  # "inj_mechanism"  
  dpc$inj_mechanism <- factor(
    dpc$inj_mechanism,
    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
    labels = c("Motor vehicle accident",
               "Motorcycle accident",
               "Bicycle accident",
               "Injured pedestrian",
               "Other vehicle accident",
               "Gunshot wound",
               "Injury from sharp object",
               "Struck by a blunt object",
               "low energy fall",
               "High energy fall",
               "Explosion",
               "Other injury"))
  
  # "inj_intention" 
  dpc$inj_intention <- factor(
    dpc$inj_intention,
    levels = c(1, 2, 3),
    labels = c("Accident", "Self-inflicted", "Abuse"))
  
  #"intub"  
  dpc$intub <- factor(
    dpc$intub,
    levels = c(1, 2, 3),
    labels = c("Inhospital", "Not intubated", "Prehospital"))
  
  #"host_care_level" 
  dpc$host_care_level <- factor(
    dpc$host_care_level,
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Emergency department",
               "General ward",
               "Surgical ward",
               "Specialist ward/Intermediate ward",
               "Intensive care unit"))
  
  #"pt_Gender"              
  dpc$pt_Gender <- factor(
    dpc$pt_Gender,
    levels = c(2, 1),
    labels = c("Female", "Male"))
  
  # "res_survival"
  dpc$res_survival <- factor(
    dpc$res_survival,
    levels = c(1, 2),     
    labels = c("Yes", "No"))
  
  # "ofi" 
  dpc$ofi <- factor(
    dpc$ofi,
    levels = c("Yes", "No"),
    labels = c("OFI", "No OFI"))
  
  ## ed_emerg_proc_other
  data$ed_emerg_proc_other <- factor(
    data$ed_emerg_proc_other,
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Thoracic drainage",
               "External fracture fixation",
               "Major fracture surgery",
               "Surgical wound revision",
               "Other intervention")) 
  
  ## "ed_emerg_proc" 
  data$ed_emerg_proc <- factor(
    data$ed_emerg_proc,
    levels = c(1, 2, 3, 4, 5, 6, 7, 8), # exclusion of 8, 99, 999 (8 - "Other intervention" removed since they are present in ed_emerg_proc_other)
    labels = c("Thoracotomy",
               "Laparotomy - hemostasis",
               "Pelvic packing",
               "Revascularization",
               "Radiological intervention",
               "Craniotomy",
               "Intracranial pressure measurement as sole intervention",
               "Other intervention")) 
  
  ## Merge ed_emerg_proc_other and ed_emerg_proc
  resuscitation.procedures <- as.character(data$ed_emerg_proc)
  index <- resuscitation.procedures == "Other intervention" & !is.na(resuscitation.procedures)
  resuscitation.procedures[index] <- as.character(data$ed_emerg_proc_other[index])
  resuscitation.procedures <- factor(resuscitation.procedures, levels = c(sort(unique(resuscitation.procedures)), "None"))
  resuscitation.procedures[is.na(resuscitation.procedures)] <- "None"
  data$resuscitation.procedures <- factor(resuscitation.procedures, levels = unique(resuscitation.procedures))
  
  
  ## Change column names
  var_label(dpc) <- list(
    ofi = "Opportunity for improvement",
    ed_gcs_sum = "ED GCS",
    ed_sbp_value = "ED Systolic Blood Pressure",
    dt_ed_first_ct = "Time to first CT",
    dt_ed_emerg_proc = "Time to definitive treatment",
    intub = "Intubated",
    host_care_level = "Highest level of care",
    pt_age_yrs = "Age",
    ed_rr_value = "Respiratory rate",
    ISS = "Injury severity score",
    pt_Gender = "Gender",
    res_survival = "Dead at 30 days",
    hosp_los_days = "Days in hospital", 
    inj_dominant ="Injury type",          
    inj_mechanism ="Injury mechanism",
    inj_intention ="Injury intention",
    pt_asa_preinjury ="ASA-score before injury",      
    pre_card_arrest ="PH cardiac arrest",
    pre_gcs_sum ="PH GCS",
    pre_gcs_motor ="PH GCS - Motor response",         
    ed_gcs_motor ="ED GCS",
    pre_sbp_value ="PH systolic blood preassure",         
    pre_rr_value ="PH respiratory rate",
    ed_be_art ="First base excess",     
    ed_inr ="First INR",
    ed_inr_NotDone ="INR not meassured",
    hosp_vent_days ="Days on ventilator",        
    host_vent_days_NotDone ="Ventilator not required",
    hosp_dischg_dest ="Discharge destination",      
    res_gos_dischg ="Discharge glascow outcome scale",
    TraumaAlarmAtHospital ="Type of trauma alarm",
    AlarmRePrioritised ="reprioritizations",
    NISS ="New Injury Severity Score",
    FirstTraumaDT_NotDone ="CT not done",
    dt_alarm_hosp ="Time, alarm to hospital arrival",         
    pre_provided ="Highest PH competence",
    pre_intub_type ="PH airway management",        
    ed_intub_type ="Hospital airway management",
    pre_transport ="Arrival mode",         
    ed_emerg_proc ="Emergency procedure",
    ed_emerg_proc_other ="Other emergency prodecures",
    host_transfered ="Transfer status",
    dt_alarm_scene ="Injury scene time",        
    dt_ed_norm_be ="Time to normal base excess",
    NumberOfActions ="Number of interventions",
    NumberOfInjuries ="Number of injuries",
  resuscitation.procedures = "Resuscitation procedure")
  
  
  formated.data <- dpc
  return(formated.data)
  
}
  