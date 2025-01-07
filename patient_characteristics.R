#' Aim : Patients characteristics
#' Project : Belumosudil
#' Author : Sophie Le Grand

#'Require :
library(readxl)
library(rstatix)
library(tidyverse)
#' Summary tables
library(gtsummary)
library(gt)

#'Datamanagement
belu = read_excel("Dropbox/belumosudil/database rezurock.xlsx") %>%
  select(HEMATOLOGICAL_DISORDER, AGE_AT_DIAGNOSIS, GREFFE_AGE, CONDITIONING_REGIMEN, T_DEPLETION, TBI, GVHD_PROPHYLAXIS, DISEASE_STATUS_AT_TRANSPLANT, HLA_MATCHING,                  
         SEX_MISMATCH, ABO_COMPAT, GRAFT_SOURCE, ACUTE_GVHD_GRADE_MAX, ACUTE_GVHD_REMISSION,            
         CHRONIC_GVHD_ONSET_SEVERITY,                    
         CGVHD_NIH_ONSET_GRADING, NUMBER_OF_LINE_BEFORE_BELU,
         NIH_GRADING_AT_BELU_ONSET, BELU_LINE_NUMBER, BELU_POSOLOGY,               
         PPI, PROPHYLAXIS_ATB, PROPHYLAXIS_ANTIVIR, BELU_REASON_FOR_ARREST,      
         BELU_NIH_GRADING_END_OF_TREATMENT,      
         RELAPSE, LAST_FOLLOWUP_DATE, Follow_up_inmonths, Response_belumosudil_delay,duration_belu_days, 
         delay_belu_newtreatment_or_progression, delay_belu_last_followup, RELAPSE_delay_months,    
         relapse_delay_frombelu) %>%
  mutate(AGE_AT_DIAGNOSIS = as.numeric(AGE_AT_DIAGNOSIS)) %>%
  mutate(Response_belumosudil_delay = as.numeric(Response_belumosudil_delay)) %>%
  mutate(RELAPSE_delay_months = as.numeric(RELAPSE_delay_months))

#'Patient's ELN group in preemptive patients group using tbl_summary
char_patients = belu %>%
  tbl_summary(statistic = all_continuous() ~ c("{median} ({min}, {max})")) %>%
  add_n() %>%
  as_gt() %>%
  gtsave(filename = "char_patients_belu_20241121.docx",
         path = "Dropbox/Tmp Sophie/")




