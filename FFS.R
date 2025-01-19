#' Aim : FFS belu patients
#' Project : Belumosudil
#' Author : Sophie Le Grand

#'Require
#'Data management
library(readxl)
library(tidyverse)
#'Survival analysis
library(ggsurvfit)
library(ggplot2)
library(survminer)
library(RColorBrewer)

#' OS from HSCT preemptiv 
#'Load files
mdata = read_excel("Dropbox/belumosudil/database rezurock.xlsx") %>%
  select(CENTRE, FIRSTNAME, Follow_up_inmonths, FFS, delay_belu_last_followup, BELU_START_DATE, LAST_FOLLOWUP_DATE)
mdata$delay_belu_last_followup = (mdata$delay_belu_last_followup)/30.4375
#'Survival analysis
logtest_FFS = Surv(mdata$delay_belu_last_followup, as.numeric(mdata$FFS))

surv_FFS = survfit2(data = mdata, logtest_FFS ~ 1) %>%
  ggsurvplot(
    palette = "#ffbe33",
    surv.median.line = "hv", 
    conf.int = FALSE,
    font.main = c(16, "plain", "black"),
    font.x = c(16,"plain",  "black"),
    font.y = c(16, "plain","black"),
    font.tickslab = c(16, "plain", "black"),
    ggtheme = theme_bw(), 
    censor.size = 10,
    xlim = c(0, 12),
    break.time.by = 2,  
    xlab = "Time (months)",   
    title = "FFS belu all cohort, med followup = 337 days, 11 months", 
    tables.height = 0.2,
    tables.theme = theme_cleantable(),
    risk.table = "abs_pct", 
    risk.table.y.text = FALSE
  )
print(surv_FFS)

pdf("tmp/FFS_belu.pdf", width = 10, height = 6)
print(surv_FFS)
dev.off()
  
summary( survfit2(data = mdata, logtest_FFS ~ 1), times = (0:18))
  
