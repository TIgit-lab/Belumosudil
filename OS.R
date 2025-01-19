#' Aim : OS belu patients
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
  select(Follow_up_inmonths, OS, LAST_FOLLOWUP_DATE, GREFFE_DATE)

mdata$LFU_CALC = as.numeric(difftime(mdata$LAST_FOLLOWUP_DATE, mdata$GREFFE_DATE, units = "days")) 
mdata$LFU_CALC = (mdata$LFU_CALC)/365.25
#'Survival analysis
logtest_OS = Surv(mdata$LFU_CALC, as.numeric(mdata$OS))

surv_OS = survfit2(data = mdata, logtest_OS ~ 1) %>%
  ggsurvplot(
    palette = "#0C7BDC",
    surv.median.line = "hv", 
    conf.int = FALSE,
    font.main = c(16, "plain", "black"),
    font.x = c(16,"plain",  "black"),
    font.y = c(16, "plain","black"),
    font.tickslab = c(16, "plain", "black"),
    ggtheme = theme_bw(), 
    censor.size = 10,
    xlim = c(0, 3.8),
    break.time.by = 1,  
    xlab = "Time (years)",   
    title = "OS belu, med follow up = 47 months/3.9 years", 
    tables.height = 0.2,
    tables.theme = theme_cleantable(),
    risk.table = "abs_pct", 
    risk.table.y.text = FALSE
  )
print(surv_OS)

pdf("tmp/OS_belu.pdf", width = 10, height = 6)
print(surv_OS)
dev.off()
