#' Aim : Belu response over time
#' Project : Belumosudil
#' Author : Sophie Le Grand

#'Require
library(readxl)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

# Load data
mdata = read_excel("belumosudil/recueil/database rezurock.xlsx") %>%
  select(FIRSTNAME, BELU_RESPONSE_AT_3MONTHS, BELU_RESPONSE_AT_6MONTHS, BELU_BEST_RESPONSE) %>%
  mutate(
    BELU_RESPONSE_AT_3MONTHS = factor(BELU_RESPONSE_AT_3MONTHS, 
                                      levels = c("complete remission","partial response", "stable disease", "progression", "NA")),
    BELU_RESPONSE_AT_6MONTHS = factor(BELU_RESPONSE_AT_6MONTHS, 
                                      levels = c("complete remission","partial response", "stable disease", "progression", "NA")),
    BELU_BEST_RESPONSE = factor(BELU_BEST_RESPONSE, 
                                levels = c("complete remission","partial response", "stable disease", "progression", "NA"))
  )

# Datamanagement
mdata_long <- mdata %>%
  pivot_longer(cols = -FIRSTNAME, 
               names_to = "time_point", 
               values_to = "response") %>%
  mutate(time_point = recode(time_point, 
                             BELU_RESPONSE_AT_3MONTHS = "3 months",
                             BELU_RESPONSE_AT_6MONTHS = "6 months", 
                             BELU_BEST_RESPONSE = "Best Response"))
#'prepare percents in barplots
data_percent <- mdata_long %>%
  count(time_point, response) %>%
  group_by(time_point) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()
#plot
plot_resp_combined <- ggplot(mdata_long, aes(x = time_point, fill = response)) +
  geom_bar(position = "fill", stat = "count") + 
  scale_fill_manual(values = c("NA" = "black", 
                               "stable disease" = "#1B9E77", 
                               "progression" = "#D95F02", 
                               "partial response" = "#7570B3", 
                               "complete remission" = "#E7298A")) +
  theme_classic() +
  xlab("Time point") +
  ylab("Response") +
  ggtitle("Belu Response Over Time") +
  theme(axis.text.y = element_blank(),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 14)) +
  geom_text(data = data_percent, 
            aes(y = n / sum(n), 
                label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4)
print(plot_resp_combined)

#'Save plots
pdf("tmp Sophie/belu_response.pdf")
print(plot_resp_combined)
dev.off()



