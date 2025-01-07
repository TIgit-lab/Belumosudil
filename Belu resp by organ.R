#' Aim : Belu over time by organ
#' Project : Belumosudil
#' Author : Sophie Le Grand

# Require
library(ggplot2)
library(dplyr)
library(readxl)
library(cowplot)

# Load data
mdata <- read_excel("belumosudil/recueil/database rezurock.xlsx")
#'plot it
#'Skin
mdata_skin <- mdata %>%
  select(FIRSTNAME, Skin_response, CGVHD_SKIN_STAGING_AT_BELU_ONSET) %>%
  filter(CGVHD_SKIN_STAGING_AT_BELU_ONSET > 0) %>%
  filter(Skin_response != "NA") %>%
  mutate(Skin_response = factor(Skin_response, levels = c("CR+PR", "SD", "PD"))) 

data_percent_skin <- mdata_skin %>%
  count(Skin_response) %>%
  mutate(percentage = n / sum(n) * 100)

plot_skin = mdata_skin %>%
  ggplot(aes(x = 1, fill = Skin_response)) + 
  geom_bar(position = "fill", stat = "count") + 
  scale_fill_manual(values = c("#E66100","#5D3A9B", "#40B0A6")) +
  labs(x = "Skin 43 patients", 
       y = "% patients", 
       fill = "Resp") +  
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        axis.line = element_blank()) +
  geom_text(data = data_percent_skin, 
           aes(x = 1, y = n / sum(n), 
               label = paste0(round(percentage, 1), "%")),
           position = position_stack(vjust = 0.5), size = 5)
print(plot_skin)

data_percent <- mdata %>%
  count(Skin_response) %>%
  mutate(percentage = n / sum(n) * 100)
#'Muscle
mdata_muscle <- mdata %>%
  select(FIRSTNAME, MUSCLE_RESPONSE, CGVHD_MUSCLE_STAGING_AT_BELU_ONSET) %>%
  filter(CGVHD_MUSCLE_STAGING_AT_BELU_ONSET > 0) %>%
  filter(MUSCLE_RESPONSE != "NA") %>%
  mutate(MUSCLE_RESPONSE = factor(MUSCLE_RESPONSE, levels = c("CR+PR", "SD", "PD"))) 

data_percent_muscle <- mdata_muscle %>%
  count(MUSCLE_RESPONSE) %>%
  mutate(percentage = n / sum(n) * 100) 

plot_muscle = mdata_muscle %>% 
  ggplot(aes(x = 1, fill = MUSCLE_RESPONSE)) + 
  geom_bar(position = "fill", stat = "count") +  
  scale_fill_manual(values = c("#E66100","#5D3A9B", "#40B0A6")) +
  labs(x = "Muscle, 21 patients", 
       y = "% patients", 
       fill = "Resp") +  
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        axis.line = element_blank()) +
  geom_text(data = data_percent_muscle, 
            aes(x = 1, y = n / sum(n), 
                label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)
print(plot_muscle)
#'Mouth
mdata_mouth <- mdata %>%
  select(FIRSTNAME, Mouth_response, CGVHD_MOUTH_STAGING_AT_BELU_ONSET) %>%
  filter(CGVHD_MOUTH_STAGING_AT_BELU_ONSET > 0) %>%
  filter(Mouth_response != "NA") %>%
  mutate(Mouth_response = factor(Mouth_response, levels = c("CR+PR", "SD", "PD"))) 

data_percent_mouth <- mdata_mouth %>%
  count(Mouth_response) %>%
  mutate(percentage = n / sum(n) * 100) 

plot_mouth = mdata_mouth %>%
  ggplot(aes(x = 1, fill = Mouth_response)) + 
  geom_bar(position = "fill", stat = "count") +  
  scale_fill_manual(values = c("#E66100","#5D3A9B", "#40B0A6")) +
  labs(x = "Mouth, 29 patients", 
       y = "% patients", 
       fill = "Resp") +  
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        axis.line = element_blank()) +
  geom_text(data = data_percent_mouth, 
            aes(x = 1, y = n / sum(n), 
                label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)
print(plot_mouth)
#'Eyes
mdata_eyes <- mdata %>%
  select(FIRSTNAME, EYES_RESPONSE, CGVHD_EYES_STAGING__AT_BELU_ONSET) %>%
  filter(CGVHD_EYES_STAGING__AT_BELU_ONSET > 0) %>%
  filter(EYES_RESPONSE != "NA") %>%
  mutate(EYES_RESPONSE = factor(EYES_RESPONSE, levels = c("CR+PR", "SD", "PD")))

data_percent_eyes <- mdata_eyes %>%
  count(EYES_RESPONSE) %>%
  mutate(percentage = n / sum(n) * 100) 

plot_eyes = mdata_eyes %>%
  filter(EYES_RESPONSE != "NA") %>%
  ggplot(aes(x = 1, fill = EYES_RESPONSE)) + 
  geom_bar(position = "fill", stat = "count") +  
  scale_fill_manual(values = c("#E66100","#5D3A9B", "#40B0A6")) +
  labs(x = "Eyes, 32 patients", 
       y = "% patients", 
       fill = "Resp") +  
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        axis.line = element_blank()) +
  geom_text(data = data_percent_eyes, 
            aes(x = 1, y = n / sum(n), 
                label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)
print(plot_eyes)
#'Genital
mdata_genitals <- mdata %>%
  select(FIRSTNAME, GENITAL_RESPONSE, CGVHD_GENITALS_STAGING_AT_BELU_ONSET) %>%
  filter(CGVHD_GENITALS_STAGING_AT_BELU_ONSET > 0) %>%
  filter(GENITAL_RESPONSE != "NA") %>%
  mutate(GENITAL_RESPONSE = factor(GENITAL_RESPONSE, levels = c("CR+PR", "SD", "PD"))) 

data_percent_genitals <- mdata_genitals %>%
  count(GENITAL_RESPONSE) %>%
  mutate(percentage = n / sum(n) * 100) 

plot_genital = mdata_genitals %>%
  ggplot(aes(x = 1, fill = GENITAL_RESPONSE)) + 
  geom_bar(position = "fill", stat = "count") +  
  scale_fill_manual(values = c("#E66100","#5D3A9B", "#40B0A6")) +
  labs(x = "genital, 8 patients", 
       y = "% patients", 
       fill = "Resp") +  
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        axis.line = element_blank()) +
  geom_text(data = data_percent_genitals, 
            aes(x = 1, y = n / sum(n), 
                label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)
print(plot_genital)
#'Gut
mdata_gut <- mdata %>%
  select(FIRSTNAME, GUT_RESPONSE, CGVHD_GUT_STAGING_AT_BELU_ONSET) %>%
  filter(CGVHD_GUT_STAGING_AT_BELU_ONSET > 0) %>%
  filter(GUT_RESPONSE != "NA") %>%
  mutate(GUT_RESPONSE = factor(GUT_RESPONSE, levels = c("CR+PR", "SD", "PD"))) 

data_percent_gut <- mdata_gut %>%
  count(GUT_RESPONSE) %>%
  mutate(percentage = n / sum(n) * 100) 

plot_gut = mdata_gut %>%
  ggplot(aes(x = 1, fill = GUT_RESPONSE)) + 
  geom_bar(position = "fill", stat = "count") +  
  scale_fill_manual(values = c("#E66100","#5D3A9B", "#40B0A6")) +
  labs(x = "gut, 9 patients", 
       y = "% patients", 
       fill = "Resp") +  
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        axis.line = element_blank()) +
  geom_text(data = data_percent_gut, 
            aes(x = 1, y = n / sum(n), 
                label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)
print(plot_gut)
#'Liver
mdata_liver <- mdata %>%
  select(FIRSTNAME, LIVER_RESPONSE, CGVHD_LIVER_STAGING_AT_BELU_ONSET) %>%
  filter(CGVHD_LIVER_STAGING_AT_BELU_ONSET > 0) %>%
  filter(LIVER_RESPONSE != "NA") %>%
  mutate(LIVER_RESPONSE = factor(LIVER_RESPONSE, levels = c("CR+PR", "SD", "PD")))

data_percent_liver <- mdata_liver %>%
  count(LIVER_RESPONSE) %>%
  mutate(percentage = n / sum(n) * 100) 

plot_liver = mdata_liver %>%
  ggplot(aes(x = 1, fill = LIVER_RESPONSE)) + 
  geom_bar(position = "fill", stat = "count") + 
  scale_fill_manual(values = c("#E66100","#5D3A9B", "#40B0A6")) +
  labs(x = "liver, 13 patients", 
       y = "% patients", 
       fill = "Resp") +  
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        axis.line = element_blank()) +
  geom_text(data = data_percent_liver, 
            aes(x = 1, y = n / sum(n), 
                label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)
print(plot_liver)
#' Lung
mdata_lung = mdata %>%
  select(FIRSTNAME, LUNG_RESPONSE, CGVHD_LUNG_STAGING_AT_BELU_ONSET) %>%
  filter(CGVHD_LUNG_STAGING_AT_BELU_ONSET > 0) %>%
  filter(LUNG_RESPONSE != "NA") %>%
  mutate(LUNG_RESPONSE = factor(LUNG_RESPONSE, levels = c("CR+PR", "SD", "PD"))) 

data_percent_lung <- mdata_lung %>%
  count(LUNG_RESPONSE) %>%
  mutate(percentage = n / sum(n) * 100) 

plot_lung = mdata_lung %>%
  ggplot(aes(x = 1, fill = LUNG_RESPONSE)) + 
  geom_bar(position = "fill", stat = "count") +  
  scale_fill_manual(values = c("#E66100","#5D3A9B", "#40B0A6")) +
  labs(x = "lung, 26 patients", 
       y = "% patients", 
       fill = "Resp") +  
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        axis.line = element_blank()) +
  geom_text(data = data_percent_lung, 
            aes(x = 1, y = n / sum(n), 
                label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)
print(plot_lung)
#'Other
mdata_other <- mdata %>%
  select(FIRSTNAME, OTHER_RESPONSE, CGVHD_OTHER_STAGING_AT_BELU_ONSET) %>%
  filter(CGVHD_OTHER_STAGING_AT_BELU_ONSET > 0) %>%
  filter(OTHER_RESPONSE != "NA") %>%
  mutate(OTHER_RESPONSE = factor(OTHER_RESPONSE, levels = c("CR+PR", "SD", "PD"))) 

data_percent_other <- mdata_other %>%
  count(OTHER_RESPONSE) %>%
  mutate(percentage = n / sum(n) * 100) 

plot_other = mdata_other %>%
  ggplot(aes(x = 1, fill = OTHER_RESPONSE)) + 
  geom_bar(position = "fill", stat = "count") +  
  scale_fill_manual(values = c("#E66100","#5D3A9B", "#40B0A6")) +
  labs(x = "Other, 2 patients", 
       y = "% patients", 
       fill = "Resp") +  
  theme_classic() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        axis.line = element_blank()) +
  geom_text(data = data_percent_other, 
            aes(x = 1, y = n / sum(n), 
                label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 5)
print(plot_other)

#Save it
pdf("Tmp Sophie/plot_resp_organs.pdf", width = 20, height = 4)
plot_grid(plot_skin, plot_gut, plot_liver, plot_lung, plot_mouth, 
          plot_genital, plot_eyes, plot_muscle, plot_other,
          ncol = 9, align = "hv")
dev.off()


