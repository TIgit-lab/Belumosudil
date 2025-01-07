#' Aim : Sankey diagram with resp
#' Project : Belumosudil
#' Author : Sophie Le Grand

#'Require
library(readxl)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(networkD3)
library(webshot)

set.seed(42)
# Load data
mdata = read_excel("belumosudil/recueil/database rezurock SANKEY.xlsx") %>%
  select(CGVHD_NIH_ONSET_GRADING, 
         NIH_GRADING_AT_BELU_ONSET, 
         BELU_RESPONSE_AT_3MONTHS, BELU_RESPONSE_AT_6MONTHS) %>%
  mutate(CGVHD_NIH_ONSET_GRADING = factor(CGVHD_NIH_ONSET_GRADING, levels = c("Mild", "Moderate", "Severe"))) %>%
  mutate(BELU_RESPONSE_AT_3MONTHS = factor(BELU_RESPONSE_AT_3MONTHS, levels = c("complete remission", "partial response", "stable disease", "progression", "NA"))) %>%
  arrange(CGVHD_NIH_ONSET_GRADING, BELU_RESPONSE_AT_3MONTHS)

#Datamanagement
links.df <- mdata %>%
  mutate(row = row_number()) %>%                                       
  pivot_longer(cols = -row, names_to = "column", values_to = "source") %>%
  mutate(source = factor(source, levels = c("Mild", "Moderate", "Severe", "complete remission", "partial response", "stable disease", "progression", "NA")))


links.df <- links.df %>%
  mutate(column = match(column, names(mdata))) %>%  
  group_by(row) %>% 
  mutate(target = lead(source, order_by = column)) %>% 
  filter(!is.na(target)) %>%                           
  ungroup()                                            


links.df <-
  links.df %>%
  mutate(source = paste0(source, '_', column)) %>%
  mutate(target = paste0(target, '_', column + 1)) %>% 
  select(row, column, source, target)                 

#Prepare nodes
nodes.df <- data.frame(name = unique(c(links.df$source, links.df$target))) 
nodes.df$label <- sub('_[0-9]*$', '', nodes.df$name)
# Order nodes in df
nodes.df$order <- match(nodes.df$label, c("Mild", "Moderate", "Severe", 
                                          "complete remission", "partial response", "stable disease", "progression", "NA"))
nodes.df <- nodes.df[order(nodes.df$order), ]
# Links taking in acount nodes order
links.df$source_id <- match(links.df$source, nodes.df$name) - 1
links.df$target_id <- match(links.df$target, nodes.df$name) - 1 
links.df$value <- 1  

#Plot
sankey_plot = sankeyNetwork(Links = links.df,     
              Nodes = nodes.df,     
              Source = 'source_id', 
              Target = 'target_id', 
              Value = 'value',      
              NodeID = 'label', 
              colourScale = JS("d3.scaleOrdinal().range(['#FFC20A', '#0C7BDC', '#4B0092', '#E7298A', '#7570B3', '#D95F02', '#1B9E77', 'black'])"), 
              fontSize = 12, 
              nodeWidth = 40, 
              nodePadding = 15, 
              iteration = 0)     
#'prepare percents in barplots
mdata %>%
  count(BELU_RESPONSE_AT_6MONTHS) %>%
  mutate(percentage = n / sum(n) * 100)
#'Save plots#'iterations = Save plots
htmlwidgets::saveWidget(sankey_plot, "sankey_plot_resp.html")
webshot("sankey_plot_resp.html", "sankey_plot_resp.pdf", vwidth = 800, vheight = 600)
