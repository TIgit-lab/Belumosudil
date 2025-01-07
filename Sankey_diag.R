#' Aim : Sankey diagram
#' Project : Belumosudil
#' Author : Sophie Le Grand

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
         NIH_GRADING_BELU_3MONTHS, NIH_GRADING_BELU_6MONTHS) %>%
  mutate(
    CGVHD_NIH_ONSET_GRADING = factor(CGVHD_NIH_ONSET_GRADING, levels = c("Mild", "Moderate", "Severe")),
    NIH_GRADING_AT_BELU_ONSET = factor(NIH_GRADING_AT_BELU_ONSET, levels = c("Moderate", "Severe")),
    NIH_GRADING_BELU_3MONTHS = factor(NIH_GRADING_BELU_3MONTHS, levels = c("Mild", "Moderate", "Severe")),
    NIH_GRADING_BELU_6MONTHS = factor(NIH_GRADING_BELU_6MONTHS, levels = c("Mild", "Moderate", "Severe"))) 

# Datamanagement
links.df <- mdata %>%
  mutate(row = row_number()) %>%                                       
  pivot_longer(cols = -row, names_to = "column", values_to = "source") %>%
  mutate(source = factor(source, levels = c("Mild", "Moderate", "Severe"))) 

links.df <- links.df %>%
  mutate(column = match(column, names(mdata))) %>%  
  group_by(row) %>% 
  mutate(target = lead(source, order_by = column)) %>% 
  filter(!is.na(target)) %>%                           
  ungroup()                                            

links.df <- links.df %>%
  mutate(source = paste0(source, '_', column)) %>%
  mutate(target = paste0(target, '_', column + 1)) %>% 
  select(row, column, source, target)                 

# Nodes df
nodes.df <- data.frame(name = unique(c(links.df$source, links.df$target))) 
nodes.df$label <- sub('_[0-9]*$', '', nodes.df$name)

# Order nodes in df
nodes.df$order <- match(nodes.df$label, c("Mild", "Moderate", "Severe", "NA"))
nodes.df <- nodes.df[order(nodes.df$order), ]

# Links taking in acount nodes order
links.df$source_id <- match(links.df$source, nodes.df$name) - 1
links.df$target_id <- match(links.df$target, nodes.df$name) - 1
links.df$value <- 1 
#'prepare percents in barplots
mdata %>%
  count(NIH_GRADING_BELU_6MONTHS) %>%
  mutate(percentage = n / sum(n) * 100)
# Plots
sankey_plot = sankeyNetwork(Links = links.df,     
              Nodes = nodes.df,     
              Source = 'source_id', 
              Target = 'target_id', 
              Value = 'value',      
              NodeID = 'label', 
              colourScale = JS("d3.scaleOrdinal().range(['#FFC20A', '#0C7BDC', '#4B0092'])"), 
              fontSize = 12, 
              nodeWidth = 30, 
              nodePadding = 10,
              iteration = 0)

#'Save plots#'iterations = Save plots
htmlwidgets::saveWidget(sankey_plot, "sankey_plot1.html")
webshot("sankey_plot1.html", "sankey_plot1.pdf", vwidth = 800, vheight = 600)
