# Redbreast project 2022 
# Date: 05/17/2022 
# Author(s): Olivia H Hawkins
# Goals: Make morphospace comparisons across centrarchids 

# Git hub repository 
browseURL("https://github.com/hawkinso/Redbreast2022.git")

# Load in libraries ----
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rstatix)
library(car)
library(plyr)
library(reshape2)
library(lmer4)
library(RColorBrewer)
library(lmerTest)
library(reshape2)
library(rptR)
library(ggridges)
library(ICC)
library(cowplot)

# Load in data 
data <- read.csv("SpecimenMeans_2022.csv")

# Summarize the data for each species irrespective of collection ID and state

species.sum <- data %>% 
  group_by(Species) %>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height,Mean.protrusion) %>%
  get_summary_stats()

# Summarize the data for each species with collection in mind and not accounting for state

species.sum.EAK <- data %>% 
  filter(Collection=="EAK") %>%
  group_by(Species)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height,Mean.protrusion) %>%
  get_summary_stats()

species.sum.LSU <- data %>% 
  filter(Collection=="LSU") %>%
  group_by(Species)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height,Mean.protrusion) %>%
  get_summary_stats()

# Summarize the data for each species with collection and state in mind 
species.sum.EAK.state <- data %>% 
  filter(Collection=="EAK") %>%
  group_by(Species,Location)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height,Mean.protrusion) %>%
  get_summary_stats()

species.sum.LSU.state <- data %>% 
  filter(Collection=="LSU") %>%
  group_by(Species,Location)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height,Mean.protrusion) %>%
  get_summary_stats()
