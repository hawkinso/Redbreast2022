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

# Summarize the data for each species irrespective of collection ID and state----

species.sum <- data %>% 
  select(Sci.name,Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  convert_as_factor(Sci.name) %>%
  group_by(Sci.name) %>%
  get_summary_stats()

species.sum.fig <- species.sum %>%
  select(Sci.name,variable,mean,sd)%>%
  convert_as_factor(Sci.name)

means <- data %>%
  filter(Collection=="LSU")%>%
  group_by(Sci.name) %>%
  summarise(Mean.D.L=mean(Mean.D.L.ratio),
            sd.DL = sd(Mean.D.L.ratio),
            Mean.pos = mean(Mean.dorsal.fin.position),
            sd.pos = sd(Mean.dorsal.fin.position))
  
# Summarize the data for each species with collection in mind and not accounting for state ----

# EAK Collection
species.sum.EAK <- data %>% 
  filter(Collection=="EAK") %>% 
  select(Sci.name,Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  convert_as_factor(Sci.name) %>%
  group_by(Sci.name) %>%
  get_summary_stats()

means.EAK <- data %>%
  group_by(Sci.name) %>%
  filter(Collection=="EAK") %>% 
  summarise(Mean.D.L=mean(Mean.D.L.ratio),
            sd.DL = sd(Mean.D.L.ratio),
            Mean.pos = mean(Mean.dorsal.fin.position),
            sd.pos = sd(Mean.dorsal.fin.position))

# LSU Collection
species.sum.LSU <- data %>% 
  filter(Collection=="LSU") %>%
  select(Sci.name,Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  convert_as_factor(Sci.name) %>%
  group_by(Sci.name)%>%
  get_summary_stats()

means.LSU <- data %>%
  group_by(Sci.name) %>%
  filter(Collection=="LSU") %>% 
  summarise(Mean.D.L=mean(Mean.D.L.ratio),
            sd.DL = sd(Mean.D.L.ratio),
            Mean.pos = mean(Mean.dorsal.fin.position),
            sd.pos = sd(Mean.dorsal.fin.position))

# Double check that the species we have from EAK (frozen) and LSU (ethanol) collections are comparable 
allmeans.EAK <- data %>%
  select(Sci.name,Collection, Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height)%>%
  filter(Collection=="EAK") 

allmeans.LSU <- data %>%
  select(Sci.name, Collection,Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height)%>%
  filter(Collection=="LSU") 

# Visual check 
species.sum.col <- data %>% 
  group_by(Sci.name,Collection)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height)

ggplot(data = species.sum.col,aes(x=Mean.D.L.ratio,y=Mean.dorsal.fin.position,color=Sci.name,shape=Collection))+
  geom_point()+
  theme_classic()

# Summarize the data for each species with collection and state in mind ----
species.sum.EAK.state <- data %>% 
  filter(Collection=="EAK") %>%
  group_by(Species,Location)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  get_summary_stats()

species.sum.LSU.state <- data %>% 
  filter(Collection=="LSU") %>%
  group_by(Species,Location)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  get_summary_stats()

# Double check that the species we have from Georgia and Louisiana collections are comparable 
species.sum.state <- data %>% 
  group_by(Sci.name,Location)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height)

ggplot(data = species.sum.state,aes(x=Mean.D.L.ratio,y=Mean.dorsal.fin.position,color=Sci.name,shape=Location))+
  geom_point()+
  theme_classic()

# Stats check ----
# Frozen vs ethanol, LSU vs EAK, Georgia vs Louisiana basically the same grouping
# use t test to figure out if the speciemens from EAK can be included with the LSU measures 
t.test(allmeans.EAK$Mean.D.L.ratio[allmeans.EAK$Sci.name=="Lepomis auritus"],allmeans.LSU$Mean.D.L.ratio[allmeans.LSU$Sci.name=="Lepomis auritus"])
t.test(allmeans.EAK$Mean.D.L.ratio[allmeans.EAK$Sci.name=="Lepomis punctatus"],allmeans.LSU$Mean.D.L.ratio[allmeans.LSU$Sci.name=="Lepomis punctatus"])
t.test(allmeans.EAK$Mean.D.L.ratio[allmeans.EAK$Sci.name=="Lepomis macrochirus"],allmeans.LSU$Mean.D.L.ratio[allmeans.LSU$Sci.name=="Lepomis macrochirus"])

t.test(allmeans.EAK$Mean.dorsal.fin.position[allmeans.EAK$Sci.name=="Lepomis auritus"],allmeans.LSU$Mean.dorsal.fin.position[allmeans.LSU$Sci.name=="Lepomis auritus"])
t.test(allmeans.EAK$Mean.dorsal.fin.position[allmeans.EAK$Sci.name=="Lepomis punctatus"],allmeans.LSU$Mean.dorsal.fin.position[allmeans.LSU$Sci.name=="Lepomis punctatus"])
t.test(allmeans.EAK$Mean.dorsal.fin.position[allmeans.EAK$Sci.name=="Lepomis macrochirus"],allmeans.LSU$Mean.dorsal.fin.position[allmeans.LSU$Sci.name=="Lepomis macrochirus"])

# GRAPH 1: body depth/length ratio and position of dorsal fin 
# using pooled data for species (no constraint of collection,individual,location,year,preservation method)

ggplot(data = means,aes(x=Mean.D.L,y=Mean.pos,color=Sci.name,shape=Sci.name))+
  geom_point(size=3)+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,11))+
  geom_errorbar(aes(ymin = Mean.pos-sd.pos,ymax = Mean.pos+sd.pos),) + 
  geom_errorbar(aes(xmin = Mean.D.L-sd.DL,xmax = Mean.D.L + sd.DL))+
  theme_classic()+
  xlab("Body depth/Body length")+
  ylab("Dorsal fin position")+
  xlim(0.3,0.45)+
  ylim(0.3,0.40)
 

