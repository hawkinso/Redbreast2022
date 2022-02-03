# Redbreast project 2022 
# Date: 02/03/2022
# Author(s): Olivia H Hawkins 
# Goals: Graphing integration and comparing slopes/models 

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

# Load in data sets --- 
small <- read.csv("SmallMouth_Redbreast2022.csv")
large <- read.csv("LargeMouth_Redbreast2022.csv")

# Make sure data is grouped by individual 
small.data <- small %>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

large.data <- large %>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

# Plot integration lines by indivdual using center and scaled data 
ggplot()+
  geom_point(large.mouth, mapping=aes(x=VELPG_scaled_ind,y=PG_scaled_ind,group=Individual,color=Individual,shape=Strategy))+
  geom_smooth(large.mouth,method = "lm",se=F,mapping=aes(x=VELPG_scaled_ind,y=PG_scaled_ind,group=Individual,color=Individual))+
  scale_color_brewer(palette="Dark2")+
  geom_hline(yintercept = -0.2,linetype=2)+
  geom_point(small.mouth, mapping=aes(x=VELPG_scaled_ind,y=PG_scaled_ind,group=Individual,color=Individual,shape=Strategy))+
  geom_smooth(small.mouth,method = "lm",se=F,mapping=aes(x=VELPG_scaled_ind,y=PG_scaled_ind,group=Individual,color=Individual))+
  theme_classic()+
  xlab("Scaled velocity at peak gape (cm/s)")+
  ylab("Scaled peak gape (cm)")+ 
  theme(axis.title.x=element_text(face="bold"),
        axis.title.y=element_text(face="bold"))

# Linear Mixed Models  ----
# Comapre the lines within individual by strategy 

mod.fish1 <- glm(all.data$PG_scaled_ind[all.data$Individual=="LAUR01"]~all.data$VELPG_scaled_ind[all.data$Individual=="LAUR01"] + all.data$Strategy[all.data$Individual=="LAUR01"])
summary(mod.fish1)

mod.fish2 <- glm(all.data$PG_scaled_ind[all.data$Individual=="LAUR02"]~all.data$VELPG_scaled_ind[all.data$Individual=="LAUR02"] + all.data$Strategy[all.data$Individual=="LAUR02"])
summary(mod.fish2)

mod.fish3 <- glm(all.data$PG_scaled_ind[all.data$Individual=="LAUR03"]~all.data$VELPG_scaled_ind[all.data$Individual=="LAUR03"] + all.data$Strategy[all.data$Individual=="LAUR03"])
summary(mod.fish3)

mod.fish4 <- glm(all.data$PG_scaled_ind[all.data$Individual=="LAUR04"]~all.data$VELPG_scaled_ind[all.data$Individual=="LAUR04"] + all.data$Strategy[all.data$Individual=="LAUR04"])
summary(mod.fish4)

mod.fish5 <- glm(all.data$PG_scaled_ind[all.data$Individual=="LAUR05"]~all.data$VELPG_scaled_ind[all.data$Individual=="LAUR05"] + all.data$Strategy[all.data$Individual=="LAUR05"])
summary(mod.fish5)

# Get intercept and slopes for each individual line ----

## SMALL MOUTH 
small.mod.fish1 <- lm(small$PG_scaled_ind[small$Individual=="LAUR01"]~small$VELPG_scaled_ind[small$Individual=="LAUR01"])
summary(small.mod.fish1)

# This individual cannot be used for a singular regression because there are two points 
small.mod.fish2 <- lm(small$PG_scaled_ind[small$Individual=="LAUR02"]~small$VELPG_scaled_ind[small$Individual=="LAUR02"])
summary(small.mod.fish2)

small.mod.fish3 <- lm(small$PG_scaled_ind[small$Individual=="LAUR03"]~small$VELPG_scaled_ind[small$Individual=="LAUR03"])
summary(small.mod.fish3)

small.mod.fish4 <- lm(small$PG_scaled_ind[small$Individual=="LAUR04"]~small$VELPG_scaled_ind[small$Individual=="LAUR04"])
summary(small.mod.fish4)

small.mod.fish5 <- lm(small$PG_scaled_ind[small$Individual=="LAUR05"]~small$VELPG_scaled_ind[small$Individual=="LAUR05"])
summary(small.mod.fish5)

## LARGE MOUTH 
large.mod.fish1 <- lm(large$PG_scaled_ind[large$Individual=="LAUR01"]~large$VELPG_scaled_ind[large$Individual=="LAUR01"])
summary(large.mod.fish1)

large.mod.fish2 <- lm(large$PG_scaled_ind[large$Individual=="LAUR02"]~large$VELPG_scaled_ind[large$Individual=="LAUR02"])
summary(large.mod.fish2)

large.mod.fish3 <- lm(large$PG_scaled_ind[large$Individual=="LAUR03"]~large$VELPG_scaled_ind[large$Individual=="LAUR03"])
summary(large.mod.fish3)

large.mod.fish4 <- lm(large$PG_scaled_ind[large$Individual=="LAUR04"]~large$VELPG_scaled_ind[large$Individual=="LAUR04"])
summary(large.mod.fish4)

large.mod.fish5 <- lm(large$PG_scaled_ind[large$Individual=="LAUR05"]~large$VELPG_scaled_ind[large$Individual=="LAUR05"])
summary(large.mod.fish5)