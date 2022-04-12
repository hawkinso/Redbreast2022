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
library(lme4)
library(RColorBrewer)
library(lmerTest)
library(reshape2)
library(rptR)
library(ggridges)
library(strucchange)
library(effects)
library(sjmisc)
library(sjstats)
library(cowplot)
library(sjPlot)
library(rcompanion)


# Load in data sets --- 
# Still using all.data subset 

# Amend dataset with morphology 
# Need to scale by maximum anatomical gape, so we will add a column with the value of the proportion of realized/potential gape 

measures <- read.csv("MorphologyMeasurements_Redbreast_2022_averaged.csv")

data_merged <- merge(all.data, measures, by= "Individual")
data_merged$Gape_prop <- data_merged$PG/data_merged$Gape_height

# add to existing subsets 

all.data$Gape_prop <- data_merged$Gape_prop
all.data$VELPG_scale_ind <- all.data.ind$VELPG_scale_ind

# Plot integration lines by individual 
# PG data scaled by maximum anatomical gape 
ggplot()+
  geom_point(all.data, mapping=aes(x=VELPG_scale_ind,y=Gape_prop,group=Individual,color=Individual,shape=Individual),alpha=0.75)+
  geom_smooth(all.data,method = "lm",se=F,mapping=aes(x=VELPG_scale_ind,y=Gape_prop,group=Individual,color=Individual))+
  scale_color_manual(values=park_palettes$Everglades)+
  theme_classic()+
  xlab("Scaled velocity at peak gape (cm/s)")+
  ylab("Proportion of anatomical gape (cm)")+ 
  theme(axis.title.x=element_text(face="bold",size=14),
        axis.title.y=element_text(face="bold",size=14),
        legend.position = "top",
        legend.title = element_text(face="bold"))

# Linear Mixed Models  ----
# Get intercept and slopes for each individual line ----

all.data.mod.fish1 <- lm(all.data$Gape_prop[all.data$Individual=="LAUR01"]~all.data$VELPG_scale_ind[all.data$Individual=="LAUR01"])
summary(all.data.mod.fish1)

all.data.mod.fish2 <- lm(all.data$Gape_prop[all.data$Individual=="LAUR02"]~all.data$VELPG_scale_ind[all.data$Individual=="LAUR02"])
summary(all.data.mod.fish2)


all.data.mod.fish3 <- lm(all.data$Gape_prop[all.data$Individual=="LAUR03"]~all.data$VELPG_scale_ind[all.data$Individual=="LAUR03"])
summary(all.data.mod.fish3)


all.data.mod.fish4 <- lm(all.data$Gape_prop[all.data$Individual=="LAUR04"]~all.data$VELPG_scale_ind[all.data$Individual=="LAUR04"])
summary(all.data.mod.fish4)


all.data.mod.fish5 <- lm(all.data$Gape_prop[all.data$Individual=="LAUR05"]~all.data$VELPG_scale_ind[all.data$Individual=="LAUR05"])
summary(all.data.mod.fish5)


# Visualize differences within individual 
ggplot()+
  geom_point(all.data, mapping=aes(x=VELPG_scale_ind,y=Gape_prop,group=Individual,color=Individual))+
  geom_smooth(all.data,method = "lm",se=F,mapping=aes(x=VELPG_scale_ind,y=Gape_prop,group=Individual,color=Individual))+
  scale_color_manual(values=park_palettes$Everglades)+
  theme_classic()+
  xlab("Scaled velocity at peak gape (cm/s)")+
  ylab("Proportion of maximum anatomical gape at peak gape")+ 
  theme(axis.title.x=element_text(face="bold"),
        axis.title.y=element_text(face="bold"),
        legend.position = "none")+
  facet_grid(~Individual)

## 

# Linear mixed model with VELPG as fixed effect and individual as random effect 

all.lmm <- lmer(Gape_prop~VELPG_scaled_ind  + (1|Individual),data=all.data)
summary(all.lmm)

tab_model(all.lmm,
          pred.labels =c("(Intercept)","VELPG"))

ggplot()+
  geom_point(all.data, mapping=aes(x=VELPG_scaled_ind,y=Gape_prop,color=Individual))+
  geom_smooth(all.data,method = "lm",se=F,mapping=aes(x=VELPG_scaled_ind,y=Gape_prop))+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Scaled velocity at peak gape (cm/s)")+
  ylab("Proportion of anatomical gape (cm)")+ 
  theme(axis.title.x=element_text(face="bold",size=14),
        axis.title.y=element_text(face="bold",size=14),
        legend.position="top")
 

## General mixed model (GLM) for all possible interactions 
# Make all possible models 

full <- glm(Gape_prop~VELPG_scaled_ind + Individual + VELPG_scaled_ind*Individual, data= all.data)
summary(full)

red1 <- glm(Gape_prop~VELPG_scaled_ind + Individual, data= all.data)
summary(red1)

red2 <- glm(Gape_prop~VELPG_scaled_ind, data= all.data)
summary(red2)


mod.check <- compareGLM(full,red1,red2)


# plot effect sizes for model 2
plot_model(full, 
           axis.labels=c("VELPG*Individual-LAUR05","VELPG*Individual-LAUR04",
                         "VELPG*Individual-LAUR03","VELPG*Individual-LAUR02",
                         "Individual-LAUR05","Individual-LAUR04","Individual-LAUR03",
                         "Individual-LAUR02","VELPG"),
           show.values=TRUE, show.p=TRUE,
           title="Effects of Velocity at Peak Gape and Individual on Integration",value.size = ,dot.size = 2)+
  theme_sjplot()+
  scale_color_brewer(palette="Dark2")

# Get table with results 
tab_model(full)

tab_model(full, 
          show.re.var= TRUE, 
          pred.labels =c("(Intercept)","VELPG","Individual-LAUR02","Individual-LAUR03",
                         "Individual-LAUR04","Individual-LAUR05",
                         "VELPG*Individual-LAUR02","VELPG*Individual-LAUR03",
                         "VELPG*Individual-LAUR04","VELPG*Individual-LAUR05"),
          dv.labels= "Effects of Velocity at Peak Gape and Individual on Integration")


