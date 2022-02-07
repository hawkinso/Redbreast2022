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
small <- read.csv("SmallMouth_Redbreast2022.csv")
large <- read.csv("LargeMouth_Redbreast2022.csv")

# Make sure data is grouped by individual 
small.data <- small %>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

large.data <- large %>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

# Plot integration lines by individual using center and scaled data 
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
        axis.title.y=element_text(face="bold"))+
  

# Linear Mixed Models  ----
# Compare the lines within individual by strategy 
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

# Visualize differences within individual 
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
        axis.title.y=element_text(face="bold"))+
  facet_grid(~Individual)

## 

# Across strategy- is there a difference? 
# Linear mixed model with strategy as fixed effect and individual as random effect 

small.all <- lmer(PG_scaled_ind~VELPG_scaled_ind  + (1|Individual),data=small)
summary(small.all)

large.all <- lmer(PG_scaled_ind~VELPG_scaled_ind  + (1|Individual),data=large)
summary(large.all)

ggplot()+
  geom_point(large.mouth, mapping=aes(x=VELPG_scaled_ind,y=PG_scaled_ind,shape=Strategy,color=Individual))+
  geom_smooth(large.mouth,method = "lm",se=F,mapping=aes(x=VELPG_scaled_ind,y=PG_scaled_ind))+
  scale_color_brewer(palette="Dark2")+
  geom_hline(yintercept = -0.2,linetype=2)+
  geom_point(small.mouth, mapping=aes(x=VELPG_scaled_ind,y=PG_scaled_ind,shape=Strategy,color=Individual))+
  geom_smooth(small.mouth,method = "lm",se=F,mapping=aes(x=VELPG_scaled_ind,y=PG_scaled_ind))+
  theme_classic()+
  xlab("Scaled velocity at peak gape (cm/s)")+
  ylab("Scaled peak gape (cm)")+ 
  theme(axis.title.x=element_text(face="bold"),
        axis.title.y=element_text(face="bold"))
 

## Linear Mixed Model for all possible interactions 
# Make all possible models 

full <- glm(PG_scaled_ind~VELPG_scaled_ind + Strategy + Strategy*VELPG_scaled_ind + Individual*Strategy + Individual*VELPG_scaled_ind + Individual*Strategy*VELPG_scaled_ind, data= all.data)
summary(full)

red1 <- glm(PG_scaled_ind~VELPG_scaled_ind + Strategy + Strategy*VELPG_scaled_ind + Individual*Strategy + Individual*VELPG_scaled_ind  , data= all.data)
summary(red1)

red2 <-  glm(PG_scaled_ind~VELPG_scaled_ind + Strategy + Strategy*VELPG_scaled_ind + Individual*Strategy , data= all.data)
summary(red2)

red3 <-  glm(PG_scaled_ind~VELPG_scaled_ind + Strategy + Strategy*VELPG_scaled_ind  , data= all.data)
summary(red3)

red4 <-  glm(PG_scaled_ind~VELPG_scaled_ind + Strategy  , data= all.data)
summary(red4)

red5 <-  glm(PG_scaled_ind~VELPG_scaled_ind  , data= all.data)
summary(red5)

mod.check <- compareGLM(full,red1,red2,red3,red4,red5)


# plot effect sizes for model 2
plot_model(red2, 
           axis.labels=c("VELPG", "Strategy", "Individual-LAUR02","Individual-LAUR03","Individual-LAUR04","Individual-LAUR05","VELPG*Strategy","Strategy*Individual-LAUR02","Strategy*Individual-LAUR03","Strategy*Individual-LAUR04","Strategy*Individual-LAUR05"),
           show.values=TRUE, show.p=TRUE,
           title="Effects of Strategy, Vecolity at peak gape, and Individual on Peak gape",value.size = ,dot.size = 2)+
  theme_sjplot()+
  scale_color_brewer(palette="Dark2")+
  ylim(-3,3)

# Get table with results 
tab_model(red2)

tab_model(red2, 
          show.re.var= TRUE, 
          pred.labels =c("(Intercept)","VELPG", "Strategy","Individual-LAUR02","Individual-LAUR03","Individual-LAUR04","Individual-LAUR05","VELPG*Strategy","Strategy*Individual-LAUR02","Strategy*Individual-LAUR03","Strategy*Individual-LAUR04","Strategy*Individual-LAUR05"),
          dv.labels= "Effects of Strategy, Velocity at peak gape, and Individual on Integration")


