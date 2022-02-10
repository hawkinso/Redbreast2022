# Redbreast project 2022 
# Date: 01/26/2022 
# Author(s): Olivia H Hawkins, Dr. Callie Crawford 
# Goals: Repeatability measures and intraclass correlation 

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
data <- read.csv("RedBreast_2021.csv")

# Subset data that will be used in analysis ----
# Individual
all.data <- data %>%
  dplyr::select(Individual,SL,PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG,H_L_ratio,AI,ingested_volume,PPDiopen,timeatcapture,VELpreycapture)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

# Pull in small data and large data datasets 
small.data <- read.csv("SmallMouth_Redbreast2022.csv")
large.data <- read.csv("LargeMouth_Redbreast2022.csv")

# intraclass correlation ----
# In this case, we will use the ICC to investigate the variability of kinematics within an individual in the context of variation across all individuals 
# high ICC (close to 1) = high repeatability/similarity of observations 
# low ICC (close to 0) = observations are not similar 
# below 0.5: poor, 0.5-0.75: moderate, 0.75-0.90: good, above 0.9: excellent 

#### Making dataframes and calculating ICC for each variable 

# STEPS: 
    # ONE: Subset the data to where it only has the kinematic variable and Individuals
pg <- small.data %>%
 select(Individual,PG)

pg.small<- small.data %>% select(Individual,PG)
tto.small<- small.data %>% select(Individual,TTO)
ttc.small<- small.data %>% select(Individual,TTC)
pprot.small<- small.data %>% select(Individual,PPROT)
pprotvel.small<- small.data %>% select(Individual,PPROTVEL)
tpprot.small<- small.data %>% select(Individual,tPPROT)
velpg.small<- small.data %>% select(Individual,VELPG)
maxvel.small<- small.data %>% select(Individual,maxVEL)
tmaxvel.small<- small.data %>% select(Individual,tmaxVEL)
accpg.small<- small.data %>% select(Individual,ACCPG)
ratio.small<- small.data %>% select(Individual,H_L_ratio)
ai.small<- small.data %>% select(Individual,AI)
vol.small<- small.data %>% select(Individual,ingested_volume)
ppd.small<- small.data %>% select(Individual,PPDiopen)
time.small<- small.data %>% select(Individual,timeatcapture)
velpc.small<- small.data %>% select(Individual,VELpreycapture)


pg.large<- large.data %>% select(Individual,PG)
tto.large<- large.data %>% select(Individual,TTO)
ttc.large<- large.data %>% select(Individual,TTC)
pprot.large<- large.data %>% select(Individual,PPROT)
pprotvel.large<- large.data %>% select(Individual,PPROTVEL)
tpprot.large<- large.data %>% select(Individual,tPPROT)
velpg.large<- large.data %>% select(Individual,VELPG)
maxvel.large<- large.data %>% select(Individual,maxVEL)
tmaxvel.large<- large.data %>% select(Individual,tmaxVEL)
accpg.large<- large.data %>% select(Individual,ACCPG)
ratio.large<- large.data %>% select(Individual,H_L_ratio)
ai.large<- large.data %>% select(Individual,AI)
vol.large<- large.data %>% select(Individual,ingested_volume)
ppd.large<- large.data %>% select(Individual,PPDiopen)
time.large<- large.data %>% select(Individual,timeatcapture)
velpc.large<- large.data %>% select(Individual,VELpreycapture)

pg<- data %>% select(Individual,PG)
tto<- data %>% select(Individual,TTO)
ttc<- data %>% select(Individual,TTC)
pprot<- data %>% select(Individual,PPROT)
pprotvel<- data %>% select(Individual,PPROTVEL)
tpprot<- data %>% select(Individual,tPPROT)
velpg<- data %>% select(Individual,VELPG)
maxvel<- data %>% select(Individual,maxVEL)
tmaxvel<- data %>% select(Individual,tmaxVEL)
accpg<- data %>% select(Individual,ACCPG)
ratio<- data %>% select(Individual,H_L_ratio)
ai<- data %>% select(Individual,AI)
vol<- data %>% select(Individual,ingested_volume)
ppd<- data %>% select(Individual,PPDiopen)
time<- data %>% select(Individual,timeatcapture)
velpc<- data %>% select(Individual,VELpreycapture)


# TWO: Run the ICC model 
# x : grouping variable 
# y : measurement 
# data : subset of original split dataset 
# alpha : significance cut off 
# CI.type : THD for unbalanced design

pg.small.icc <- ICCest(Individual, PG, data = pg.small, alpha = 0.05, CI.type = c("THD"))
tto.small.icc <- ICCest(Individual, TTO, data = tto.small, alpha = 0.05, CI.type = c("THD"))
ttc.small.icc <- ICCest(Individual, TTC, data = ttc.small, alpha = 0.05, CI.type = c("THD"))
pprot.small.icc <- ICCest(Individual, PPROT, data = pprot.small, alpha = 0.05, CI.type = c("THD"))
pprotvel.small.icc <- ICCest(Individual, PPROTVEL, data = pprotvel.small, alpha = 0.05, CI.type = c("THD"))
tpprot.small.icc <- ICCest(Individual, tPPROT, data = tpprot.small, alpha = 0.05, CI.type = c("THD"))
velpg.small.icc <- ICCest(Individual, VELPG, data = velpg.small, alpha = 0.05, CI.type = c("THD"))
maxvel.small.icc <- ICCest(Individual, maxVEL, data = maxvel.small, alpha = 0.05, CI.type = c("THD"))
tmaxvel.small.icc <- ICCest(Individual, tmaxVEL, data = tmaxvel.small, alpha = 0.05, CI.type = c("THD"))
accpg.small.icc <- ICCest(Individual, ACCPG, data = accpg.small, alpha = 0.05, CI.type = c("THD"))
ratio.small.icc <- ICCest(Individual, H_L_ratio, data = ratio.small, alpha = 0.05, CI.type = c("THD"))
ai.small.icc <- ICCest(Individual, AI, data = ai.small, alpha = 0.05, CI.type = c("THD"))
vol.small.icc <- ICCest(Individual, ingested_volume, data = vol.small, alpha = 0.05, CI.type = c("THD"))
ppd.small.icc <- ICCest(Individual, PPDiopen, data = ppd.small, alpha = 0.05, CI.type = c("THD"))
time.small.icc <- ICCest(Individual, timeatcapture, data = time.small, alpha = 0.05, CI.type = c("THD"))
velpc.small.icc <- ICCest(Individual, VELpreycapture, data = velpc.small, alpha = 0.05, CI.type = c("THD"))


pg.large.icc <- ICCest(Individual, PG, data = pg.large, alpha = 0.05, CI.type = c("THD"))
tto.large.icc <- ICCest(Individual, TTO, data = tto.large, alpha = 0.05, CI.type = c("THD"))
ttc.large.icc <- ICCest(Individual, TTC, data = ttc.large, alpha = 0.05, CI.type = c("THD"))
pprot.large.icc <- ICCest(Individual, PPROT, data = pprot.large, alpha = 0.05, CI.type = c("THD"))
pprotvel.large.icc <- ICCest(Individual, PPROTVEL, data = pprotvel.large, alpha = 0.05, CI.type = c("THD"))
tpprot.large.icc <- ICCest(Individual, tPPROT, data = tpprot.large, alpha = 0.05, CI.type = c("THD"))
velpg.large.icc <- ICCest(Individual, VELPG, data = velpg.large, alpha = 0.05, CI.type = c("THD"))
maxvel.large.icc <- ICCest(Individual, maxVEL, data = maxvel.large, alpha = 0.05, CI.type = c("THD"))
tmaxvel.large.icc <- ICCest(Individual, tmaxVEL, data = tmaxvel.large, alpha = 0.05, CI.type = c("THD"))
accpg.large.icc <- ICCest(Individual, ACCPG, data = accpg.large, alpha = 0.05, CI.type = c("THD"))
ratio.large.icc <- ICCest(Individual, H_L_ratio, data = ratio.large, alpha = 0.05, CI.type = c("THD"))
ai.large.icc <- ICCest(Individual, AI, data = ai.large, alpha = 0.05, CI.type = c("THD"))
vol.large.icc <- ICCest(Individual, ingested_volume, data = vol.large, alpha = 0.05, CI.type = c("THD"))
ppd.large.icc <- ICCest(Individual, PPDiopen, data = ppd.large, alpha = 0.05, CI.type = c("THD"))
time.large.icc <- ICCest(Individual, timeatcapture, data = time.large, alpha = 0.05, CI.type = c("THD"))
velpc.large.icc <- ICCest(Individual, VELpreycapture, data = velpc.large, alpha = 0.05, CI.type = c("THD"))


pg.icc <- ICCest(Individual, PG, data = pg, alpha = 0.05, CI.type = c("THD"))
tto.icc <- ICCest(Individual, TTO, data = tto, alpha = 0.05, CI.type = c("THD"))
ttc.icc <- ICCest(Individual, TTC, data = ttc, alpha = 0.05, CI.type = c("THD"))
pprot.icc <- ICCest(Individual, PPROT, data = pprot, alpha = 0.05, CI.type = c("THD"))
pprotvel.icc <- ICCest(Individual, PPROTVEL, data = pprotvel, alpha = 0.05, CI.type = c("THD"))
tpprot.icc <- ICCest(Individual, tPPROT, data = tpprot, alpha = 0.05, CI.type = c("THD"))
velpg.icc <- ICCest(Individual, VELPG, data = velpg, alpha = 0.05, CI.type = c("THD"))
maxvel.icc <- ICCest(Individual, maxVEL, data = maxvel, alpha = 0.05, CI.type = c("THD"))
tmaxvel.icc <- ICCest(Individual, tmaxVEL, data = tmaxvel, alpha = 0.05, CI.type = c("THD"))
accpg.icc <- ICCest(Individual, ACCPG, data = accpg, alpha = 0.05, CI.type = c("THD"))
ratio.icc <- ICCest(Individual, H_L_ratio, data = ratio, alpha = 0.05, CI.type = c("THD"))
ai.icc <- ICCest(Individual, AI, data = ai, alpha = 0.05, CI.type = c("THD"))
vol.icc <- ICCest(Individual, ingested_volume, data = vol, alpha = 0.05, CI.type = c("THD"))
ppd.icc <- ICCest(Individual, PPDiopen, data = ppd, alpha = 0.05, CI.type = c("THD"))
time.icc <- ICCest(Individual, timeatcapture, data = time, alpha = 0.05, CI.type = c("THD"))
velpc.icc <- ICCest(Individual, VELpreycapture, data = velpc, alpha = 0.05, CI.type = c("THD"))



# THREE: Export ICC output to DataFrame

ICCResults <- function(Grouping, var){
  mod <- ICCest(Grouping, var, alpha = 0.05, CI.type = c("THD"))
  data.frame(mod$ICC, mod$LowerCI, mod$UpperCI, mod$N, mod$k, mod$varw, mod$vara)
}


pg.small.icc <- ICCResults(pg.small$Individual, pg.small$PG)
tto.small.icc <- ICCResults(tto.small$Individual, tto.small$TTO)
ttc.small.icc <- ICCResults(ttc.small$Individual, ttc.small$TTC)
pprot.small.icc <- ICCResults(pprot.small$Individual, pprot.small$PPROT)
pprotvel.small.icc <- ICCResults(pprotvel.small$Individual, pprotvel.small$PPROTVEL)
tpprot.small.icc <- ICCResults(tpprot.small$Individual, tpprot.small$tPPROT)
velpg.small.icc <- ICCResults(velpg.small$Individual, velpg.small$VELPG)
maxvel.small.icc <- ICCResults(maxvel.small$Individual, maxvel.small$maxVEL)
tmaxvel.small.icc <- ICCResults(tmaxvel.small$Individual, tmaxvel.small$tmaxVEL)
accpg.small.icc <- ICCResults(accpg.small$Individual, accpg.small$ACCPG)
ratio.small.icc <- ICCResults(ratio.small$Individual, ratio.small$H_L_ratio)
ai.small.icc <- ICCResults(ai.small$Individual, ai.small$AI)
vol.small.icc <- ICCResults(vol.small$Individual, vol.small$ingested_volume)
ppd.small.icc <- ICCResults(ppd.small$Individual, ppd.small$PPDiopen)
time.small.icc <- ICCResults(time.small$Individual, time.small$timeatcapture)
velpc.small.icc <- ICCResults(velpc.small$Individual, velpc.small$VELpreycapture)



pg.large.icc <- ICCResults(pg.large$Individual, pg.large$PG)
tto.large.icc <- ICCResults(tto.large$Individual, tto.large$TTO)
ttc.large.icc <- ICCResults(ttc.large$Individual, ttc.large$TTC)
pprot.large.icc <- ICCResults(pprot.large$Individual, pprot.large$PPROT)
pprotvel.large.icc <- ICCResults(pprotvel.large$Individual, pprotvel.large$PPROTVEL)
tpprot.large.icc <- ICCResults(tpprot.large$Individual, tpprot.large$tPPROT)
velpg.large.icc <- ICCResults(velpg.large$Individual, velpg.large$VELPG)
maxvel.large.icc <- ICCResults(maxvel.large$Individual, maxvel.large$maxVEL)
tmaxvel.large.icc <- ICCResults(tmaxvel.large$Individual, tmaxvel.large$tmaxVEL)
accpg.large.icc <- ICCResults(accpg.large$Individual, accpg.large$ACCPG)
ratio.large.icc <- ICCResults(ratio.large$Individual, ratio.large$H_L_ratio)
ai.large.icc <- ICCResults(ai.large$Individual, ai.large$AI)
vol.large.icc <- ICCResults(vol.large$Individual, vol.large$ingested_volume)
ppd.large.icc <- ICCResults(ppd.large$Individual, ppd.large$PPDiopen)
time.large.icc <- ICCResults(time.large$Individual, time.large$timeatcapture)
velpc.large.icc <- ICCResults(velpc.large$Individual, velpc.large$VELpreycapture)


pg.icc <- ICCResults(pg$Individual, pg$PG)
tto.icc <- ICCResults(tto$Individual, tto$TTO)
ttc.icc <- ICCResults(ttc$Individual, ttc$TTC)
pprot.icc <- ICCResults(pprot$Individual, pprot$PPROT)
pprotvel.icc <- ICCResults(pprotvel$Individual, pprotvel$PPROTVEL)
tpprot.icc <- ICCResults(tpprot$Individual, tpprot$tPPROT)
velpg.icc <- ICCResults(velpg$Individual, velpg$VELPG)
maxvel.icc <- ICCResults(maxvel$Individual, maxvel$maxVEL)
tmaxvel.icc <- ICCResults(tmaxvel$Individual, tmaxvel$tmaxVEL)
accpg.icc <- ICCResults(accpg$Individual, accpg$ACCPG)
ratio.icc <- ICCResults(ratio$Individual, ratio$H_L_ratio)
ai.icc <- ICCResults(ai$Individual, ai$AI)
vol.icc <- ICCResults(vol$Individual, vol$ingested_volume)
ppd.icc <- ICCResults(ppd$Individual, ppd$PPDiopen)
time.icc <- ICCResults(time$Individual, time$timeatcapture)
velpc.icc <- ICCResults(velpc$Individual, velpc$VELpreycapture)



#Creating DataFrames

ICCResults_DF_large <- data.frame(rbind(pg.large.icc, tto.large.icc, ttc.large.icc, pprot.large.icc, 
                                        pprotvel.large.icc, tpprot.large.icc, velpg.large.icc, 
                                        maxvel.large.icc, tmaxvel.large.icc, accpg.large.icc, 
                                        ratio.large.icc, ai.large.icc, vol.large.icc, ppd.large.icc, 
                                        time.large.icc, velpc.large.icc))

ICCResults_DF_large$variable <- c("PG","TTO","TTC","PPROT","PPROTVEL","tPPROT","VELPG",
                                  "maxVEL","tmaxVEL","ACCPG","H:L","AI", "ingested_volume", "PPDiopen",
                                  "timeatcapture", "VelPreyCapture")

ICCResults_DF_small <- data.frame(rbind(pg.small.icc, tto.small.icc, ttc.small.icc, pprot.small.icc, 
                                        pprotvel.small.icc, tpprot.small.icc, velpg.small.icc, 
                                        maxvel.small.icc, tmaxvel.small.icc, accpg.small.icc, 
                                        ratio.small.icc, ai.small.icc, vol.small.icc, ppd.small.icc, 
                                        time.small.icc, velpc.small.icc))

ICCResults_DF_small$variable <- c("PG","TTO","TTC","PPROT","PPROTVEL","tPPROT","VELPG",
                                  "maxVEL","tmaxVEL","ACCPG","H:L","AI", "ingested_volume", "PPDiopen",
                                  "timeatcapture", "VelPreyCapture")


ICCResults_DF_all <- data.frame(rbind(pg.icc, tto.icc, ttc.icc, pprot.icc, 
                                      pprotvel.icc, tpprot.icc, velpg.icc, 
                                      maxvel.icc, tmaxvel.icc, accpg.icc, 
                                      ratio.icc, ai.icc, vol.icc, ppd.icc, 
                                      time.icc, velpc.icc))

ICCResults_DF_all$variable <- c("PG","TTO","TTC","PPROT","PPROTVEL","tPPROT","VELPG",
                                "maxVEL","tmaxVEL","ACCPG","H:L","AI", "ingested_volume", "PPDiopen",
                                "timeatcapture", "VelPreyCapture")





# Write to a csv file 
#NOTE: Rows are doubled because of CI calculations. Variables will also be doubled. 
# Each variable technically gets two rows 
write.csv(ICCResults_DF_large,"ICCResults_large_redbreast_2022.csv")
write.csv(ICCResults_DF_small,"ICCResults_small_redbreast_2022.csv")
write.csv(ICCResults_DF_all,"ICCResults_all_redbreast_2022.csv")


ICC_Large <- read.csv("ICCResults_large_redbreast_2022.csv")
ICC_small <- read.csv("ICCResults_small_redbreast_2022.csv")
ICC_all <- read.csv("ICCResults_all_redbreast_2022.csv")


ICC_comp <- read.csv("ICCResults_comparison_redbreast_2022.csv")

ICC_comp$Category <- factor(ICC_comp$Category,levels =c("Feeding","Locomotion","Accuracy"))

#Plotting ICC + Upper/Lower CI

var_break <- c("PG", "TTO", "TTC", "PPROT", "PPROTVEL", "tPPROT", "VELPG", 
               "maxVEL", "tmaxVEL", "ACCPG", "timeatcapture", "H:L", 
               "AI", "ingested_volume", "PPDiopen", "VelPreyCapture")

ggplot(ICC_comp, aes(x=variable, y=ICC, group= Gape, color=Gape))+
  geom_point(aes(size=2))+
  scale_color_brewer(palette = "Dark2")+
  scale_x_discrete(limits = var_break)+
  geom_errorbar(aes(ymin=LowerCI, ymax=UpperCI), width=0.5, size=1)+
  geom_hline(yintercept = 0.7, linetype=2)+
  facet_grid(Gape~ .) +
  theme_cowplot()


ggplot(ICC_comp, aes(x=variable, y=ICC, group= Gape, color=Category))+
  geom_point(aes(size=2))+
  scale_color_brewer(palette = "Dark2")+
  scale_x_discrete(limits = var_break)+
  geom_errorbar(aes(ymin=LowerCI, ymax=UpperCI), width=0.5, size=1)+
  geom_hline(yintercept = 0.7, linetype=2)+
  facet_grid(Gape~.) +
  theme_cowplot()


ggplot(ICC_comp, aes(x=variable, y=ICC, group= Gape, color=Category))+
  geom_point(aes(size=2))+
  scale_color_brewer(palette = "Dark2")+
  geom_errorbar(aes(ymin=LowerCI, ymax=UpperCI), width=0.5, size=1)+
  geom_hline(yintercept = 0.7, linetype=2)+
  facet_grid(Gape~Category, scales="free_x") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        legend.position= "None")+
  xlab("Variable")+
  theme(axis.title.x=element_text(face="bold"),
        axis.title.y = element_text(face="bold"))


