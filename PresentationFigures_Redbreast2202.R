# Redbreast project 2022 
# Date: 02/08/2022 
# Author(s): Olivia H Hawkins, Dr. Callie Crawford 
# Goals: Making figures

# libraries 
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

# Presentation figures ----

## Multi-panel plot 

# Accuracy index density histo of all observations, small mouth, and large mouth 
AI_all <- ggplot(data=all.data, aes(x=AI ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  ylab("Density")+
  xlab("Accuracy Index")+
  xlim(0,1)+ 
  theme(axis.title.x=element_text(face="bold",size=14),
        axis.title.y=element_text(face="bold", size=14),
        legend.title = element_text(face="bold"),
        legend.position="top")+
  ylim(0,8)+
ggtitle("All observations")

AI_small <- ggplot(data=small.data, aes(x=AI ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  ylab("Density")+
  xlab("Accuracy Index")+
  xlim(0,1)+ 
  theme(axis.title.x=element_text(face="bold",size=14),
        axis.title.y=element_text(face="bold", size=14))+
  ylim(0,8)+
  ggtitle("Small mouth")

AI_large <- ggplot(data=large.data, aes(x=AI ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  ylab("Density")+
  xlab("Accuracy Index")+
  xlim(0,1)+ 
  theme(axis.title.x=element_text(face="bold",size=14),
        axis.title.y=element_text(face="bold", size=14))+
          ggtitle("Large mouth")+
  ylim(0,8)

ggarrange(AI_all,AI_small,AI_large,
          labels=c("A","B","C"),
          ncol=3,nrow=1,
          common.legend = TRUE)

# Boxplots for each individual for feeding and swimming variables 
# Subset feeding and locomotion data sets 

Feeding <- all.data %>% 
  select(Individual,PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,Strategy) %>%
  group_by(Individual)
  
Locomotion <- all.data %>% 
  select(Individual,VELPG,maxVEL,tmaxVEL,ACCPG,Strategy) %>%
  group_by(Individual)

# feeding 
tto.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=TTO,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Time to mouth opening (ms)")+
  geom_hline(yintercept=0,linetype=2)
  
ttc.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=TTC,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14))+
  ylab("Time to mouth closing (ms)")+
  geom_hline(yintercept=0,linetype=2)

pprot.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=PPROT,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        ,legend.position = "none")+
  ylab("Peak Protrusion (cm)")

pprotvel.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=PPROTVEL,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Protrusion velocity (cm/s)")

tpprot.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=tPPROT,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Timing of peak protrusion (ms)")+
  geom_hline(yintercept=0,linetype=2)

# locomotion 
velpg.plot <- ggplot()+
  geom_boxplot(Locomotion, mapping=aes(x=Individual,y=VELPG,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Velocity at peak gape (cm/s")

maxvel.plot <- ggplot()+
  geom_boxplot(Locomotion, mapping=aes(x=Individual,y=maxVEL,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Maximum velocity (cm/s)")

tmaxVEL.plot <- ggplot()+
  geom_boxplot(Locomotion, mapping=aes(x=Individual,y=tmaxVEL,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Timing of maximum velocity (ms)")+
  geom_hline(yintercept=0,linetype=2)+
  ylim(-100,100)

accpg.plot <- ggplot()+
  geom_boxplot(Locomotion, mapping=aes(x=Individual,y=ACCPG,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  ylab(bquote(bold('Acceleration at peak gape'~(cm/s^2))))+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14))

# accuracy 
ggplot()+
  geom_boxplot(all.data, mapping=aes(x=Individual,y=ingested_volume,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  ylab(bquote(bold('Ingested volume'~(cm^3))))+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))

ggplot()+
  geom_boxplot(all.data, mapping=aes(x=Individual,y=H_L_ratio,fill=Strategy))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  ylab("Height to length ratio")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))+
  ylim(0.5,1.3)

# Make combined feeding figure
feed <- ggarrange(pprot.plot,pprotvel.plot,tpprot.plot,
          labels = c("A", "B","C"),
          ncol = 3, nrow = 1,
          common.legend = FALSE)
annotate_figure(feed, top = text_grob("Feeding", 
                                      color = "black", face = "bold", size = 20))

loco <- ggarrange(velpg.plot,maxvel.plot,tmaxVEL.plot,
                  labels = c("D", "E","F"),
                  ncol = 3, nrow = 1)

annotate_figure(loco, top = text_grob("Locomotion", 
                                      color = "black", face = "bold", size = 20))


