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
  ylim(0,8)


# Boxplots for each individual for feeding and swimming variables 
# Subset feeding and locomotion data sets 

Feeding <- all.data %>% 
  select(Individual,PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,Gape_prop) %>%
  group_by(Individual)
  
Locomotion <- all.data %>% 
  select(Individual,VELPG,maxVEL,tmaxVEL,ACCPG) %>%
  group_by(Individual)

# feeding 

propgape.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=Gape_prop,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Proportion of peak gape")+
  stat_compare_means(method = "anova",label.y=1,label.x=4.5)

tto.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=TTO,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Time to mouth opening (ms)")+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)
  
ttc.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=TTC,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14))+
  ylab("Time to mouth closing (ms)")+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

pprot.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=PPROT,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Peak Protrusion (cm)")+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

pprotvel.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=PPROTVEL,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Protrusion velocity (cm/s)")+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

tpprot.plot <- ggplot()+
  geom_boxplot(Feeding, mapping=aes(x=Individual,y=tPPROT,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        legend.position = "none")+
  ylab("Timing of peak protrusion (ms)")+
  geom_hline(yintercept=0,linetype=2)+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

# locomotion 
velpg.plot <- ggplot()+
  geom_boxplot(Locomotion, mapping=aes(x=Individual,y=VELPG,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x=element_text(face="bold"),
        axis.text.y=element_text(face="bold"),
        legend.position = "none")+
  ylab("Velocity at peak gape (cm/s")+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

maxvel.plot <- ggplot()+
  geom_boxplot(Locomotion, mapping=aes(x=Individual,y=maxVEL,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x=element_text(face="bold"),
        axis.text.y=element_text(face="bold"),
        legend.position = "none")+
  ylab("Maximum velocity (cm/s)")+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

tmaxVEL.plot <- ggplot()+
  geom_boxplot(Locomotion, mapping=aes(x=Individual,y=tmaxVEL,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x=element_text(face="bold"),
        axis.text.y=element_text(face="bold"),
        legend.position = "none")+
  ylab("Timing of maximum velocity (ms)")+
  geom_hline(yintercept=0,linetype=2)+
  ylim(-100,100)+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

accpg.plot <- ggplot()+
  geom_boxplot(Locomotion, mapping=aes(x=Individual,y=ACCPG,fill=Indivdual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  ylab(bquote(bold('Acceleration at peak gape'~(cm/s^2))))+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14))+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

# accuracy 
ggplot()+
  geom_boxplot(all.data, mapping=aes(x=Individual,y=ingested_volume,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  ylab(bquote(bold('Ingested volume'~(cm^3))))+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14))+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

ggplot()+
  geom_boxplot(all.data, mapping=aes(x=Individual,y=H_L_ratio,fill=Individual))+
  scale_fill_brewer(palette="Dark2")+
  theme_classic()+
  ylab("Height to length ratio")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14))+
  ylim(0.5,1.3)+
  stat_compare_means(method = "anova",label.y=90,label.x=4.5)

# Make combined feeding figure
feed <- ggarrange(propgape.plot,pprot.plot,pprotvel.plot,tpprot.plot,
          labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2,
          common.legend = FALSE)
annotate_figure(feed, top = text_grob("Feeding", 
                                      color = "black", face = "bold", size = 20))

loco <- ggarrange(velpg.plot,tmaxVEL.plot,
                  labels = c("D", "E"),
                  ncol = 2, nrow = 1)

annotate_figure(loco, top = text_grob("Locomotion", 
                                      color = "black", face = "bold", size = 20))


# add everything 

ggarrange(propgape.plot,pprot.plot,pprotvel.plot,tpprot.plot,
          labels=c("A", "B","C","D"),
          common.legend=FALSE,
          nrow=1,
          ggarrange(velpg.plot,tmaxVEL.plot,
                    labels= c("D", "E"),
                    ncol=3,
          ggarrange(AI_all,
                    labels="F")))

## Try ridge plots to parse out histogram differences -----

ggplot(all.data, aes(x = Gape_prop, y = Individual)) +
  geom_density_ridges(aes(fill = Individual),scale = 1.3, quantile_lines=TRUE,
                      quantile_fun=function(x,...)median(x))+
  theme_classic() 

ggplot(all.data, aes(x = VELPG_scale_ind, y = Individual)) +
  geom_density_ridges(aes(fill = Individual),scale = 1.3, quantile_lines=TRUE,
                      quantile_fun=function(x,...)median(x))+
  theme_classic() 
