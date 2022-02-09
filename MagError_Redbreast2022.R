# Redbreast project 2022 
# Date: 02/09/2022
# Author(s): Dr. Callie Crawford
# Goals: Assess magnification error 

# Git hub repository 
browseURL("https://github.com/hawkinso/Redbreast2022.git")

# Load in libraries
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


data <- read.csv("RedBreast_2021.csv")
measures <- read.csv("MorphologyMeasurements_Redbreast_2022_averaged.csv")

data_merged <- merge(data, measures, by= "Individual")
data_merged$Gape_prop <- data_merged$PG/data_merged$Gape_height

data_merged$SL_prop <- data_merged$SL.x/data_merged$SL.y
data_merged$Strategy <- ifelse(data_merged$Gape_prop < 0.5, "Small mouth","Large mouth")


ggplot(data=data_merged, aes(x=PG, y=Gape_prop ,colour=Individual, fill=Individual)) +
  geom_point() +
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Peak Gape")+
  ylab("Proportion of Max Gape")


ggplot(data=data_merged, aes(x=SL.y, y=Gape_height ,colour=Individual, fill=Individual)) +
  geom_point() +
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Standard Length")+
  ylab("Max Anatomical Gape")


ggplot(data=data_merged, aes(x=SL.y, y=SL_prop ,colour=Individual, fill=Individual)) +
  geom_point() +
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Standard Length")+
  ylab("Video Standard Length as Proportion of Measured Standard Length")


SL_comparisons <- ggplot(data=data_merged, aes(x=SL.y, y=SL.x ,colour=Individual, fill=Individual)) +
  geom_point(size= 3) +
  geom_smooth(method=lm , color="black", size = 1.5, fill="#949494", se=TRUE)+
  geom_abline(intercept=0, slope=1, color= "black", linetype = "dashed", size= 1.5)+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Measured Standard Length (cm)")+
  ylab("Video Standard Length (cm)")+
  theme_cowplot()+
  theme(
    axis.title = element_text( color="black", size=13),
    axis.line = element_line(size = 1, colour = "black"),
    axis.text = element_text( color="black", size=10),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank())

VELPG_comparison <- ggplot(data=data_merged, aes(x=SL.y, y=VELPG ,colour=Individual,  fill=Individual)) +
  geom_point(size= 3) +
  geom_smooth(method=lm , color="black", size = 1.5, fill="#949494", se=TRUE)+
  geom_abline(intercept=0, slope=1, color= "black", linetype = "dashed", size= 1.5)+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Measured Standard Length (cm)")+
  ylab("Velocity at Peak Gape (cm/s)")+
  theme_cowplot()+
  theme(
    axis.title = element_text( color="black", size=13),
    axis.line = element_line(size = 1, colour = "black"),
    axis.text = element_text( color="black", size=10),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank())

PG_comparison <- ggplot(data=data_merged, aes(x=SL.y, y=PG, group = Strategy, colour=Individual, fill=Individual)) +
  geom_point(size= 3) +
  geom_smooth(method=lm , color="black", size = 1.5, fill="#949494", se=TRUE)+
  geom_abline(intercept=0, slope=1, color= "black", linetype = "dashed", size= 1.5)+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Measured Standard Length (cm)")+
  ylab("Video Peak Gape (cm)")+
  theme_cowplot()+
  theme(
    axis.title = element_text( color="black", size=13),
    axis.line = element_line(size = 1, colour = "black"),
    axis.text = element_text( color="black", size=10))

plot_grid(SL_comparisons, VELPG_comparison, PG_comparison, align= "v",
          labels = "AUTO", ncol = 1, rel_heights = c(1,1,1))
