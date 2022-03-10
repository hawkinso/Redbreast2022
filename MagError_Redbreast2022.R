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

### Info about variables----
# PG: 95% of peak gape when mouth is open (height) in cm 
# TTO: Timing of mouth opening in relation to PG in ms 
# TTC: Timing of mouth closing in relation to PG in ms 
# PPROT: Peak protrusion (eye to anterior tip of upper jaw) in cm 
# PPROTVEL: Speed of protrusion in cm/s
# tPPROT: Timing of peak protusion in relation to peak gape in cm 
# VELPG: velocity of the body at the time of peak gape in cm/s 
# maxVEL: maximum velocity of the body through the digitized frames in cm/s
# tmaxVEL: timing of maximum velocity in relation to peak gape in ms 
# ACCPG: acceleration at the time of peak gape in cm/s^2 
# H_L_ratio: height to length ratio of the ingested volume 
# AI: accuracy index 
# ingested_volume: volume of ingested water during suction feeding (cm^3) 
# PPDiopen: predator prey distance at mouth opening in cm 
# timeatcapture: timing of prey capture relative to peak gape in ms 
# VELpreycapture: velocity of the body at the time of prey capture in cm/s

# Read in data ----
data <- read.csv("Redbreast2022_FINAL.csv")
measures <- read.csv("MorphologyMeasurements_Redbreast_2022_averaged.csv")

# Reformat data ----
data_merged <- merge(data, measures, by= "Individual")
data_merged$Gape_prop <- data_merged$PG/data_merged$Gape_height

data_merged$SL_prop <- data_merged$SL.x/data_merged$SL.y

# Plot ----
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
  geom_point(size= 2) +
  geom_smooth(method=lm , color="black", size = 1.5, fill="#949494", se=TRUE)+
  geom_abline(intercept=0, slope=1, color= "black", linetype = "dashed", size= 1.5)+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Measured Standard Length (cm)")+
  ylab("Video Standard Length (cm)")+
  theme_cowplot()+
  theme(
    axis.title = element_text( color="black", size=12),
    axis.line = element_line(size = 1, colour = "black"),
    axis.text = element_text( color="black", size=10),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank())

VELPG_comparison <- ggplot(data=data_merged, aes(x=SL.y, y=VELPG ,colour=Individual,  fill=Individual)) +
  geom_point(size= 2) +
  geom_smooth(method=lm , color="black", size = 1.5, fill="#949494", se=TRUE)+
  geom_abline(intercept=0, slope=1, color= "black", linetype = "dashed", size= 1.5)+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Measured Standard Length (cm)")+
  ylab("Velocity at Peak Gape (cm/s)")+
  theme_cowplot()+
  theme(
    axis.title = element_text( color="black", size=12),
    axis.line = element_line(size = 1, colour = "black"),
    axis.text = element_text( color="black", size=10),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(face="bold"),
    legend.text = element_text(size=12))

PG_comparison <- ggplot(data=data_merged, aes(x=SL.y, y=PG, group = Strategy, colour=Individual, fill=Individual)) +
  geom_point(size= 2) +
  geom_smooth(method=lm , color="black", size = 1.5, fill="#949494", se=TRUE)+
  geom_abline(intercept=0, slope=1, color= "black", linetype = "dashed", size= 1.5)+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Measured Standard Length (cm)")+
  ylab("Video Peak Gape (cm)")+
  theme_cowplot()+
  theme(
    axis.title = element_text( color="black", size=12),
    axis.line = element_line(size = 1, colour = "black"),
    axis.text = element_text( color="black", size=10))

plot_grid(SL_comparisons, VELPG_comparison, PG_comparison, align= "v",
          labels = "AUTO", ncol = 1,rel_heights = c(1,1,1))

## Convert the rest of the variables to make a new data set to use for analysis that cancels out the magnification error 
