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

    # TWO: Run the ICC model 
          # x : grouping variable 
          # y : measurement 
          # data : subset of original split dataset 
          # alpha : significance cut off 
          # CI.type : THD for unbalanced design

pg.icc <- ICCest(Individual, PG, data = pg, alpha = 0.05, CI.type = c("THD"))



