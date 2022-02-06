# Redbreast project 2022 
# Date: 01/24/2022
# Author(s): Olivia H Hawkins 
# Goals:Split the data into large mouth and small mouth datasets

# Info about code: Upon initial investigation of the data, it was discovered that there were two peaks for peak gape, a common 
# metric used in determining integration. Due to the dual peak nature of the data, normality assumptions are violated 
# and make analyses complicated. We choose to split the data as it appears two strategies are being used by most of the individuals 

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

# Read in data ----
# This data sheet is available at: browse.URL("")
data <- read.csv("RedBreast_2021.csv")

# Subset data that will be used in analysis ----
# Individual
all.data <- data %>%
  dplyr::select(Individual,SL,PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG,H_L_ratio,AI,ingested_volume,PPDiopen,timeatcapture,VELpreycapture)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

# center and scale by body size BY INDIVIDUAL ----
# subset by fish 
fish.1.scale <- all.data %>%
  select(Individual,PG,VELPG)%>%
  filter(Individual=="LAUR01")

fish.1.scale$PG_scale_ind <- scale(fish.1.scale$PG,center = T,scale = T)
fish.1.scale$VELPG_scale_ind <- scale(fish.1.scale$VELPG,center = T,scale = T)

fish.2.scale <- all.data %>%
  select(Individual,PG,VELPG)%>%
  filter(Individual=="LAUR02")

fish.2.scale$PG_scale_ind <- scale(fish.2.scale$PG,center = T,scale = T)
fish.2.scale$VELPG_scale_ind <- scale(fish.2.scale$VELPG,center = T,scale = T)

fish.3.scale <- all.data %>%
  select(Individual,PG,VELPG)%>%
  filter(Individual=="LAUR03")

fish.3.scale$PG_scale_ind <- scale(fish.3.scale$PG,center = T,scale = T)
fish.3.scale$VELPG_scale_ind <- scale(fish.3.scale$VELPG,center = T,scale = T)

fish.4.scale <- all.data %>%
  select(Individual,PG,VELPG)%>%
  filter(Individual=="LAUR04")

fish.4.scale$PG_scale_ind <- scale(fish.4.scale$PG,center = T,scale = T)
fish.4.scale$VELPG_scale_ind <- scale(fish.4.scale$VELPG,center = T,scale = T)

fish.5.scale <- all.data %>%
  select(Individual,PG,VELPG)%>%
  filter(Individual=="LAUR05")

fish.5.scale$PG_scale_ind <- scale(fish.5.scale$PG,center = T,scale = T)
fish.5.scale$VELPG_scale_ind <- scale(fish.5.scale$VELPG,center = T,scale = T)

# Make data frame 
all.data.ind <- data.frame(rbind(fish.1.scale,fish.2.scale,fish.3.scale,fish.4.scale,fish.5.scale))

# Use ifelse to make a new column that separates the "small mouth" vs "large mouth" approach. This column in called "Strategy"
all.data$Strategy <- ifelse(all.data.ind$PG_scale_ind < -0.2, "Small mouth","Large mouth")

# Add scaled data to the overall data frame 
all.data$PG_scaled_ind <- all.data.ind$PG_scale_ind
all.data$VELPG_scaled_ind <- all.data.ind$VELPG_scale_ind

# Check the split 
small.mouth <- all.data %>%
  select(Individual,VELPG_scaled_ind,PG_scaled_ind,Strategy)%>%
  filter(Strategy=="Small mouth")

large.mouth <- all.data %>%
  select(Individual,VELPG_scaled_ind,PG_scaled_ind,Strategy)%>%
  filter(Strategy=="Large mouth")

# Apply split to whole data set 

small.all.data <- all.data %>%
  filter(Strategy=="Small mouth")

large.all.data <- all.data %>%
  filter(Strategy=="Large mouth")

small.all.data <- data.frame(small.all.data)
large.all.data <- data.frame(large.all.data)


# write csv files 
write.csv(small.all.data,file = "SmallMouth_Redbreast2022.csv")
write.csv(large.all.data,file = "LargeMouth_Redbreast2022.csv")


# Visualize split 
# Check integration figure 
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

# Check distributions 
ggplot(data=small.all.data, aes(x=PG ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Peak gape (cm)")

ggplot(data=large.all.data, aes(x=PG ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Peak gape (cm)")

### OUTCOMES ### 

# The data set is now split into "small mouth" and "large mouth" strategies. 
# We will use these separate datasets to investigate univariate difference across and within individuals 
# If we kept the data set together, we would have some variables with bimodal peaks and linear statistics 
  # would not be appropriate

# For subsequent scripts, the data sets will be as follows: 

# small mouth strategy: "SmallMouth_Redbreast_2022.csv"
# large mouth strategy: "LargeMouth_Redbreast_2022.csv" 

