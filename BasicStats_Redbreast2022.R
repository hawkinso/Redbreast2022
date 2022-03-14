# Redbreast project 2022 
# Date: 11/23/2021, updated 01/12/2022
# Author(s): Olivia H Hawkins 
# Goals: clean and summarize data,statistical assumptions, transformations 

# Git hub repository 
browseURL("https://github.com/hawkinso/Redbreast2022.git")

# Info about data: 
# Sex: LAUR01 and LAUR04 are male, LAUR02, LAUR03, and LAUR05 are female 
# 

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

# Read in data 
# This data sheet is available at: browse.URL("")
data <- read.csv("Redbreast2022_MAG.csv")

# Subset data that will be used in analysis ----
# Individual
all.data <- data %>%
  dplyr::select(Individual,SL_mag,PG_mag,TTO,TTC,PPROT_mag,PPROTVEL_mag,tPPROT,VELPG_mag,maxVEL_mag,tmaxVEL,ACCPG_mag,H_L_ratio,AI,ingested_volume_mag,PPDiopen_mag,timeatcapture,VELpreycapture_mag)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual) 

all.data.shap <-  data %>%
  dplyr::select(Individual,PG_mag,TTO,TTC,PPROT_mag,PPROTVEL_mag,tPPROT,VELPG_mag,maxVEL_mag,tmaxVEL,ACCPG_mag,H_L_ratio,AI,ingested_volume_mag,PPDiopen_mag,timeatcapture,VELpreycapture_mag)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual) 


# Make sure data is stored as data frame 
as.data.frame(all.data)

# Check out data structure 
str(all.data)

# Get group means and sd for each individual ----
means <- all.data %>%
  get_summary_stats()

# Get standard length (mean +/- SD): 9.25 +/- 0.97 cm 
mean(all.data$SL_mag) #9.25
sd(all.data$SL_mag) # 0.97

# Write to .csv 
write_csv(means,file = "Redbreast_summarystatXindividual_2021.csv",append = FALSE)

# Check assumptions ----
# Normality, homogeneity of variance,independence of observations
# Independence of observations is not met as each individual is sampled 20 times 


# Check normality with Shapiro-Wilk test 
## By individuals 
sw <- ddply(.data=all.data.shap, .variables=c("Individual"),numcolwise(shapiro.test))
sw <- sw[-c(3:4,7:8,11:12,15:16,19:20),]

sw.results <- gather(data = sw,key = Variable, value=Results,2:17)

# Add column to show what the value is (W statistic and p value)
sw.results$Value <- rep(c("W statistic","p value"))

# Find the individual x variable combination that violates normality 
sw.results2 <- sw.results %>%
   group_by(Individual)%>%
    filter(Value=="p value")%>% 
  filter(Results<0.05)

# Export data 
write.csv(sw.results,"Shapiro.wilk_Redbreast2022.csv")

# Visualize normality by individual
ggqqplot(data=all.data, x = ("PG_mag"),
         color = "Individual",facet.by="Individual") # pretty good
ggqqplot(all.data, x = "TTO",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "TTC",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "PPROT_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "PPROTVEL_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "tPPROT",
         color = "Individual",facet.by = "Individual") # ok
ggqqplot(all.data, x = "VELPG_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "maxVEL_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "tmaxVEL",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "ACCPG_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "H_L_ratio",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "AI",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "ingested_volume_mag",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "PPDiopen_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "timeatcapture",
         color = "Individual",facet.by = "Individual") # pretty good 
ggqqplot(all.data, x = "VELpreycapture_mag",
         color = "Individual",facet.by = "Individual") # pretty good

# Identify outliers 

# Individual 1 
Fish.1 <- all.data %>%
  filter(Individual=="LAUR01")

Fish.1 %>% select(PG_mag) %>% identify_outliers() 
Fish.1 %>% select(TTO) %>% identify_outliers() 
Fish.1 %>% select(TTC) %>% identify_outliers()
Fish.1 %>% select(PPROT_mag) %>% identify_outliers()
Fish.1 %>% select(PPROTVEL_mag) %>% identify_outliers()  
Fish.1 %>% select(tPPROT) %>% identify_outliers() # one extreme 
Fish.1 %>% select(VELPG_mag) %>% identify_outliers()
Fish.1 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.1 %>% select(tmaxVEL) %>% identify_outliers() 
Fish.1 %>% select(ACCPG_mag) %>% identify_outliers() # one extreme 
Fish.1 %>% select(H_L_ratio) %>% identify_outliers()
Fish.1 %>% select(AI) %>% identify_outliers()
Fish.1 %>% select(ingested_volume_mag) %>% identify_outliers()
Fish.1 %>% select(PPDiopen_mag) %>% identify_outliers()
Fish.1 %>% select(timeatcapture) %>% identify_outliers()
Fish.1 %>% select(VELpreycapture_mag) %>% identify_outliers()


# Individual 2 
Fish.2 <- all.data %>%
  filter(Individual=="LAUR02")

Fish.2 %>% select(PG_mag) %>% identify_outliers() 
Fish.2 %>% select(TTO) %>% identify_outliers() 
Fish.2 %>% select(TTC) %>% identify_outliers()
Fish.2 %>% select(PPROT_mag) %>% identify_outliers() # one extreme
Fish.2 %>% select(PPROTVEL_mag) %>% identify_outliers() 
Fish.2 %>% select(tPPROT) %>% identify_outliers() 
Fish.2 %>% select(VELPG_mag) %>% identify_outliers()
Fish.2 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.2 %>% select(tmaxVEL) %>% identify_outliers() 
Fish.2 %>% select(ACCPG_mag) %>% identify_outliers() # one extreme 
Fish.2 %>% select(H_L_ratio) %>% identify_outliers()
Fish.2 %>% select(AI) %>% identify_outliers()
Fish.2 %>% select(ingested_volume_mag) %>% identify_outliers()
Fish.2 %>% select(PPDiopen_mag) %>% identify_outliers() 
Fish.2 %>% select(timeatcapture) %>% identify_outliers()
Fish.2 %>% select(VELpreycapture_mag) %>% identify_outliers()


# Individual 3
Fish.3 <- all.data %>%
  filter(Individual=="LAUR03")

Fish.3 %>% select(PG_mag) %>% identify_outliers()  
Fish.3 %>% select(TTO) %>% identify_outliers() 
Fish.3 %>% select(TTC) %>% identify_outliers()
Fish.3 %>% select(PPROT_mag) %>% identify_outliers() 
Fish.3 %>% select(PPROTVEL_mag) %>% identify_outliers() 
Fish.3 %>% select(tPPROT) %>% identify_outliers() 
Fish.3 %>% select(VELPG_mag) %>% identify_outliers()
Fish.3 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.3 %>% select(tmaxVEL) %>% identify_outliers() 
Fish.3 %>% select(ACCPG_mag) %>% identify_outliers() 
Fish.3 %>% select(H_L_ratio) %>% identify_outliers()
Fish.3 %>% select(AI) %>% identify_outliers()
Fish.3 %>% select(ingested_volume_mag) %>% identify_outliers() 
Fish.3 %>% select(PPDiopen_mag) %>% identify_outliers() 
Fish.3 %>% select(timeatcapture) %>% identify_outliers()
Fish.3 %>% select(VELpreycapture_mag) %>% identify_outliers()

# Individual 4 
Fish.4 <- all.data %>%
  filter(Individual=="LAUR04")

Fish.4 %>% select(PG_mag) %>% identify_outliers()  
Fish.4 %>% select(TTO) %>% identify_outliers() 
Fish.4 %>% select(TTC) %>% identify_outliers()
Fish.4 %>% select(PPROT_mag) %>% identify_outliers() 
Fish.4 %>% select(PPROTVEL_mag) %>% identify_outliers() 
Fish.4 %>% select(tPPROT) %>% identify_outliers()  
Fish.4 %>% select(VELPG_mag) %>% identify_outliers()
Fish.4 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.4 %>% select(tmaxVEL) %>% identify_outliers()  
Fish.4 %>% select(ACCPG_mag) %>% identify_outliers() # one extreme
Fish.4 %>% select(H_L_ratio) %>% identify_outliers()
Fish.4 %>% select(AI) %>% identify_outliers()
Fish.4 %>% select(ingested_volume_mag) %>% identify_outliers()
Fish.4 %>% select(PPDiopen_mag) %>% identify_outliers()  
Fish.4 %>% select(timeatcapture) %>% identify_outliers()
Fish.4 %>% select(VELpreycapture_mag) %>% identify_outliers()

# Individual 5 
Fish.5 <- all.data %>%
  filter(Individual=="LAUR05")

Fish.5 %>% select(PG_mag) %>% identify_outliers()  
Fish.5 %>% select(TTO) %>% identify_outliers() 
Fish.5 %>% select(TTC) %>% identify_outliers()
Fish.5 %>% select(PPROT_mag) %>% identify_outliers() 
Fish.5 %>% select(PPROTVEL_mag) %>% identify_outliers() 
Fish.5 %>% select(tPPROT) %>% identify_outliers() 
Fish.5 %>% select(VELPG_mag) %>% identify_outliers()
Fish.5 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.5 %>% select(tmaxVEL) %>% identify_outliers()  
Fish.5 %>% select(ACCPG_mag) %>% identify_outliers() 
Fish.5 %>% select(H_L_ratio) %>% identify_outliers()
Fish.5 %>% select(AI) %>% identify_outliers()
Fish.5 %>% select(ingested_volume_mag) %>% identify_outliers()
Fish.5 %>% select(PPDiopen_mag) %>% identify_outliers() 
Fish.5 %>% select(timeatcapture) %>% identify_outliers()
Fish.5 %>% select(VELpreycapture_mag) %>% identify_outliers()

# Check homogeneity of variance with Levene's test across all individuals 
leveneTest(all.data$PG_mag~all.data$Individual) # 0.15
leveneTest(all.data$TTO~all.data$Individual) # 0.26
leveneTest(all.data$TTC~all.data$Individual) # 0.2
leveneTest(all.data$PPROT_mag~all.data$Individual) # 0.13
leveneTest(all.data$PPROTVEL_mag~all.data$Individual) # p < 0.0001
leveneTest(all.data$tPPROT~all.data$Individual) # p = 0.25
leveneTest(all.data$VELPG_mag~all.data$Individual) # p < 0.0001
leveneTest(all.data$maxVEL_mag~all.data$Individual) # p = 0.0001
leveneTest(all.data$tmaxVEL~all.data$Individual) # 0.4 
leveneTest(all.data$ACCPG_mag~all.data$Individual) # p = 0.0002
leveneTest(all.data$H_L_ratio~all.data$Individual)#  p =0.23
leveneTest(all.data$AI~all.data$Individual) #p = 0.64
leveneTest(all.data$ingested_volume_mag~all.data$Individual) # P < 0.0001
leveneTest(all.data$PPDiopen_mag~all.data$Individual) # p = 0.04
leveneTest(all.data$timeatcapture~all.data$Individual) # p = 0.54
leveneTest(all.data$VELpreycapture_mag~all.data$Individual) # p = 0.0001


# Check that size is similar among individuals ----
# We want to be sure that size does not influence any variables 
ggboxplot(all.data, x = "Individual", y = "SL", add = "point")

# Check assumptions
# Outliers
SL <- all.data %>%  # no extreme outliers 
  group_by(Individual) %>%
  select(SL) %>%
  identify_outliers()

# Normality 
shapiro.test(all.data$SL) # p = 0.01 
ggqqplot(all.data$SL) # looks ok 

# General linear mixed model 
# Some variables are influenced by size... we will need to scale variables by standard length
PGmod.SL <- lmer(PG~SL+(1|Individual),data=all.data)
summary(PGmod.SL) # p < 0.0001

TTOmod.SL <- lmer(TTO~SL+(1|Individual),data=all.data)
summary(TTOmod.SL) # p =0.02

TTCmod.SL <- lmer(TTC~SL+(1|Individual),data=all.data)
summary(TTCmod.SL) # 0.82

PPROTmod.SL <- lmer(PPROT~SL+(1|Individual),data=all.data)
summary(PPROTmod.SL)  # p = 0.004


PPROTVELmod.SL <- lmer(PPROTVEL~SL+(1|Individual),data=all.data)
summary(PPROTVELmod.SL) # p =0.02

tPPROTmod.SL <- lmer(tPPROT~SL+(1|Individual),data=all.data)
summary(tPPROTmod.SL) # p = 0.77

VELPGmod.SL <- lmer(VELPG~SL+(1|Individual),data=all.data)
summary(VELPGmod.SL) # = 0.006

maxVELmod.SL <- lmer(maxVEL~SL+(1|Individual),data=all.data)
summary(maxVELmod.SL) # p = 0.01

tmaxVELmod.SL <- lmer(tmaxVEL~SL+(1|Individual),data=all.data)
summary(tmaxVELmod.SL) # p = 0.18

ACCPGmod.SL <- lmer(ACCPG~SL+(1|Individual),data=all.data)
summary(ACCPGmod.SL) # p = 0.61

HLmod.SL <- lmer(H_L_ratio~SL+(1|Individual),data=all.data)
summary(HLmod.SL) # p = 0.16

AImod.SL <- lmer(AI~SL+(1|Individual),data=all.data)
summary(AImod.SL) # p = 0.7

ingestedmod.SL <- lmer(ingested_volume~SL+(1|Individual),data=all.data)
summary(ingestedmod.SL) # p < 0.001

PPDiopenmod.SL <- lmer(PPDiopen~SL+(1|Individual),data=all.data)
summary(PPDiopenmod.SL) # p = 0.13

timeatcapturemod.SL <- lmer(timeatcapture~SL+(1|Individual),data=all.data)
summary(timeatcapturemod.SL) # p = 0.8

VELpreycapturemod.SL <- lmer(VELpreycapture~SL+(1|Individual),data=all.data)
summary(VELpreycapturemod.SL) # p = 0.002

# Diagnostic plots ---- 

# Use histogram overlaps 
ggplot(data=all.data.ind, aes(x=PG_scale_ind,group=Individual, fill=Individual))  +
  scale_fill_brewer(palette="Dark2")+
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Scaled Peak gape")+
  xlim(-3,3)+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14)) # use

ggplot(data=all.data, aes(x=TTO ,group=Individual, fill=Individual))
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Duration of mouth opening (ms)")

ggplot(data=all.data, aes(x=TTC ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic()+
  ylab("Density")+
  xlab("Duration of mouth closing (ms)")+
  xlim(10,150)+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14))


ggplot(data=all.data, aes(x=PPROT ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic()+
  ylab("Density")+
  xlab("Peak protrusion (cm)")


ggplot(data=all.data, aes(x=PPROTVEL ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Protrusion velocity (cm/s)")


ggplot(data=all.data, aes(x=tPPROT ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Timing of peak protrusion (ms)")

ggplot(data=all.data.ind, aes(x=VELPG_scale_ind ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Scaled velocity at peak gape")+
  xlim(-3,3)+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14))# use


ggplot(data=all.data, aes(x=maxVEL ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Maximum velocity (cm/s)")


ggplot(data=all.data, aes(x=tmaxVEL ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Timing of maximum velocity (ms)")


ggplot(data=all.data, aes(x=ACCPG ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  labs(x=bquote('Acceleration at peak gape'~(cm/s^2)))

ggplot(data=all.data, aes(x=H_L_ratio ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Height:Length of volume")


ggplot(data=all.data, aes(x=AI ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Accuracy Index")+
  xlim(0,1)# use 


ggplot(data=all.data, aes(x=ingested_volume ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  labs(x=bquote('Ingested volume'~(cm^3)))


ggplot(data=all.data, aes(x=PPDiopen ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Predator-prey distance at mouth opening (cm)")


ggplot(data=all.data, aes(x=timeatcapture ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Time at capture relative to peak gape (ms)")


ggplot(data=all.data, aes(x=VELpreycapture ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Velocity at prey capture (cm/s)")


# Coefficient of variation by individual ----

# Write custom function 
CoVar <- function(mean,sd){
  CV1 <- ((sd)/(mean))
  CV <- CV1 * 100
  return(abs(CV))
}

# use data frame 'means' to supply the mean and sd 
PG <- CoVar(mean = means$mean[means$variable=="PG"],sd=means$sd[means$variable=="PG"])
TTO <- CoVar(mean = means$mean[means$variable=="TTO"],sd=means$sd[means$variable=="TTO"])
TTC <- CoVar(mean = means$mean[means$variable=="TTC"],sd=means$sd[means$variable=="TTC"])
PPROT <- CoVar(mean = means$mean[means$variable=="PPROT"],sd=means$sd[means$variable=="PPROT"])
PPROTVEL <- CoVar(mean = means$mean[means$variable=="PPROTVEL"],sd=means$sd[means$variable=="PPROTVEL"])
tPPROT <- CoVar(mean = means$mean[means$variable=="tPPROT"],sd=means$sd[means$variable=="tPPROT"])
VELPG <- CoVar(mean = means$mean[means$variable=="VELPG"],sd=means$sd[means$variable=="VELPG"])
maxVEL <- CoVar(mean = means$mean[means$variable=="maxVEL"],sd=means$sd[means$variable=="maxVEL"])
tmaxVEL <- CoVar(mean = means$mean[means$variable=="tmaxVEL"],sd=means$sd[means$variable=="tmaxVEL"])
ACCPG <- CoVar(mean = means$mean[means$variable=="ACCPG"],sd=means$sd[means$variable=="ACCPG"])
H_L_ratio <- CoVar(mean = means$mean[means$variable=="H_L_ratio"],sd=means$sd[means$variable=="H_L_ratio"])
AI <- CoVar(mean = means$mean[means$variable=="AI"],sd=means$sd[means$variable=="AI"])
ingested_volume <- CoVar(mean = means$mean[means$variable=="ingested_volume"],sd=means$sd[means$variable=="ingested_volume"])
PPDiopen <- CoVar(mean = means$mean[means$variable=="PPDiopen"],sd=means$sd[means$variable=="PPDiopen"])
timeatcapture <- CoVar(mean = means$mean[means$variable=="timeatcapture"],sd=means$sd[means$variable=="timeatcapture"])
VELpreycapture <- CoVar(mean = means$mean[means$variable=="VELpreycapture"],sd=means$sd[means$variable=="VELpreycapture"])

# Merge into a dataframe
CV <- data.frame(rbind(PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG,H_L_ratio,AI,ingested_volume,PPDiopen,timeatcapture,VELpreycapture))

# rename the columns by individual 
names(CV)[1] <- "LAUR01"
names(CV)[2] <- "LAUR02"
names(CV)[3] <- "LAUR03"
names(CV)[4] <- "LAUR04"
names(CV)[5] <- "LAUR05"

CV$Variables <- c("PG","TTO","TTC","PPROT","PPROTVEL","tPPROT","VELPG","maxVEL","tmaxVEL","ACCPG","H_L_ratio","AI",
                  "ingested_volume","PPDiopen","timeatcapture","VELpreycapture")


# Melt data frame
CV_melt <- melt(CV,id.vars = "Variables", measure.vars = c("LAUR01","LAUR02","LAUR03","LAUR04","LAUR05"))
names(CV_melt)[2] <- "Individual"
names(CV_melt)[3] <- "CV"

# Export data 
write_csv(CV_melt,file = "Redbreast_CV_2021.csv",append = TRUE)


# Taking a look at integration ---- 
# Remind ourselves of the density plots 
ggplot(data=all.data, aes(x=PG ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Peak gape (cm)")

ggplot(data=all.data, aes(x=VELPG ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Velocity at peak gape (cm)")

ggplot(data=all.data, aes(x=AI ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Accuracy Index")


# In the context of integration (using two or more systems at the same time), 
# We are interested in the integration of feeding and swimming 
# To begin to look at this, we can look at how swim speed predicts mouth size 

# center and scale by body size BY INDIVIDUAL 
# subset by fish 
fish.1.scale <- all.data %>%
  select(Individual,PG,VELPG)%>%
  filter(Individual=="LAUR01")

fish.1.scale$PG_scale_ind <- scale(fish.1.scale$PG,center = T,scale = T)
fish.1.scale$VELPG_scale_ind <- scale(fish.1.scale$VELPG,center = T,scale = T)

fish.2.scale <- all.data %>%
  select(Individual,PG,VELPG,)%>%
  filter(Individual=="LAUR02")

fish.2.scale$PG_scale_ind <- scale(fish.2.scale$PG,center = T,scale = T)
fish.2.scale$VELPG_scale_ind <- scale(fish.2.scale$VELPG,center = T,scale = T)

fish.3.scale <- all.data %>%
  select(Individual,PG,VELPG,)%>%
  filter(Individual=="LAUR03")

fish.3.scale$PG_scale_ind <- scale(fish.3.scale$PG,center = T,scale = T)
fish.3.scale$VELPG_scale_ind <- scale(fish.3.scale$VELPG,center = T,scale = T)

fish.4.scale <- all.data %>%
  select(Individual,PG,VELPG,)%>%
  filter(Individual=="LAUR04")

fish.4.scale$PG_scale_ind <- scale(fish.4.scale$PG,center = T,scale = T)
fish.4.scale$VELPG_scale_ind <- scale(fish.4.scale$VELPG,center = T,scale = T)

fish.5.scale <- all.data %>%
  select(Individual,PG,VELPG,)%>%
  filter(Individual=="LAUR05")

fish.5.scale$PG_scale_ind <- scale(fish.5.scale$PG,center = T,scale = T)
fish.5.scale$VELPG_scale_ind <- scale(fish.5.scale$VELPG,center = T,scale = T)

# Make data frame 
all.data.ind <- data.frame(rbind(fish.1.scale,fish.2.scale,fish.3.scale,fish.4.scale,fish.5.scale))

# PCA ----
# First regress all of the individual response variables by standard length to remove the effect of size 
# Make function to extract residuals 
ResidExtract <- function(Y,X,data){
  model <- lm(Y~X)
  data.frame(model$residuals)
}

# Extract residuals 
PG <- ResidExtract(Y=all.data$PG,X=all.data$SL)
TTO <- ResidExtract(Y=all.data$TTO,X=all.data$SL)
TTC <- ResidExtract(Y=all.data$TTC,X=all.data$SL)
PPROT <- ResidExtract(Y=all.data$PPROT,X=all.data$SL)
PPROTVEL <- ResidExtract(Y=all.data$PPROTVEL,X=all.data$SL)
tPPROT <- ResidExtract(Y=all.data$tPPROT,X=all.data$SL)
VELPG <- ResidExtract(Y=all.data$VELPG,X=all.data$SL)
maxVEL <- ResidExtract(Y=all.data$maxVEL,X=all.data$SL)
tmaxVEL <- ResidExtract(Y=all.data$tmaxVEL,X=all.data$SL)
ACCPG <- ResidExtract(Y=all.data$ACCPG,X=all.data$SL)

# Make dataframe with just residuals
all.data.res <- data.frame(PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG)
colnames(all.data.res) <- c("PG","TTO","TTC","PPROT","PPROTVEL","tPPROT","VELPG","maxVEL","tmaxVEL","ACCPG")

# Add the individual grouping and the strategy ("line")
all.data.res$Individual <- as.factor(all.data$Individual)

PCA_data <- all.data.res

# Export data for PCA
write_csv(PCA_data,file="PCA_data_Redbreast2021.csv")

# Read in data
pca.data <- read.csv(file = "PCA_data_Redbreast2021.csv")

# Subset the feeding and locomotion variables out 
pca.data_mod <- pca.data %>%
  dplyr::select(PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG)

# Run PCA 
results <- prcomp(pca.data_mod,scale=TRUE)

#display principal components
comp <- results$x

#calculate total variance explained by each principal component
var <- results$sdev^2
var_results <- round(var/sum(var)*100,1)

# Make biplot 
biplot(results,scale=0)

# Save results from components analysis ad data frame (PC)
comp.out <- as.data.frame(comp)


# Get the PCA output and check out other stats/properties of the components 
fviz_pca_var(results)
fviz_eig(results) # skree plot : pc under 10% difference between components not as important 

# get the loading scores for each component. In prcomp(), loading scores are referred to as "rotation"
load.score <- results$rotation[,1] # loading by PC of choice
variable.score <- abs(load.score) # magnitude of loadings
ranked.score <- sort(variable.score,decreasing=TRUE)
top.ten <- names(ranked.score[1:10])
fviz_contrib(results, choice="var",axes=1, top=10) # See what variables are explaining variation 
fviz_contrib(results, choice="var",axes=2, top=10)

## Graph 
# Rename the factors 
comp.out$Individual <- c("LAUR01","LAUR02","LAUR03","LAUR04","LAUR05")
as.factor(comp.out$Individual)
levels(comp.out$Individual) <- c("LAUR01","LAUR02","LAUR03","LAUR04","LAUR05")

ggplot(comp.out,aes(x=PC1,y=PC2,color=Individual)) +
  geom_point()+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("PC1 (50.4%)")+
  ylab("PC2 (19.6%)")+
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        legend.position = "top")+
  stat_ellipse()

## Calculate the distribution of scores for each PC
fish1.pc1 <- comp.out$PC1[comp.out$Individual=="LAUR01"]
fish1.pc2 <- comp.out$PC2[comp.out$Individual=="LAUR01"]

fish2.pc1 <- comp.out$PC1[comp.out$Individual=="LAUR02"]
fish2.pc2 <- comp.out$PC2[comp.out$Individual=="LAUR02"]

fish3.pc1 <- comp.out$PC1[comp.out$Individual=="LAUR03"]
fish3.pc2 <- comp.out$PC2[comp.out$Individual=="LAUR03"]

fish4.pc1 <- comp.out$PC1[comp.out$Individual=="LAUR04"]
fish4.pc2 <- comp.out$PC2[comp.out$Individual=="LAUR04"]

fish5.pc1 <- comp.out$PC1[comp.out$Individual=="LAUR05"]
fish5.pc2 <- comp.out$PC2[comp.out$Individual=="LAUR05"]

PC1_scores <- data.frame(fish1.pc1,fish2.pc1,
                         fish3.pc1,fish4.pc1,
                         fish5.pc1)
PC1_scores <- cbind(stack(PC1_scores[,1:5]))
PC1_scores$ind <- comp.out$Individual

PC2_scores <- data.frame(fish1.pc2,fish2.pc2,
                         fish3.pc2,fish4.pc2,
                         fish5.pc2)
PC2_scores <- cbind(stack(PC2_scores[,1:5]))
PC2_scores$ind <- comp.out$Individual

ggplot(data=PC1_scores, aes(x=values ,group=ind, fill=ind)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("PC1 scores")+
  scale_fill_discrete(name = "Individual")

ggplot(data=PC2_scores, aes(x=values ,group=ind, fill=ind)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("PC2 scores")+
  scale_fill_discrete(name = "Individual")


# Get mean PC scores for each fish 
ddply(.data = PC1_scores,.variables = c("ind"),summarize, mean=mean(values),sd=sd(values))
ddply(.data = PC2_scores,.variables = c("ind"),summarize, mean=mean(values),sd=sd(values))


# Morphology data ---- 
measures <- read.csv("MorphologyMeasurements_Redbreast_2022_averaged.csv")

data_merged <- merge(all.data, measures, by= "Individual")
data_merged$Gape_prop <- data_merged$PG/data_merged$Gape_height

ggplot(data=data_merged, aes(x=PG, y=Gape_prop ,colour=Individual, fill=Individual)) +
  geom_point() +
  theme_classic()+
  xlab("Peak Gape")+
  ylab("Proportion of Max Gape")

ggplot(data=data_merged, aes(x=SL.y, y=Gape_height ,colour=Individual, fill=Individual)) +
  geom_point() +
  theme_classic()+
  xlab("Standard Length")+
  ylab("Max Anatomical Gape")


