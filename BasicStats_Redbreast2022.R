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
data <- read.csv("RedBreast_2021.csv")

# Subset data that will be used in analysis ----
# Individual
all.data <- data %>%
  dplyr::select(Individual,SL,PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG,H_L_ratio,AI,ingested_volume,PPDiopen,timeatcapture,VELpreycapture)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual)


# Make sure data is stored as data frame 
as.data.frame(all.data)

# Check out data structure 
str(all.data)

# Get group means and sd for each individual ----
means <- all.data %>%
  get_summary_stats()

# Get standard length (mean +/- SD)
mean(all.data$SL) #9.80
sd(all.data$SL) # 0.92

# Write to .csv 
write_csv(means,file = "Redbreast_summarystats_2021.csv",append = FALSE)

# Check assumptions ----
# Normality, homogeneity of variance,independence of observations
# Independence of observations is not met as each individual is sampled 20 times 


# Check normality with Shapiro-Wilk test 
## By individuals 
sw <- ddply(.data=all.data, .variables=c("Individual"),numcolwise(shapiro.test))
sw <- sw[-c(3:4,7:8,11:12,15:16,19:20),]

sw.results <- gather(data = sw,key = Variable, value=Results,2:18)

# Add column to show what the value is (W statistic and p value)
sw.results$Value <- rep(c("W statistic","p value"))

# Find the individual x variable combination that violates normality 
sw.results2 <- sw.results %>%
   group_by(Individual)%>%
    filter(Value=="p value")%>% 
  filter(Results<0.05)

# Visualize normality by individual
ggqqplot(data=all.data, x = ("PG"),
         color = "Individual",facet.by="Individual") # some not
ggqqplot(all.data, x = "TTO",
         color = "Individual",facet.by = "Individual") # some not
ggqqplot(all.data, x = "TTC",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "PPROT",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "PPROTVEL",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "tPPROT",
         color = "Individual",facet.by = "Individual") # ok
ggqqplot(all.data, x = "VELPG",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "maxVEL",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "tmaxVEL",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "ACCPG",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "H_L_ratio",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "AI",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "ingested_volume",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "PPDiopen",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "timeatcapture",
         color = "Individual",facet.by = "Individual") # pretty good 
ggqqplot(all.data, x = "VELpreycapture",
         color = "Individual",facet.by = "Individual") # pretty good

# Identify outliers 

# Individual 1 
Fish.1 <- all.data %>%
  filter(Individual=="LAUR01")

Fish.1 %>% select(PG) %>% identify_outliers() # three extreme 
Fish.1 %>% select(TTO) %>% identify_outliers() # three extreme
Fish.1 %>% select(TTC) %>% identify_outliers(TTC)
Fish.1 %>% select(PPROT) %>% identify_outliers(PPROT)
Fish.1 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) # two extreme 
Fish.1 %>% select(tPPROT) %>% identify_outliers(tPPROT) # one extreme 
Fish.1 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.1 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.1 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL) # one extreme 
Fish.1 %>% select(ACCPG) %>% identify_outliers(ACCPG) # one extreme 
Fish.1 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.1 %>% select(AI) %>% identify_outliers(AI)
Fish.1 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.1 %>% select(PPDiopen) %>% identify_outliers(PPDiopen)
Fish.1 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.1 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)


# Individual 2 
Fish.2 <- all.data %>%
  filter(Individual=="LAUR02")

Fish.2 %>% select(PG) %>% identify_outliers() # two extreme 
Fish.2 %>% select(TTO) %>% identify_outliers() # two extreme
Fish.2 %>% select(TTC) %>% identify_outliers(TTC)
Fish.2 %>% select(PPROT) %>% identify_outliers(PPROT) # two extreme
Fish.2 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.2 %>% select(tPPROT) %>% identify_outliers(tPPROT) # one extreme 
Fish.2 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.2 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.2 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL) # one extreme 
Fish.2 %>% select(ACCPG) %>% identify_outliers(ACCPG) # one extreme 
Fish.2 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.2 %>% select(AI) %>% identify_outliers(AI)
Fish.2 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.2 %>% select(PPDiopen) %>% identify_outliers(PPDiopen) # one extreme 
Fish.2 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.2 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)


# Individual 3
Fish.3 <- all.data %>%
  filter(Individual=="LAUR03")

Fish.3 %>% select(PG) %>% identify_outliers()  
Fish.3 %>% select(TTO) %>% identify_outliers() 
Fish.3 %>% select(TTC) %>% identify_outliers(TTC)
Fish.3 %>% select(PPROT) %>% identify_outliers(PPROT) 
Fish.3 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.3 %>% select(tPPROT) %>% identify_outliers(tPPROT) 
Fish.3 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.3 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.3 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL) 
Fish.3 %>% select(ACCPG) %>% identify_outliers(ACCPG) # one extreme
Fish.3 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.3 %>% select(AI) %>% identify_outliers(AI)
Fish.3 %>% select(ingested_volume) %>% identify_outliers(ingested_volume) # one extreme
Fish.3 %>% select(PPDiopen) %>% identify_outliers(PPDiopen) # one extreme 
Fish.3 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.3 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Individual 4 
Fish.4 <- all.data %>%
  filter(Individual=="LAUR04")

Fish.4 %>% select(PG) %>% identify_outliers()  
Fish.4 %>% select(TTO) %>% identify_outliers() 
Fish.4 %>% select(TTC) %>% identify_outliers(TTC)
Fish.4 %>% select(PPROT) %>% identify_outliers(PPROT) 
Fish.4 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.4 %>% select(tPPROT) %>% identify_outliers(tPPROT)  
Fish.4 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.4 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.4 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
Fish.4 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.4 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.4 %>% select(AI) %>% identify_outliers(AI)
Fish.4 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.4 %>% select(PPDiopen) %>% identify_outliers(PPDiopen)  
Fish.4 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.4 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Indivdual 5 
Fish.5 <- all.data %>%
  filter(Individual=="LAUR05")

Fish.5 %>% select(PG) %>% identify_outliers()  
Fish.5 %>% select(TTO) %>% identify_outliers() 
Fish.5 %>% select(TTC) %>% identify_outliers(TTC)
Fish.5 %>% select(PPROT) %>% identify_outliers(PPROT) 
Fish.5 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.5 %>% select(tPPROT) %>% identify_outliers(tPPROT) 
Fish.5 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.5 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.5 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
Fish.5 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.5 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.5 %>% select(AI) %>% identify_outliers(AI)
Fish.5 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.5 %>% select(PPDiopen) %>% identify_outliers(PPDiopen) 
Fish.5 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.5 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Check homogeneity of variance with Levene's test 
leveneTest(all.data$PG~all.data$Individual)
leveneTest(all.data$TTO~all.data$Individual)
leveneTest(all.data$TTC~all.data$Individual)
leveneTest(all.data$PPROT~all.data$Individual)
leveneTest(all.data$PPROTVEL~all.data$Individual) # p < 0.0001
leveneTest(all.data$tPPROT~all.data$Individual) # p = 0.007
leveneTest(all.data$VELPG~all.data$Individual) # p = 0.0005
leveneTest(all.data$maxVEL~all.data$Individual) # p = 0.0002
leveneTest(all.data$tmaxVEL~all.data$Individual)
leveneTest(all.data$ACCPG~all.data$Individual)
leveneTest(all.data$H_L_ratio~all.data$Individual)#  p < 0.001
leveneTest(all.data$AI~all.data$Individual)
leveneTest(all.data$ingested_volume~all.data$Individual) # P < 0.0001
leveneTest(all.data$PPDiopen~all.data$Individual) # p = 0.02
leveneTest(all.data$timeatcapture~all.data$Individual) # p = 0.02
leveneTest(all.data$VELpreycapture~all.data$Individual) # p < 0.0001



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
summary(PGmod.SL) 

TTOmod.SL <- lmer(TTO~SL+(1|Individual),data=all.data)
summary(TTOmod.SL)

TTCmod.SL <- lmer(TTC~SL+(1|Individual),data=all.data)
summary(TTCmod.SL) 

PPROTmod.SL <- lmer(PPROT~SL+(1|Individual),data=all.data)
summary(PPROTmod.SL) # p = 0.006


PPROTVELmod.SL <- lmer(PPROTVEL~SL+(1|Individual),data=all.data)
summary(PPROTVELmod.SL) # p = 0.02

tPPROTmod.SL <- lmer(tPPROT~SL+(1|Individual),data=all.data)
summary(tPPROTmod.SL)

VELPGmod.SL <- lmer(VELPG~SL+(1|Individual),data=all.data)
summary(VELPGmod.SL) # p = 0.002

maxVELmod.SL <- lmer(maxVEL~SL+(1|Individual),data=all.data)
summary(maxVELmod.SL) # p = 0.02

tmaxVELmod.SL <- lmer(tmaxVEL~SL+(1|Individual),data=all.data)
summary(tmaxVELmod.SL)

ACCPGmod.SL <- lmer(ACCPG~SL+(1|Individual),data=all.data)
summary(ACCPGmod.SL)

HLmod.SL <- lmer(H_L_ratio~SL+(1|Individual),data=all.data)
summary(HLmod.SL)

AImod.SL <- lmer(AI~SL+(1|Individual),data=all.data)
summary(AImod.SL)

ingestedmod.SL <- lmer(ingested_volume~SL+(1|Individual),data=all.data)
summary(ingestedmod.SL) # p = 0.005

PPDiopenmod.SL <- lmer(PPDiopen~SL+(1|Individual),data=all.data)
summary(PPDiopenmod.SL) 

timeatcapturemod.SL <- lmer(timeatcapture~SL+(1|Individual),data=all.data)
summary(timeatcapturemod.SL)

VELpreycapturemod.SL <- lmer(VELpreycapture~SL+(1|Individual),data=all.data)
summary(VELpreycapturemod.SL) # p = 0.002

# Determining the repeatability measure and confidence intervals (CI) ----
# Repeatability measures how individuals contrast in behavior and if those contrasts in behavior are consistent. 
# We can use sum of squares for individuals and for the residuals and then use parametric bootstrapping to calculate CI 

pg <- rpt(PG ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(pg)
summary(pg)

tto <- rpt(TTO ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(tto)
summary(tto)

ttc <- rpt(TTC ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(ttc)
summary(ttc)

pprot <- rpt(PPROT ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(pprot)
summary(pprot)

tpprot <- rpt(tPPROT ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(tpprot)
summary(tpprot)

pprotvel <- rpt(PPROTVEL ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(pprotvel)
summary(pprotvel)

velpg <- rpt(VELPG ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(velpg)
summary(velpg)

maxvel <- rpt(maxVEL ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(maxvel)
summary(maxvel)

tmaxvel <- rpt(tmaxVEL ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(tmaxvel)
summary(tmaxvel)

accpg <- rpt(ACCPG ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(accpg)
summary(accpg)

hlratio <- rpt(H_L_ratio ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(hlratio)
summary(hlratio)

ai <- rpt(AI ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(ai)
summary(ai)

ingestedvol <- rpt(ingested_volume ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(ingestedvol)
summary(ingestedvol)

ppd <- rpt(PPDiopen ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(ppd)
summary(ppd)

timeatcap <- rpt(timeatcapture ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(timeatcap)
summary(timeatcap)

velpreycapture <- rpt(VELpreycapture ~ SL + (1|Individual), grname = "Individual", data= all.data, datatype="Gaussian",nboot=1000,npermut=0)
plot(velpreycapture)
summary(velpreycapture)

# We can also calculate intra-class correlation  





# Diagnostic plots ---- 

# Use histogram overlaps 
ggplot(data=all.data, aes(x=PG ,group=Individual, fill=Individual))  +
  scale_fill_brewer(palette="Dark2")+
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Peak gape (cm)") # use

ggplot(data=all.data, aes(x=TTO ,group=Individual, fill=Individual))
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Duration of mouth opening (ms)")

ggplot(data=all.data, aes(x=TTC ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic()+
  ylab("Density")+
  xlab("Duration of mouth closing (ms)")


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

ggplot(data=all.data, aes(x=VELPG ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Velocity at peak gape (cm/s)") # use


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
  xlab("Accuracy Index") # use 


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

## Try ridge plots to parse out histogram differences -----

ggplot(all.data, aes(x = PG_scale, y = Individual)) +
  geom_density_ridges(aes(fill = Individual),scale = 1.3, quantile_lines=TRUE,
                      quantile_fun=function(x,...)median(x))+
  theme_classic() 

ggplot(all.data, aes(x = VELPG_scale, y = Individual)) +
  geom_density_ridges(aes(fill = Individual),scale = 1.3, quantile_lines=TRUE,
                      quantile_fun=function(x,...)median(x))+
  theme_classic() 



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

# We need to center and scale the data to remove any effect of SL 
all.data$PG_scale <- scale(all.data$PG,center = T,scale = T)
all.data$VELPG_scale <- scale(all.data$VELPG,center = T,scale = T)

# In the context of integration (using two or more systems at the same time), 
# We are interested in the integration of feeding and swimming 
# To begin to look at this, we can look at how swim speed predicts mouth size 

# Rough glance at the data 
ggplot(all.data, aes(x=VELPG_scale,y=PG_scale,group=Individual,color=Individual))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()+
  scale_color_brewer(palette = "Paired")+
  xlab("Velocity at peak gape (cm/s)")+
  ylab("Peak gape (cm)")

# It looks like there are enough outliers for each individual to make another line 
# Are individuals using two different approaches? 

# Use ifelse to make a new column that separates the "small mouth" vs "large mouth" approach 
all.data$line <- ifelse(all.data$PG_scale < 0, "Small mouth","Large mouth")

small.mouth <- all.data %>%
  select(Individual,VELPG_scale,PG_scale,line)%>%
  filter(line=="Small mouth")

large.mouth <- all.data %>%
  select(Individual,VELPG_scale,PG_scale,line)%>%
  filter(line=="Large mouth")

# Now plot again, this time taking into account the two lines 
# export plot as pdf 
names(small.mouth)[4] <- "Strategy"
names(large.mouth)[4] <- "Strategy"

pdf(file="scaled_integration.pdf")

ggplot()+
  geom_point(large.mouth, mapping=aes(x=VELPG_scale,y=PG_scale,group=Individual,color=Individual,shape=Strategy))+
  geom_smooth(large.mouth,method = "lm",se=F,mapping=aes(x=VELPG_scale,y=PG_scale,group=Individual,color=Individual))+
  geom_point(small.mouth, mapping=aes(x=VELPG_scale,y=PG_scale,group=Individual,color=Individual,shape=Strategy))+
  geom_smooth(small.mouth,method = "lm",se=F,mapping=aes(x=VELPG_scale,y=PG_scale,group=Individual,color=Individual))+
  theme_classic()+
  xlab("Velocity at peak gape (cm/s)")+
  ylab("Peak gape (cm)")

dev.off()


# center and scale by body size BY INDIVIDUAL 
# subset by fish 
fish.1.scale <- all.data %>%
  select(Individual,PG,VELPG,line)%>%
  filter(Individual=="LAUR01")

fish.1.scale$PG_scale_ind <- scale(fish.1.scale$PG,center = T,scale = T)
fish.1.scale$VELPG_scale_ind <- scale(fish.1.scale$VELPG,center = T,scale = T)

fish.2.scale <- all.data %>%
  select(Individual,PG,VELPG,line)%>%
  filter(Individual=="LAUR02")

fish.2.scale$PG_scale_ind <- scale(fish.2.scale$PG,center = T,scale = T)
fish.2.scale$VELPG_scale_ind <- scale(fish.2.scale$VELPG,center = T,scale = T)

fish.3.scale <- all.data %>%
  select(Individual,PG,VELPG,line)%>%
  filter(Individual=="LAUR03")

fish.3.scale$PG_scale_ind <- scale(fish.3.scale$PG,center = T,scale = T)
fish.3.scale$VELPG_scale_ind <- scale(fish.3.scale$VELPG,center = T,scale = T)

fish.4.scale <- all.data %>%
  select(Individual,PG,VELPG,line)%>%
  filter(Individual=="LAUR04")

fish.4.scale$PG_scale_ind <- scale(fish.4.scale$PG,center = T,scale = T)
fish.4.scale$VELPG_scale_ind <- scale(fish.4.scale$VELPG,center = T,scale = T)

fish.5.scale <- all.data %>%
  select(Individual,PG,VELPG,line)%>%
  filter(Individual=="LAUR05")

fish.5.scale$PG_scale_ind <- scale(fish.5.scale$PG,center = T,scale = T)
fish.5.scale$VELPG_scale_ind <- scale(fish.5.scale$VELPG,center = T,scale = T)

# Make data frame 
all.data.ind <- data.frame(rbind(fish.1.scale,fish.2.scale,fish.3.scale,fish.4.scale,fish.5.scale))
names(all.data.ind)[4] <- "Strategy"

# Subset data into categories 
small.mouth.ind <- all.data.ind %>%
  select(Individual,VELPG_scale_ind,PG_scale_ind,Strategy)%>%
  filter(Strategy=="Small mouth")

large.mouth.ind <- all.data.ind %>%
  select(Individual,VELPG_scale_ind,PG_scale_ind,Strategy)%>%
  filter(Strategy=="Large mouth")

# Graph again 
names(small.mouth.ind)[4] <- "Strategy"
names(large.mouth.ind)[4] <- "Strategy"

pdf(file="scaled_byindividual_integration.pdf")
ggplot()+
  geom_point(large.mouth.ind, mapping=aes(x=VELPG_scale_ind,y=PG_scale_ind,group=Individual,color=Individual,shape=Strategy))+
  geom_smooth(large.mouth.ind,method = "lm",se=F,mapping=aes(x=VELPG_scale_ind,y=PG_scale_ind,group=Individual,color=Individual))+
  geom_point(small.mouth.ind, mapping=aes(x=VELPG_scale_ind,y=PG_scale_ind,group=Individual,color=Individual,shape=Strategy))+
  geom_smooth(small.mouth.ind,method = "lm",se=F,mapping=aes(x=VELPG_scale_ind,y=PG_scale_ind,group=Individual,color=Individual))+
  theme_classic()+
  xlab("Velocity at peak gape (cm/s)")+
  ylab("Peak gape (cm)")
dev.off()

# Now graph with all small mouth together and all large mouth together 

ggplot()+
  geom_point(large.mouth.ind, mapping=aes(x=VELPG_scale_ind,y=PG_scale_ind,shape=Strategy))+
  geom_smooth(large.mouth.ind,method = "lm",se=F,mapping=aes(x=VELPG_scale_ind,y=PG_scale_ind,color=Strategy))+
  scale_color_brewer(palette="Paired")+
  geom_point(small.mouth.ind, mapping=aes(x=VELPG_scale_ind,y=PG_scale_ind,shape=Strategy))+
  geom_smooth(small.mouth.ind,method = "lm",se=F,mapping=aes(x=VELPG_scale_ind,y=PG_scale_ind,color=Strategy))+
  theme_classic()+
  xlab("Velocity at peak gape (cm/s)")+
  ylab("Peak gape (cm)")


# General linear mixed model with individual scaled values 
# Strategy as a fixed effect, individual as random effect 

PG_mod <- lmer(PG_scale_ind~Strategy + (1|Individual), data=all.data.ind)
summary(PG_mod)
anova(PG_mod)

VELPG_mod <- lmer(VELPG_scale_ind~Strategy + (1|Individual), data=all.data.ind)
summary(VELPG_mod)
anova(VELPG_mod)


# Repeatability measurement and liklihood ratio for confidence intervals 







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
all.data.res$line <- as.factor(all.data$line)

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

# Make a column with individual and strategy together
comp.out$Ind_Strat <- paste(all.data.res$Individual, all.data.res$line)


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
levels(comp.out$Individual) <- c("LAUR01","LAUR02","LAUR03","LAUR04","LAUR05")

ggplot(comp.out,aes(x=PC1,y=PC2,color=Individual,shape=line,group=Ind_Strat)) +
  geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  xlab("PC1 (50.4%)")+
  ylab("PC2 (19.6%)")+
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


## Make a PCA for just large mouth strategy and another for small mouth strategy 
PCA_data_large <- all.data.res %>% 
  filter(line=="Large mouth")
PCA_data_small <- all.data.res %>% 
  filter(line=="Small mouth")

# Export data for PCA
write_csv(PCA_data_large,file="PCA_data_largemouth_Redbreast2021.csv")
write_csv(PCA_data_small,file="PCA_data_smallmouth_Redbreast2021.csv")

# Read in data
pca.data.large <- read.csv(file = "PCA_data_largemouth_Redbreast2021.csv")
pca.data.small <- read.csv(file = "PCA_data_smallmouth_Redbreast2021.csv")

# Subset the feeding and locomotion variables out 
pca.data_mod_large <- pca.data.large %>%
  dplyr::select(PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG)
pca.data_mod_small <- pca.data.small %>%
  dplyr::select(PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG)

# Run PCA 
results_large <- prcomp(pca.data_mod_large,scale=TRUE)
results_small <- prcomp(pca.data_mod_small,scale=TRUE)

#display principal components
comp.large <- results_large$x
comp.small <- results_small$x

#calculate total variance explained by each principal component
var_large <- results_large$sdev^2
var_results_large <- round(var_large/sum(var_large)*100,1)

var_small <- results_small$sdev^2
var_results_small <- round(var_small/sum(var_small)*100,1)


# Make biplot 
biplot(results_large,scale=0)
biplot(results_small,scale=0)


# Save results from components analysis ad data frame (PC)
comp.out.large <- as.data.frame(comp.large)
comp.out.small <- as.data.frame(comp.small)

# Add Individual column back in as factor 
comp.out.large$Individual <- PCA_data_large$Individual
comp.out.small$Individual <- PCA_data_small$Individual

# Get the PCA output and check out other stats/properties of the components 
fviz_pca_var(results_large)
fviz_eig(results_large) 

fviz_pca_var(results_small)
fviz_eig(results_small) 

# get the loading scores for each component. In prcomp(), loading scores are referred to as "rotation"
# Adjust for large and small mouth strategy if interested in these particular numbers 
load.score <- results$rotation[,1] # loading by PC of choice
variable.score <- abs(load.score) # magnitude of loadings
ranked.score <- sort(variable.score,decreasing=TRUE)
top.ten <- names(ranked.score[1:10])

fviz_contrib(results_large, choice="var",axes=1, top=10) # See what variables are explaining variation 
fviz_contrib(results_large, choice="var",axes=2, top=10)

fviz_contrib(results_small, choice="var",axes=1, top=10) # See what variables are explaining variation 
fviz_contrib(results_small, choice="var",axes=2, top=10)

## Graph 
# Rename the factors 
levels(comp.out.large$Individual) <- c("LAUR01","LAUR02","LAUR03","LAUR04","LAUR05")
levels(comp.out.small$Individual) <- c("LAUR01","LAUR02","LAUR03","LAUR04","LAUR05")

ggplot(comp.out.large,aes(x=PC1,y=PC2,color=Individual,group=Individual)) +
  geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  xlab("PC1 (39.7%)")+
  ylab("PC2 (17.5%)")+
  stat_ellipse()+
  ggtitle("Large gape strategy")

ggplot(comp.out.small,aes(x=PC1,y=PC2,color=Individual,group=Individual)) +
  geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  xlab("PC1 (51.7%)")+
  ylab("PC2 (14.2%)")+
  stat_ellipse()+
  ggtitle("Small gape strategy")

