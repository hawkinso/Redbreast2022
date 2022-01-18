# Redbreast project 2022
# Date: 01/12/2022
# Author(s): Olivia H Hawkins 
# Goals: Partial least squares 

#Basic intro----
# Packages 
install.packages("pls")
library(pls)
library(tidyverse)
library(dplyr)
library(rstatix)


# Load in data 
data <- read.csv("RedBreast_2021.csv")

# Subset original data ----
# Subset data that will be used in analysis ----
all.data <- data %>%
  dplyr::select(Individual,SL,PG,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG,H_L_ratio,AI,ingested_volume,PPDiopen,timeatcapture,VELpreycapture)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

# Make sure data is stored as data frame 
as.data.frame(all.data)


###### Partial least squares ######

### Step one: Fit the PLS model 
# Choose response and the predictor variables 
## Some notes: 
      # scale=TRUE: data set will be scaled to have a mean of 0 and set standard 
          # deviation of 1. This keeps the influence of all predictors equal in the 
          # case of multiple units 
      # validation=CV: use k-fold cross-validation to determine the performance 
          # of the model. The default is k= 10
          # you can also used the :leave on out cross validation" (LOOCV)
    browseURL("https://www.statology.org/partial-least-squares-in-r/")
    browseURL("https://www.r-bloggers.com/2017/06/partial-least-squares-in-r/")

# Fit model 
model <- plsr(PG~Individual,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG, data=all.data, scale=TRUE, validation="CV")
