rm(list = ls())
#Rstan and others installation
# Sys.setenv(MAKEFLAGS = "-j4") 
# install.packages("rstan", dependencies = TRUE)  
# install.packages("rstanarm")
library (rstan)
library (rstanarm)
library(shinystan) # still do not know how to use this :)
library(reshape2) # manipulation with original data
#set up processors and ram for use
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#loading data
data = read.csv("Data/sciodrepoides_data.csv", header=TRUE, sep = ";") #old dataset
head(data)
#reshaping data
# reshaping the data to be in tidy format
# library(dplyr) # manipulation with original data
# library (lazyeval) # manipulation with original data
# library(tidyr) # manipulation with original data
# library(ggplot2) # plotting graphs
# library(Rmisc) # summarySE function for SE and CI calcul. and ploting
# library (broom)

#treat will be left out and not melted, but rest will be
substr <- melt (data, id=c("indiv","year", "loc", "temp"),factorsAsStrings=F) 
summary(substr[substr$stage=="egg",])
head (substr) #see how it looks like
names (substr) <- c("ID", "year", "loc", "temp", "stage", "devel") #meaningful names of variables
#data explorations
substr$devel <- as.numeric(substr$devel)
summary (substr[substr$stage=="egg",])
str(substr)
substr$year <- as.factor (substr$year)
DT <- substr$devel * substr$temp
substr <- cbind (substr, DT)
head(substr)
substr12 <- substr[substr$year=="2012",]
substr13 <- substr[substr$year=="2013",]
head(substr13)

#Bayes analysis - experiment
mm <- stan_glm (substr13$DT[substr13$stage=="egg"]~substr13$devel[substr13$stage=="egg"], data=substr13, prior=normal (0, 8)) # GLM model 
mm  #===> Results
pp_check (mm, "dist", nreps=80) #graph
summary (mm)

#L1 model
mm <- stan_glm (substr13$DT[substr13$stage=="L1"]~substr13$devel[substr13$stage=="L1"]+substr13$loc[substr13$stage=="L1"], data=substr13, prior=normal (0, 8)) # GLM model 
mm  #===> Results
pp_check (mm, "dist", nreps=80) #graph
summary (mm)
