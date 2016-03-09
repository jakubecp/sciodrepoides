rm(list = ls())
#Rstan and others installation
# Sys.setenv(MAKEFLAGS = "-j4")
# install.packages("rstan", dependencies = TRUE)
# install.packages("rstanarm")
# install.packages ("shinystan")
library (rstan)
library (rstanarm)
library(shinystan) # still do not know how to use this :)
library(reshape2) # manipulation with original data
library(magrittr)
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
names (substr) <- c("ID", "year", "loc", "temp", "stage", "devel") #meaningful names of variables

#change the type of variable 
substr$devel <- as.numeric(substr$devel)
substr$year <- as.factor (substr$year)

#create new collums (DT and rate)
DT <- substr$devel * substr$temp
substr <- cbind (substr, DT)
rate <- 1/substr$devel
substr <- cbind (substr, rate)

#filter data based on year into two datasets
substr12 <- substr[substr$year=="2012",]
substr13 <- substr[substr$year=="2013",]

#Bayes analysis - experiment DT Ikemoto
mme <- stan_glm (substr13$DT[substr13$stage=="egg"]~substr13$devel[substr13$stage=="egg"], data=substr13, prior=normal (0, 8)) # GLM model 
mme  #===> Results
pp_check (mme, "dist", nreps=30) #graph
summary (mme)

#L1 model
mm1 <- stan_glm (substr13$DT[substr13$stage=="L1"]~substr13$devel[substr13$stage=="L1"], data=substr13, prior=normal (0, 8)) # GLM model 
mm1  #===> Results
pp_check (mm1, "dist", nreps=30) #graph
summary (mm1)

#L2 model
mm2 <- stan_glm (substr$DT[substr$stage=="L2"]~substr$devel[substr$stage=="L2"], data=substr, prior=normal (0, 8)) # GLM model 
mm2  #===> Results
pp_check (mm2, "dist", nreps=30) #graph
summary (mm2)

#L3 model
mm3 <- stan_glm (substr$DT[substr$stage=="L3"]~substr$devel[substr$stage=="L3"], data=substr, prior=normal (0, 8)) # GLM model 
mm3  #===> Results
pp_check (mm3, "dist", nreps=30) #graph
summary (mm3)

#Pupae model
mmp <- stan_glm (substr$DT[substr$stage=="pupae"]~substr$devel[substr$stage=="pupae"], data=substr, prior=normal (0, 8)) # GLM model 
mmp  #===> Results
pp_check (mmp, "dist", nreps=30) #graph
summary (mmp)
