rm(list = ls())
data = read.csv("Data/cholevinae_full.csv", header=TRUE, sep = ";")
head(data)
summary(data)
class(data$DT_egg)
class (data$temp)
data_12 = data [data$year=="2012",] # data for 2012
data_13 = data [data$year=="2013",] # data for 2013
data_t15 = data [data$temp=="15",] # data for 15°C
data_t18 = data [data$temp=="18",] # data for 18°C
data_t21 = data [data$temp=="21",] # data for 21°C
data_t25 = data [data$temp=="25",] # data for 25°C
data_t28 = data [data$temp=="28",] # data for 28°C

#interaction plot of individual growth rate

interaction.plot (data$fotka, data$X, data$delka)

#Mortality
summary (data_t15$mortality_total) #until adulthood
summary (data_t18$mortality_total) #until adulthood
summary (data_t21$mortality_total) #until adulthood
summary (data_t25$mortality_total) #until adulthood
summary (data_t28$mortality_total) #until adulthood

summary (data_t15$mortality_L2) #until L3
summary (data_t18$mortality_L2) #until L3
summary (data_t21$mortality_L2) #until L3
summary (data_t25$mortality_L2) #until L3
summary (data_t28$mortality_L2) #until L3

summary (data_t15$mortality_L3) #of L3
summary (data_t18$mortality_L3) #of L3
summary (data_t21$mortality_L3) #of L3
summary (data_t25$mortality_L3) #of L3
summary (data_t28$mortality_L3) #of L3

(1-0.1429)*100
(1-0.03077)*100
(1-0.1458)*100
(1-0.03704)*100

#Plot of mean mortality for each treatment
#SEcond Instar
mort_15 <-(1-mean(data_t15$mortality_L2))*100 #until L3
mort_18 <-(1-mean(data_t18$mortality_L2))*100 #until L3
mort_21 <-(1-mean(data_t21$mortality_L2))*100 #until L3
mort_25 <-(1-mean(data_t25$mortality_L2))*100 #until L3
mort_28 <-100 #until L3
mort1 <- c(mort_15, mort_18, mort_21, mort_25)
temp1 <- c(15,18,21,25)
plot (mort1~temp1, type="o" , col="black", pch=16, 
      main = "mean mortality of second instar", ylim=c(0,100))
#Third instar
mort_15 <-(1-mean(data_t15$mortality_L3))*100 #until L3
mort_18 <-(1-mean(data_t18$mortality_L3))*100 #until L3
mort_21 <-(1-mean(data_t21$mortality_L3))*100 #until L3
mort_25 <-(1-mean(data_t25$mortality_L3))*100 #until L3
mort_28 <-100 #until L3
mort2 <- c(mort_15, mort_18, mort_21, mort_25)
plot (mort2~temp1, type="o" , col="black", pch=16, 
      main = "mean mortality of third instar", ylim=c(0,100))
#until adulthood
mort_15 <-(1-mean(data_t15$mortality_total))*100 #until L3
mort_18 <-(1-mean(data_t18$mortality_total))*100 #until L3
mort_21 <-(1-mean(data_t21$mortality_total))*100 #until L3
mort_25 <-(1-mean(data_t25$mortality_total))*100 #until L3
mort_28 <-100 #until L3
mort3 <- c(mort_15, mort_18, mort_21, mort_25)
plot (mort3~temp1, type="o" , col="black", pch=16, 
      main = "mean mortality until adulthood", ylim=c(0,100))

#Generate and export graph for mortality
tiff (filename="exports/sciodrepoides_mortality.tiff", 
      width=5000, height=5000, 
      compression="lzw", res= 800)
plot (mort1~temp1, pch=0, 
      ylim=c(0,100), xlim=c(15, 26), 
      xlab="Temperature (°C)", ylab="Mortality (%)")
points (temp1, mort2,  pch=1)
points (temp1, mort3,  pch=2)
lines (temp1, mort2,  pch=16, lty=3)
lines (temp1, mort3,  pch=16, lty=2)
lines (temp1, mort1,  pch=16, lty=1)
legend (23,20, c("2nd instar", "3rd instar", "pupae"),
        pch = c(0,1,2), lty = c(1,3,2), merge=TRUE)
dev.off()



#boxplots 
boxplot (data$DT_egg, data$DT_L1,data$DT_L2,data$DT_L3,data$DT_pupae,data$DT_total)
boxplot (data_13$DT_egg, data_13$DT_L1, data_13$DT_L2, data_13$DT_L3, data_13$DT_pupae, data_13$DT_total)
boxplot (data_12$DT_egg, data_12$DT_L1, data_12$DT_L2, data_12$DT_L3, data_12$DT_pupae, data_12$DT_total)

#plots data (egg, - pupae)
plot(data_13$DT_egg, data_13$egg, xlab='DT [HD]',ylab=' D [hours]', col='red', main="egg")
plot(data$DT_L1, data$L1, xlab='DT [HD]',ylab=' D [hours]', col='red', main="L1")
plot(data$DT_L2, data$L2, xlab='DT [HD]',ylab=' D [hours]', col='red', main="L2")
plot(data$DT_L3,  data$L3, xlab='DT [HD]',ylab=' D [hours]', col='red', main="L3")
plot(data$DT_pupae, data$pupae, xlab='DT [HD]',ylab=' D [hours]', col='red', main="pupae")

plot (data$temp,data$egg)
plot (data$temp,data$L1)
plot (data$temp,data$L2)
plot (data$temp,data$L3)
plot (data$temp,data$pupae)

#normality
#obviously non-normal distribution due to problems in recording the data
qqnorm(data_12$DT_egg) 
qqline(data_12$DT_egg)
#looks normal to me, different data recording
qqnorm(data_13$DT_egg)
qqline(data_13$DT_egg)
qqnorm(data$DT_L1)
qqline(data$DT_L1)
qqnorm(data$DT_L2)
qqline(data$DT_L2)
qqnorm(data$DT_L3)
qqline(data$DT_L3)
qqnorm(data$DT_pupae)
qqline(data$DT_pupae)

#test of normality
shapiro.test(data_13$DT_egg)
shapiro.test(data$DT_L1)
shapiro.test(data$DT_L2)
shapiro.test(data$DT_L3)
shapiro.test(data$DT_pupae)

#hist
hist(data$DT_egg, breaks=10, prob=FALSE, xlab='DT', ylab='Density', main='Histogram of development rate', col='grey')

#Tmin + SET - DT~D
lm.1=lm(data_13$DT_egg~data_13$egg)
summary(lm.1)
lm.2=lm(data_13$DT_L1~data_13$L1)
summary(lm.2)
lm.3=lm(data$DT_L2~data$L2)
summary(lm.3)
lm.4=lm(data$DT_L3 ~data$L3)
summary(lm.4)
lm.5=lm(data$DT_pupae~data$pupae)
summary(lm.5)



plot(data_13$DT_egg~data_13$egg, xlab='Development(h)',
     ylab='Development*Temperature (hD) ', main="Egg", 
     xlim=c(0,300),ylim=c(0,5000))
abline(lm.1, col='green', lwd=1)
plot(data$DT_L1~data$L1, xlab='D',ylab='DT ', main="L1")
abline(lm.2, col='green', lwd=2)
plot(data$DT_L2~data$L2, xlab='D',ylab='DT ', main="L2")
abline(lm.3, col='green', lwd=2)
plot(data$DT_L3 ~data$L3, xlab='D',ylab='DT ', main="L3")
abline(lm.4, col='green', lwd=2)
plot(data$DT_pupae~data$pupae, xlab='D',ylab='DT ', main="pupae")
abline(lm.5, col='green', lwd=2)

# Effect of locality of origin
#regrese (not very well done, still have to work on the imput and output

loc.1=lm(data_13$r_egg~data_13$temp)
summary(loc.1)
(6.045e-03)/(7.503e-04) #no idea what is this for

loc.2=lm(data$r_egg~data$temp+data$loc)
loc.3=lm(data$r_egg~data$temp*data$loc)
summary(loc.2)
AIC (loc.1, loc.2, loc.3)
BIC (loc.1, loc.2, loc.3)

loc.3=lm(data$r_egg~data$temp+data$loc+data$year)
loc.4=lm(data$r_egg~data$temp*data$loc*data$year)
summary(loc.3)
summary(loc.4)

loc.4=lm(data$DT_egg~data$egg)
summary(loc.1)
loc.5=lm(data$DT_egg~data$egg+data$loc)
loc.6=lm(data$DT_egg~data$egg*data$loc)
summary(loc.2)
AIC (loc.4, loc.5, loc.6)
BIC (loc.4, loc.5, loc.6)

loc.4=lm(data_13$DT_egg~data_13$egg)
summary(loc.1)
loc.5=lm(data_13$DT_egg~data_13$egg+data_13$loc)
loc.6=lm(data_13$DT_egg~data_13$egg*data_13$loc)
summary (loc.6)
summary(loc.2)
AIC (loc.4, loc.5, loc.6)
BIC (loc.4, loc.5, loc.6)


plot(data$temp, data$r_egg, xlab='temperature',ylab='development rate', main="vajicko")
lines(loc.1, col='green', lwd=2)
abline(loc.2, col='red', lwd=2)
abline(loc.3, col='blue', lwd=2)
AIC(loc.1,loc.2,loc.3,loc.4)
BIC(loc.1,loc.2,loc.3,loc.4)

loc.5=lm(data_13$r_egg~data_13$temp)
summary(loc.5)
anova (loc.5)
loc.6=lm(data_13$r_egg~data_13$temp+data_13$loc)
summary(loc.6)
loc.7=lm(data_13$r_egg~data_13$temp*data_13$loc)
summary(loc.7)
AIC(loc.5, loc.6, loc.7)
BIC(loc.5, loc.6, loc.7)

plot(data_13$temp[data_13$Herkomstcode==0], data_13$r_egg[data_13$Herkomstcode==0], pch=16, col=c("red"), xlab='temperature',ylab='development rate')
points(data_13$temp[data_13$Herkomstcode==1], data_13$r_egg[data_13$Herkomstcode==1], pch=2, col=c("blue"))
abline(a=nll.her$par[1],b=nll.her$par[3], col='red')
abline(a=nll.her$par[2],b=nll.her$par[3], col='blue')

glm.1=glm(data_13$r_egg~data_13$temp)
glm.2=glm(data_13$r_egg~data_13$temp, family=Gamma)
summary (glm.2)
AIC(glm.1,glm.2)
BIC(glm.1,glm.2)

glm.3=glm(data_13$r_egg~data_13$temp, family=Gamma)
glm.4=glm(data_13$r_egg~data_13$temp*data_13$loc, family=Gamma)
glm.4a=glm(data_13$r_egg~data_13$temp+data_13$loc, family=Gamma)
AIC(glm.3,glm.4,glm.4a)
BIC(glm.3,glm.4,glm.4a)

summary (glm.3)
summary (glm.4)
summary (glm.4a)
par (mfrow=c(1,1))
plot (glm.3)
plot (glm.4)
plot (glm.4a)
plot (data_13$temp, data_13$r_egg, xlim=c(0,40),ylim=c(0,0.1))
abline(glm.3, col='red', lwd=2)
abline(glm.4, col='green', lwd=2)
abline(glm.4a, col='blue', lwd=2)
plot (data_13$r_egg~data_13$temp*data_13$loc)
plot (data_13$r_egg~data_13$temp+data_13$loc)


#Negative log likelihood
nData = length(data$DT_egg)
meanData = mean(data$DT_egg)
varData = var(data$DT_egg)

# 3) Normal distibution for eggs
NLLfunNorm = function(p, x) {
  mean = p[1]
  sd = p[2]
  -sum(dnorm(x, mean = mean, sd = sd, log = TRUE)) 
}

sd(data$DT_egg, na.rm=TRUE)
paramsN = c(607,11.1)  # mean, sd
nllNorm = optim(fn=NLLfunNorm,p=paramsN, x=data$DT_egg)
nllNorm 

hist(data$r_egg, breaks=10, prob=TRUE, xlab='development rate', ylab='density', main='Histogram of development rate',col='grey')
curve(dnorm(x, mean = meanData, sd = sqrt(varData)),add=TRUE, col='green', lwd=2, lty=2)  # true
curve(dnorm(x, mean = nllNorm$par[1], sd = nllNorm$par[2]),add=TRUE, col='green', lwd=2)  # optim
legend('topright', lty=c(2,1), lwd=c(2,2), col=c('green', 'green'), legend=c('true', 'optim'), bty='n')


# confidence limits of our estimation
# finding x-intercepts
library(MASS)
fN = fitdistr(data$r_egg, "normal")
fN
ci.meanN = fN$estimate["mean"] + c(-2, 2) * fN$sd["mean"]
ci.sdN  = fN$estimate["sd"] + c(-2, 2) * fN$sd["sd"]

install.packages (c("dismo", "rgbif","raster", "maptools","XML", "rgdal"))
