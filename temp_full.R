rm(list = ls())
data = read.csv("Data/cholevinae_full.csv", header=TRUE, sep = ";")
head(data)
summary(data)
class(data$DT_egg)
class (data$temp)
data_12 = data [data$year=="2012",] # data for 2012
data_13 = data [data$year=="2013",] # data for 2013
aaa

#boxplots 
boxplot (data$DT_egg, data$DT_L1,data$DT_L2,data$DT_L3,data$DT_pupae,data$DT_total)
boxplot (data_13$DT_egg, data_13$DT_L1, data_13$DT_L2, data_13$DT_L3, data_13$DT_pupae, data_13$DT_total)
boxplot (data_12$DT_egg, data_12$DT_L1, data_12$DT_L2, data_12$DT_L3, data_12$DT_pupae, data_12$DT_total)
#plots of eggs
plot(data$egg, data$r_egg, xlab='development time [day]',ylab='development rate [1/day]', col='red')  # dvrTotalcombi = 1/durTotalcombi
plot(data$temp, data$r_egg, xlab='temperature [?C]',ylab='development rate [1/day]', col='red')
plot(data_13$DT_egg, data_13$egg, xlab='DT [HD]',ylab=' D [hours]', col='red', main="egg")
plot(data_13$DT_L1, data_13$L1, xlab='DT [HD]',ylab=' D [hours]', col='red', main="L1")
plot(data_13$DT_L2, data_13$L2, xlab='DT [HD]',ylab=' D [hours]', col='red', main="L2")
plot(data_13$DT_L3, xlab='DT [HD]',ylab=' D [hours]', col='red', main="L3")
plot(data_13$DT_pupae, data_13$pupae, xlab='DT [HD]',ylab=' D [hours]', col='red', main="pupae")
plot(data_13$DT_total, data_13$total, xlab='DT [HD]',ylab=' D [hours]', col='red', main="total")

plot(data$DT_egg, data$egg, xlab='DT [HD]',ylab=' D [hours]', col='red', main="egg")
plot(data$DT_L1, data$L1, xlab='DT [HD]',ylab=' D [hours]', col='red', main="L1")
plot(data$DT_L2, data$L2, xlab='DT [HD]',ylab=' D [hours]', col='red', main="L2")
plot(data$DT_L3,  data$L3, xlab='DT [HD]',ylab=' D [hours]', col='red', main="L3")
plot(data$DT_pupae, data$pupae, xlab='DT [HD]',ylab=' D [hours]', col='red', main="pupae")
plot(data$DT_total, data$total, xlab='DT [HD]',ylab=' D [hours]', col='red', main="total")

#normality
qqnorm(data$r_egg)
qqline(data$r_egg)
qqnorm(data_12$DT_egg)
qqline(data_12$DT_egg)
qqnorm(data_13$DT_egg)
qqline(data_13$DT_egg)
qqnorm(data_12$r_egg)
qqline(data_12$r_egg)
qqnorm(data_13$r_egg)
qqline(data_13$r_egg)

qqnorm(data$DT_egg)
qqline(data$DT_egg)
qqnorm(data$DT_L1)
qqline(data$DT_L1)
qqnorm(data$DT_L2)
qqline(data$DT_L2)
qqnorm(data$DT_L3)
qqline(data$DT_L3)
qqnorm(data$DT_pupae)
qqline(data$DT_pupae)
qqnorm(data$DT_total)
qqline(data$DT_total)

#test of normality
shapiro.test(data$DT_egg)
shapiro.test(data$DT_L1)
shapiro.test(data$DT_L2)
shapiro.test(data$DT_L3)
shapiro.test(data$DT_pupae)
shapiro.test(data$DT_total)

#hist
hist(data$DT_egg, breaks=10, prob=FALSE, xlab='DT', ylab='Density', main='Histogram of development rate', col='grey')

#Tmin + SET - DT~D
lm.1=lm(data$DT_egg~data$egg)
summary(lm.1)
lm.2=lm(data$DT_L1~data$L1)
summary(lm.2)
lm.3=lm(data$DT_L2~data$L2)
summary(lm.3)
lm.4=lm(data$DT_L3 ~data$L3)
summary(lm.4)
lm.5=lm(data$DT_pupae~data$pupae)
summary(lm.5)
lm.6=lm(data$DT_total~data$total) #nema smysl resit, je tam velky error a odporuje si to s namerenymi daty
summary(lm.6)


plot(data$DT_egg~data$egg, xlab='Development(h)',
     ylab='Development*Temperature (hD) ', main="Egg", 
     xlim=c(0,300),ylim=c(0,5000))
abline(lm.1, col='green', lwd=2)
plot(data$DT_L1~data$L1, xlab='D',ylab='DT ', main="L1")
abline(lm.2, col='green', lwd=2)
plot(data$DT_L2~data$L2, xlab='D',ylab='DT ', main="L2")
abline(lm.3, col='green', lwd=2)
plot(data$DT_L3 ~data$L3, xlab='D',ylab='DT ', main="L3")
abline(lm.4, col='green', lwd=2)
plot(data$DT_pupae~data$pupae, xlab='D',ylab='DT ', main="pupae")
abline(lm.5, col='green', lwd=2)

# Effect of locality of origin
#regrese
loc.1=lm(data_13$r_egg~data_13$temp)
summary(loc.1)

(6.045e-03)/(7.503e-04)

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
