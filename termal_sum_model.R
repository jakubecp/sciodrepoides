rm(list = ls())
data = read.csv("Data/cholevinae_full.csv", header=TRUE, sep = ";")
head(data)
summary(data)
class(data$DT_egg)
class (data$temp)
data_12 = data [data$year=="2012",] # data for 2012
data_13 = data [data$year=="2013",] # data for 2013

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


#Plot of thermal summation models on DT data

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

#COMPARISON OF TWO METHODS IN GRAPHICAL WAY
t=11.3997
k=929.3538
lm.2=lm(data_13$r_egg~data_13$temp)
summary (lm.2)
#Plot of thermal summation models on 1/D data FITS perfectly!!!!!!!
plot(data_13$r_egg~data_13$temp, 
     ylab='Developmental rate(1/D)',
     xlab='Temperature (°C) ', 
     main="Egg",
     ylim=c(0,0.02),
     xlim=c(0,25))
abline (a=-(t/k), b=1/k, col='green', lwd=1)
abline (lm.2)


plot(data$DT_L1~data$L1, xlab='D',ylab='DT ', main="L1")
abline(lm.2, col='green', lwd=2)
plot(data$DT_L2~data$L2, xlab='D',ylab='DT ', main="L2")
abline(lm.3, col='green', lwd=2)
plot(data$DT_L3 ~data$L3, xlab='D',ylab='DT ', main="L3")
abline(lm.4, col='green', lwd=2)
plot(data$DT_pupae~data$pupae, xlab='D',ylab='DT ', main="pupae")
abline(lm.5, col='green', lwd=2)

