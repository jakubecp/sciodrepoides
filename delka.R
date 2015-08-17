rm(list = ls())
data = read.csv("Data/sciodrepoides_delka_r.csv", header=TRUE, sep = ";")
data_l1 = read.csv ("Data/sciodrepoides_delka_L1.csv", header=TRUE, sep = ";")
data_l2 = read.csv ("Data/sciodrepoides_delka_L2.csv", header=TRUE, sep = ";")
data_l3 = read.csv ("Data/sciodrepoides_delka_L3.csv", header=TRUE, sep = ";")
head(data)
summary(data$instar)
class(data$instar)
class (data$oblast)
class (data$teplota)
data$teplota = as.factor (data$teplota) # for length/temperature models
summary (data)

#plot of individual growth in time
interaction.plot (data$instar, data$jedinec, data$delka)
interaction.plot (data$fotka, data$jedinec, data$delka)
length(data$delka)


# plot of lengths in different instars
tiff (filename="exports/sciodrepoides_length.tiff", width=5000, height=6000, 
      compression="lzw", res= 800)
qplot (data$instar,data$delka,
       geom="boxplot",
      xlab=substitute(Instar),
      ylab=substitute (Length (mm)))
dev.off()

p1= qplot (data_13$egg,data_13$DT_egg,
           xlab=substitute(Development(h)),
           ylab=substitute(Development*Temperature (hD)),
           main="Egg")
p1 + stat_smooth(method="lm", se=TRUE, colour="black")


tiff (filename="exports/sciodrepoides_length.tiff", width=5000, height=6000, 
      compression="lzw", res= 800)
plot (data$delka~data$instar,
      xlab="Instar",
      ylab="Length (mm)")
dev.off()

#number of observations in each instar
length (data_l1$delka)
length (data_l2$delka)
length (data_l3$delka)
#number of individuals in each instar photographed
levels (data$jedinec) #263 individuals were photographed in total
levels (data_l1$jedinec) # 252 individuals were photographed of L1 class
levels (data_l2$jedinec) # 175 individuals were photographed of L2 class
levels (data_l3$jedinec) # 127 individuals were photographed of L3 class

#plot of 
par (mfrow = c(1,2))
plot (data$delka~data$instar)
boxplot (data$delka~data$instar)

library(ggplot2)
qplot(factor(instar), delka, data = data, geom = "boxplot")
#basic characteristics of dataset
mean (data_l1$delka)
mean (data_l2$delka)
mean (data_l3$delka)

sd (data_l1$delka)
sd (data_l2$delka)
sd (data_l3$delka)

#NORMALITA
qqnorm(data_l1$delka)
qqline(data_l1$delka)
qqnorm(data_l2$delka)
qqline(data_l2$delka)
qqnorm(data_l3$delka)
qqline(data_l3$delka)


shapiro.test (data_l1$delka)
shapiro.test (data_l2$delka)
shapiro.test (data_l3$delka)
hist (data_l1$delka)
hist (data_l2$delka)
hist (data_l3$delka)

#Ruzne lin. modely a AIC testy... jen tak blbnuti...
lm.1 = lm(data_l1$delka~data_l1$oblast) 
lm.3 = lm(data_l1$delka~data_l1$oblast*data_l1$teplota)
lm.4 = lm(data_l1$delka~data_l1$teplota) 
lm.5 = lm(data_l2$delka~data_l2$teplota) 
lm.6 = lm(data_l3$delka~data_l3$teplota) 


summary (lm.4)
anova (lm.4)

summary (lm.5)
anova (lm.5)

summary (lm.6)
anova (lm.6)

AIC(lm.1, lm.3, lm.4)
BIC(lm.1, lm.3, lm.4) 

summary (lm.1)
plot (data_l1$delka~data_l1$oblast)


#vychazi ze nevetsi vliv ma oblast, ale je potreba to odfiltrovat, protoze 
#je tam velky vliv rodicovske generace, coz asi nebylo nahodne, ale vliv 
#teploty na velikost zajimavy byt muze a nahodny po odfiltrovani bude.
