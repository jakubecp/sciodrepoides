rm(list = ls())
data = read.csv("Data/sciodrepoides_delka_r.csv", header=TRUE, sep = ";")
head(data)
summary(data$instar)
class(data$instar)
class (data$oblast)

#SPLIT dat podle instaru
data_l1 = data[data$instar == "l1",]
data_l2 = data[data$instar == "l2",]
data_l3 = data[data$instar == "l3",]
par (mfrow = c(1,2))
plot (data$delka~data$instar)
boxplot (data$delka~data$instar)

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

AIC(lm.1, lm.3, lm.4)
BIC(lm.1, lm.3, lm.4) 

#vychazi ze nevetsi vliv ma oblast, ale je potreba to odfiltrovat, protoze 
#je tam velky vliv rodicovske generace, coz asi nebylo nahodne, ale vliv 
#teploty na velikost zajimavy byt muze a nahodny po odfiltrovani bude.
