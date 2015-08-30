rm(list = ls())
#data = read.csv("Data/cholevinae_full.csv", header=TRUE, sep = ";") #Hours degrees
data = read.csv("Data/medians.csv", header=TRUE, sep = ";") #Day degrees
head(data)
summary(data)
class(data$DT)
class (data$temp)
data$temp = as.factor(data$temp)
library(ggplot2)
library(gridExtra)

#Tmin + SET - DT~D
lm.1=lm(data$DT[data$stage == "egg"]~data$dev.dur[data$stage == "egg"])
summary(lm.1)
lm.2=lm(data$DT[data$stage == "L1"]~data$dev.dur[data$stage == "L1"])
summary(lm.2)
lm.3=lm(data$DT[data$stage == "L2"]~data$dev.dur[data$stage == "L2"])
summary(lm.3)
lm.4=lm(data$DT[data$stage == "L3"]~data$dev.dur[data$stage == "L3"])
summary(lm.4)
lm.5=lm(data$DT[data$stage == "pupae"]~data$dev.dur[data$stage == "pupae"])
summary(lm.5)

p1= qplot (data$dev.dur[data$stage == "egg"],data$DT[data$stage == "egg"],
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
           main="Egg")
a1 = p1 + stat_smooth(method="lm", se=TRUE, colour="black")

p2= qplot (data$dev.dur[data$stage == "L1"],data$DT[data$stage == "L1"],
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
           main="L1")
a2 = p2 + stat_smooth(method="lm", se=TRUE, colour="black")

p3= qplot (data$dev.dur[data$stage == "L2"],data$DT[data$stage == "L2"],
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
           main="L2")
a3 = p3 + stat_smooth(method="lm", se=TRUE, colour="black")

p4= qplot (data$dev.dur[data$stage == "L3"],data$DT[data$stage == "L3"],
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
           main="L3")
a4 = p4 + stat_smooth(method="lm", se=TRUE, colour="black")

p5= qplot (data$dev.dur[data$stage == "pupae"],data$DT[data$stage == "pupae"],
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
           main="pupae")
a5 = p5 + stat_smooth(method="lm", se=TRUE, colour="black")

tiff (filename="exports/sciodrepoides_development2.tiff", 
      width=8000, height=12000, 
      compression="lzw", res= 800)
grid.arrange (a1,a2, a3, a4, a5, ncol=2)
dev.off()