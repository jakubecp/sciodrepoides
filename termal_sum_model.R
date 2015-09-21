rm(list = ls())
data = read.csv("Data/cholevinae_full.csv", header=TRUE, sep = ";") #Hours degrees
#data = read.csv("Data/cholevinae_full_dd1.csv", header=TRUE, sep = ";") #Day degrees
head(data)
summary(data)
class(data$DT_egg)
class (data$temp)
data_12 = data [data$year=="2012",] # data for 2012
data_13 = data [data$year=="2013",] # data for 2013

library(ggplot2)
library(gridExtra)

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
anova (lm.5)
summary(lm.5)


#Plot of thermal summation models on DT data
p1= qplot (data_13$egg,data_13$DT_egg,
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
       main="Egg")
a1 = p1 + stat_smooth(method="lm", se=TRUE, colour="black")

p2= qplot (data_13$L1,data_13$DT_L1,
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
           main="L1")
a2= p2 + stat_smooth(method="lm", se=TRUE, colour="black")

p3= qplot (data$L2,data$DT_L2,
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
           main="L2")
a3= p3 + stat_smooth(method="lm", se=TRUE, colour="black")

p4= qplot (data$L3,data$DT_L3,
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
           main="L3")
a4= p4 + stat_smooth(method="lm", se=TRUE, colour="black")

p5= qplot (data$pupae,data$DT_pupae,
           xlab=substitute("Hours"),
           ylab=substitute("DT"),
           main="Pupae")
a5= p5 + stat_smooth(method="lm", se=TRUE, colour="black")
#tiff (filename="exports/sciodrepoides_development.tiff", 
#      width=8000, height=12000, 
#      compression="lzw", res= 800)
grid.arrange (a1,a2, a3, a4, a5, ncol=3)
#dev.off()

#COMPARISON OF TWO METHODS IN GRAPHICAL WAY
t=11.3997
k=929.3538
lm.2=lm(data_13$r_egg~data_13$temp)
summary (lm.2)
#Plot of thermal summation models on 1/D 
plot(data_13$r_egg~data_13$temp, 
     ylab='Developmental rate(1/D)',
     xlab='Temperature (°C) ', 
     main="Egg",
     ylim=c(0,0.02),
     xlim=c(0,25))
abline (a=-(t/k), b=1/k, col='green', lwd=1)
abline (lm.2)
