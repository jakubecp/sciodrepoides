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

library(ggplot2)

#NORMALITA
qqnorm(data_l1$delka)
qqline(data_l1$delka)
qqnorm(data_l2$delka)
qqline(data_l2$delka)
qqnorm(data_l3$delka)
qqline(data_l3$delka)

#basic characteristics of dataset
a1=mean (data_l1$delka)
a2=mean (data_l2$delka)
a3=mean (data_l3$delka)

s1=sd (data_l1$delka)
s2=sd (data_l2$delka)
s3=sd (data_l3$delka)

n1=length (data_l1$delka)
n2=length (data_l2$delka)
n3=length (data_l3$delka)

error <- qt(0.975,df=n1-1)*s1/sqrt(n1)

IQR(data_l1$delka, na.rm = TRUE, type = 1)
IQR(data_l2$delka, na.rm = TRUE, type = 1)
IQR(data_l3$delka, na.rm = TRUE, type = 1)
#data renaming
#renaming factors for legend
dm = data
str(dm)
levels (dm$instar)[levels(dm$instar)=="l1"] = "L1"
levels (dm$instar)[levels(dm$instar)=="l2"] = "L2"
levels (dm$instar)[levels(dm$instar)=="l3"] = "L3"
levels (dm$instar)[levels(dm$instar)=="adult"] = "Pupae"
names(dm)[names(dm)=="instar"] = "Stage"

# plot of lengths in different instars
tiff (filename="exports/sciodrepoides_length.tiff", width=5000, height=6000, 
      compression="lzw", res= 800)
qplot (dm$Stage,dm$delka,
       geom="boxplot",
      xlab=substitute("Instar"),
      ylab=substitute ("Width (mm)"))
dev.off()


# tiff (filename="exports/sciodrepoides_length.tiff", width=5000, height=6000, 
#       compression="lzw", res= 800)
# plot (data$delka~data$instar,
#       xlab="Instar",
#       ylab="Length (mm)")
# dev.off()

#number of observations in each instar
length (data_l1$delka)
length (data_l2$delka)
length (data_l3$delka)
#number of individuals in each instar photographed
levels (data$jedinec) #263 individuals were photographed in total
levels (data_l1$jedinec) # 252 individuals were photographed of L1 class
levels (data_l2$jedinec) # 175 individuals were photographed of L2 class
levels (data_l3$jedinec) # 127 individuals were photographed of L3 class

#maximum and minimum width of each instar
summary (data_l1$delka)
summary (data_l2$delka)
summary (data_l3$delka)


#larvae from Jan Ruzicka breed together on fish...
data_outside = read.csv("Data/data_s.watsoni_outside.csv", header=TRUE, sep = ";")
summary (data_outside)
L1= c(0.37, 0.36, 0.38, 0.35, 0.36, 0.37,0.36, 0.38, 0.36)
mean(data_outside$size[data_outside$instar=="L1"],na.rm = TRUE )
mean(data_outside$size[data_outside$instar=="L2"],na.rm = TRUE )
mean(data_outside$size[data_outside$instar=="L3"],na.rm = TRUE )
median(data_outside$size[data_outside$instar=="L1"],na.rm = TRUE )
median(data_outside$size[data_outside$instar=="L2"],na.rm = TRUE )
median(data_outside$size[data_outside$instar=="L3"],na.rm = TRUE )
sd(data_outside$size[data_outside$instar=="L1"],na.rm = TRUE )
sd(data_outside$size[data_outside$instar=="L2"],na.rm = TRUE )
sd(data_outside$size[data_outside$instar=="L3"],na.rm = TRUE )
sd (L1)
min (L1)
max (L1)
