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
mean (data_l1$delka)
mean (data_l2$delka)
mean (data_l3$delka)

sd (data_l1$delka)
sd (data_l2$delka)
sd (data_l3$delka)


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


