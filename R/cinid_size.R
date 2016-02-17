rm(list = ls())
library(CINID)
data  <-  read.csv("Data/sciodrepoides_delka_r.csv", header=TRUE, sep = ";")
str(data)
width <- data$delka*1000
mu4 <- mean(data$delka[data$instar == "l3"])*1000
sd4 <- sd (data$delka[data$instar == "l3"])*1000

model <- cinid.table(width, mu4, sd4, threshold = 0.95)
cinid.plot(model, breaks = 50, xlab = "Headcapsules width", ylab1 = "Density",
  ylab2 = "Number of Larvae", main = "")
