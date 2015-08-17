rm(list = ls())
data = read.csv("Data/cholevinae_full_interaction.csv", header=TRUE, sep = ";")
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


# interaction plot of individual growth rate
interaction.plot (data$stage, data$individual, data$development)
interaction.plot (data_13$stage, data_13$individual, data_13$development)
interaction.plot (data_12$stage, data_12$individual, data_12$development)
interaction.plot (data_t15$stage, data_t15$individual, data_t15$development)
interaction.plot (data_t18$stage, data_t18$individual, data_t18$development)
interaction.plot (data_t21$stage, data_t21$individual, data_t21$development)
#not enough data for those:
interaction.plot (data_t25$stage, data_13$individual, data_t25$development) 
interaction.plot (data_t28$stage, data_t28$individual, data_t28$development)

#plot of individual growth rate
plot (data$development, data$stage, type="p")
plot (data$development, data$temp)
plot (data$DT~data$r)
