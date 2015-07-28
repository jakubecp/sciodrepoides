rm(list = ls())
install.packages(c("rgbif", "raster", "maptools", "XML", "rgdal", "dismo", 
                 "sqldf", "maps", "testthat", "adehabitatsHS", 
                 "roxygen2", "spocc","rworldmap", "ggplot2", "rJava"))
install.packages("rgbif")
library (rgbif) #mrtvy
library (raster) #OK
library (maptools) #nevim
library (XML) #mrtvy
library (rgdal) #mrtvy
library (dismo) #OK
library (sqldf) #OK
library (maps) #OK
library (testthat) #OK
library (adehabitatHS) #OK
library (roxygen2) #OK
library (spocc) #mrtvy
library (rJava) #OK
library (rworldmap) #OK
newmap = getMap(resolution="low") #function from package rworldmap

# #RAW data from GBIF (only records with coordinates and you should set up upper limit of them)
# Sciod.watsoni<- occ_search(scientificName = "Sciodrepoides watsoni",
#                            hasCoordinate= TRUE, limit = 3215)
# #coordinates of observations (filter out NAs and obvious mistakes!)
# coord <- data.frame (long = Sciod.watsoni$data$decimalLongitude ,
#                      lat= Sciod.watsoni$data$decimalLatitude)
# X11()
# plot (coord)
# plot (newmap, add=T)

## SPOCC SEARCH - zmenit na Sciodrepoides watsoni az na to bude cas a asi 
#sloucit s daty z CR
scio.gbif= occ (query="Sciodrepoides watsoni", 
                    from="gbif", 
                 gbifopts=list(hasCoordinate=TRUE)
                    ,limit= 5000)

# sciod.bison = occ (query="Sciodrepoides watsoni", 
#                    from="bison", 
#                    bisonopts=list(hasCoordinate=TRUE),
#                    limit= 5000)


scio.gbif=occ2df(scio.gbif)
# scio.bison=occ2df(sciod.bison)

coord.gbif = data.frame (long=scio.gbif$longitude,
                          lat=scio.gbif$latitude)
# coord.bison = data.frame (long=scio.bison$longitude,
#                           lat=scio.bison$latitude)

# ## solution for transformation of DMS to decimal degrees
## with celestila packages and function dms2deg
install.packages("celestial")
library (celestial)

data.sw <- read.csv("Data/sc.watsoni.csv", header=TRUE, sep=";")
str (data.sw)
head (data.sw)
n=length(data.sw$lat)-7
cz.lat = c()
cz.long = c()
for (i in 1:n) {
  cz.lat[i]=dms2deg (as.character (data.sw[i,30]), sep="DMS") #DMS format is still wrong in source
  cz.long[i]=dms2deg (as.character (data.sw[i,31]), sep="DMS")
}

#made a data.frame from my data (they were in right format), Jan's data
#and Gbif data
coord.sc <- data.frame (long=data.sw$long[40:46], lat=data.sw$lat[40:46])
coord.sc2 <- data.frame (long = cz.long, lat = cz.lat)
coord.full <- rbind (coord.sc2, coord.sc)
coord <- rbind (coord.full, coord.gbif)
str (coord)
coord.long <- as.numeric (coord$long)
coord.lat <- as.numeric (coord$lat)
coord.unfiltered <- cbind (coord.long, coord.lat)
X11()
plot (newmap, xlim=c(-10,55), ylim=c(40,65))
points (coord.unfiltered, col="blue")

#choose the right (important) climatic variables (http://www.worldclim.org/bioclim) 
#for your species and stack them! USE ENFA (package adehabitat) for selection of the 
#right variables if you do not know a lot about them
setwd ("C:/Users/pavel/Downloads/Vzdelavani/Spatial_modeling/ENM_2015_Varela/climatic_layers/worldclim/") #home
setwd ("F:/Spatial_modeling/ENM_2015_Varela/climatic_layers/worldclim/") #university
?enfa
variable<- stack (c("bio1.bil", "bio12.bil")) 
#original values  c("bio10.bil", "bio18.bil", "bio8.bil", "bio16.bil")

#optional-if you are interested in more local (and quicker) predictions 
#make an object (e) of certain extant (xmin, xmax, ymin, ymax) for croping
e<-extent (-20,60,30,75)
#crop your climatic maps
variable_crop<- crop (variable, e)

#Project niches of target species by extracting values from raster and ploting them
niche <- extract (variable_crop, coord.unfiltered)
niche <- as.data.frame (niche)
X11()
par (mfrow=c(1,2))
plot (niche$bio18, niche$bio10, xlab= "prectip of warmest qrt" , ylab= "temp warmest qurt" )
plot (niche$bio16, niche$bio8 , xlab= "precip of wettest qrt" , ylab= "temp of wettest qrt" )


## FILTERING biased observations
#run envSample_Sarah_function.R
coord <- envSample (coord.unfiltered, 
                              filters=list (niche$bio1, niche$bio12),
                              res=list(100,10), do.plot=TRUE)



# MAXENT model (basic setup) - creates values of the model,
# which are used in checking the behavior of the model 
# and making predictions (fallowing steps) 
maxent_all <- maxent (variable_crop, coord, args=c("maximumbackground=1000",
                                                   "betamultiplier=1",
                                                   "defaultprevalence=0.5"))
maxent_all2 <- maxent (variable_crop, coord, args=c("maximumbackground=1000",
                                                    "betamultiplier=2",
                                                    "defaultprevalence=0.5"))
maxent_all5 <- maxent (variable_crop, coord, args=c("maximumbackground=1000",
                                                    "betamultiplier=5",
                                                    "defaultprevalence=0.5"))
#check the behavior of your data to variables (graph) and play
#with "betamultiplier" for smoother model of climatic variables (values= 1 - inf)
X11()
response (maxent_all)
response (maxent_all2)
response (maxent_all5)

#all values
maxent_all@results

#just AUC
maxent_all@results[5]
maxent_all2@results[5]
maxent_all5@results[5]

#Predict probability of occurence
maxent_all_predict<- predict (maxent_all, variable_crop)

#Plot the prediction20,70,30,70
X11()
plot (maxent_all_predict, 
      main="Sciodrepoides watsoni distribution (Maxent/all)", 
      xlim =c(-20,90),ylim=c(30,80) )
plot (newmap, add=TRUE, xlim=c(-20,90),ylim=c(30,80))

#reclasification reclasification (based on maximum training sensitivityplus 
#specificity logistic treshold)
m = c(0.3800,1,1,0,0.3800,0)
rclmat = matrix (m,ncol=3,byrow=TRUE)
sciod_reclas<- reclassify (maxent_all_predict, rclmat)

X11()
plot (sciod_reclas, 
      main="Sciodrepoides watsoni distribution (Maxent/all)")
plot (newmap, add=TRUE)
points (coord.unfiltered, col="blue")


#experiments with maps - This is IT!!!
library (ggplot2)
library (rJava)
library (rworldmap)
newmap = getMap(resolution="low")
X11()
plot (maxent_all_predict, legend=F, xlim=c(-20,90), ylim=c(30,80))
plot (newmap, xlim=c(-20,90), ylim=c(30,80), add=T)

##EXport to TIFF
getwd()
setwd ("Projects/sciodrepoides/sciodrepoides/") #skola
tiff (filename="sciodrepoides.tiff", width=5000, height=5000, 
      compression="lzw", res= 800)
plot (maxent_all_predict, legend=F, xlim=c(-20,90), ylim=c(30,80), axes=FALSE, bty="n")
plot (newmap, xlim=c(-20,90), ylim=c(30,80), add=T, axes=FALSE, bty="n")
dev.off()
#EVALUATION OF THE MAXENT MODEL
#crete object with random split of the data into k(5) subsamples by kfold
fold <- kfold(coord,k=5)
#Create training subdataset of 80% of the data for modeling by selecting 
#everything except the number one ((100%/5)*4=80%)
occtrain <- coord[fold !=1,]
#Create testing subdataset of 20% of the data to validate the model by selecting
#only the number one
occtest <- coord [fold==1,]

# MAXENT model (basic setup) for training data
maxent_occtrain <- maxent (variable_crop, occtrain, args=c("maximumbackground=1000",
                                                           "betamultiplier=5",
                                                           "defaultprevalence=0.5"))
#Prediction for training data
maxent_occtrain_predict <- predict (maxent_occtrain, variable_crop )

#PLotting training subdataset vs. whole dataset
x11()
plot (maxent_all_predict, main="Sciodrepoides watsoni distribution (Maxent/all)")
plot (newmap, add=TRUE)
x11()
plot (maxent_occtrain_predict, main="Sciodrepoides watsoni distribution (Maxent/training)")
plot (newmap, add=TRUE)

#what is discriminant value (AUC)
maxent_occtrain@results[5]
#what is maximum training sensitivityplus specificity logistic treshold 
#aka where to split
maxent_occtrain@results
#what is dimmension of my data to enter right number of random points
dim(occtest)

#testing on pseudoabsences (number 100 is estimated based on the dim value)
pseudoabsence <- randomPoints (variable_crop, 650)
x11()
plot (pseudoabsence)
plot (newmap,add=T)
points (occtest, col="red")

#evaluation compare the value of AUC 
#from this evaluate_nicveo with value of training set 
#evaluate will compere with random pseudoabsence set and 
#come with new AUC = compare it with maxent_training_nicveo@results[5]

evaluate_maxent_occtrain <- evaluate (maxent_occtrain,
                                      p=occtest, a=pseudoabsence,
                                      x=variable_crop)

#Evaluate AUC between these two models:
evaluate_maxent_occtrain
maxent_occtrain@results[5]

#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE
#DONE

#predicting in the past by function predict (every prediction should be run on the current 
#data and then the prediction for the future or the past - use the same mathematical model 
#for both)
setwd ("C:/Users/pavel/Downloads/Vzdelavani/Spatial_modeling/ENM_2015_Varela/climatic_layers/
       CCSM_21/")
variable21<- stack (c("bio10.bil", "bio18.bil", "bio8.bil", "bio16.bil"))
glac_crop <- crop (variable21, e)
nic_glac <- predict (maxent_occtrain, glac_crop)
X11()
plot (nic_glac, main="Sciodrepoides watsoni in the last glacial (Maxent/training)")
plot (newmap,add=T)

#other maps of climate models (ECOclim - past layers )
#future MIROC_21 ()
setwd ("C:/Users/pavel/Downloads/Spatial_modeling/ENM_2015_Varela/climatic_layers/MIROC_21/")
variable22<- stack (c("bio10.bil", "bio18.bil", "bio8.bil", "bio16.bil"))
fut_crop <- crop (variable22, e)
nic_fut <- predict (maxent_occtrain, fut_crop)
x11()
plot (nic_fut, main="Sciodrepoides watsoni MIROC_21 (Maxent/training)")
plot (newmap,add=T)


##ADITIONAL STUFF
#extract = take values from raster
niche <- extract (var_imp_crop, coord)
niche <- as.data.frame (niche)
plot (niche$bio18, niche$bio10, xlab= "prectip of warmest qrt" , ylab= "temp warmest qurt" )
plot (niche$bio16, niche$bio8 , xlab= "precip of wettest qrt" , ylab= "temp of wettest qrt" )

#reclassify to places where it is and where it is not (1 or 0)
nicveo_reclas<- reclassify (map_nicveo_new, c(0,1,1))
plot (nicveo_reclas)
points (nicveo$data$decimalLongitude [nicveo$data$decimalLongitude>0],
        nicveo$data$decimalLatitude[nicveo$data$decimalLongitude>0], 
        pch=16, col="red", main = "wettest" )
