rm(list = ls())
#install.packages (c("rgbif", "raster ", "maptools", "XML", "rgdal", "dismo",
#                  "sqldf", "maps ", "testthat","roxygen2",
#                  "celestial", "ggplot2", "rJava"))
#=======================================================================================
#PACKAGES used in this script
library (rgbif) #nefunguje 
library (raster)
library (maptools) # for wrld_simpl
library (rgdal) #nefunguje pod ubuntu
library (dismo)
library (celestial)
library(spocc)


#=======================================================================================
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
#install.packages("celestial")
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
head(coord.unfiltered)
coord.scio <- coord.unfiltered [complete.cases(coord.unfiltered),]
head(coord.scio)
colnames(coord.scio) <- c("long", "lat")
length(coord.scio)
write.table(coord.scio, file= "data/coord.full.csv", row.names=F, dec=".", sep=";")
