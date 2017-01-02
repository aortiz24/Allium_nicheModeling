### running maxent in R
## use ?maxent to check documentation for correct installation of maxent.jar

# load libraries
library(dismo)
library(fields)
library(maps)
library(rgdal)
library(raster)
library(maptools)
library(dplyr)
library(rJava)

# create directory for saving models later
dir.create("models")

### import occurrence data and convert to format required by maxent
##using file made from textbook source
#importing species csv files into R
alliumcanadense<-read.csv("alliumdataset_map_data.csv")

#remove missing data in alliumcanadense
alliumcanadense <- na.omit(alliumcanadense)

#assign scientific name to an object
target1<-c("Allium canadense var. canadense")

#filtered allium canadense canadense csv file
alliumcanadense1<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target1)

#assign scientific name to an object
target2<-c("Allium canadense var. ecristatum")

#filtered allium canadense ecristatum csv file
alliumcanadense2<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target2)

#assign scientific name to an object
target3<-c("Allium canadense var. Fraseri")

#filtered allium canadense Fraseri csv file
alliumcanadense3<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target3)

#assign scientific name to an object
target4<-c("Allium canadense var. hyacinthoides")

#filtered allium canadense hyacinthoides csv file
alliumcanadense4<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target4)

#assign scientific name to an object
target5<-c("Allium canadense var. lavendulare")

#filtered allium canadense lavendulare csv file
alliumcanadense5<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target5)

#assign scientific name to an object
target6<-c("Allium canadense var. mobilense")

#filtered allium canadense mobilense csv file
alliumcanadense6<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target6)

##using file made from herbarium websites
#importing species csv files into R
alliumcanadense0<-read.csv("allium_onlinedataset.csv")

#remove missing data in alliumcanadense
alliumcanadense0 <- na.omit(alliumcanadense0)

#assign scientific name to an object
target7<-c("ecristatum")

#filtered allium canadense ecristatum csv file
alliumcanadense7<-alliumcanadense0 %>%
  select(Subspecies,Collector,Latitude,Longitude) %>%
  filter(Subspecies == target7)

#assign scientific name to an object
target8<-c("fraseri")

#filtered allium canadense fraseri csv file
alliumcanadense8<-alliumcanadense0 %>%
  select(Subspecies,Collector,Latitude,Longitude) %>%
  filter(Subspecies == target8)

#assign scientific name to an object
target9<-c("mobilense")

#filtered allium canadense mobilense csv file
alliumcanadense9<-alliumcanadense0 %>%
  select(Subspecies,Collector,Latitude,Longitude) %>%
  filter(Subspecies == target9)

#assign scientific name to an object
target10<-c("hyacinthoides")

#filtered allium canadense hyacinthoides csv file
alliumcanadense10<-alliumcanadense0 %>%
  select(Subspecies,Collector,Latitude,Longitude) %>%
  filter(Subspecies == target10)

##merging occurrence data
#merging occurrence data for a variety into one R object
ecristatum<- merge(alliumcanadense2,alliumcanadense7, by="Collector",all=TRUE)
fraseri<- merge(alliumcanadense3,alliumcanadense8, by="Collector",all=TRUE)
hyacinthoides <- merge(alliumcanadense4,alliumcanadense10, by="Collector",all=TRUE)
mobilense <- merge(alliumcanadense6,alliumcanadense9, by="Collector",all=TRUE)

#merging occurrence data for parentals(mobilense,fraseri) into one R object
parentals<- merge(mobilense, fraseri, by="Collector", all=TRUE)

#merging occurrence data for hybrids(hyacinthoides,ecristatum,lavendulare) into one R object
hybrid<- merge(hyacinthoides, ecristatum, by="Collector", all=TRUE)
hybrids<- merge(hybrid, alliumcanadense5, by="Collector", all=TRUE)
 
##prepare varieties,parentals,and hybrids for modeling
canadense <- alliumcanadense1[,c(3,2)]
lavendulare <- alliumcanadense5[,c(3,2)]
ecristatum<- ecristatum[,c(3,2)]
fraseri<- fraseri[,c(3,2)]
hyacinthoides<- hyacinthoides[,c(3,2)]
mobilense<- mobilense[,c(3,2)]
parentals<- parentals[,c(3,2)]
hybrids<- hybrids[,c(3,2)]

#layers ending in 0 are for PRISM1930
#layers ending in 1 are for PRISM2014
# import layers with CRS specified
CRS <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
alt <- raster("layers/alt.asc", crs=CRS)
bio2 <- raster("layers/bio2.asc", crs=CRS)
bio3 <- raster("layers/bio3.asc", crs=CRS)
bio5 <- raster("layers/bio5.asc", crs=CRS)
bio6 <- raster("layers/bio6.asc", crs=CRS)
bio8 <- raster("layers/bio8.asc", crs=CRS)
bio9 <- raster("layers/bio9.asc", crs=CRS)
bio12 <- raster("layers/bio12.asc", crs=CRS)
bio13 <- raster("layers/bio13.asc", crs=CRS)
bio14 <- raster("layers/bio14.asc", crs=CRS)
bio19 <- raster("layers/bio19.asc", crs=CRS)
ppt0 <- raster("layers/avg_1930_ppt0.asc", crs=CRS)
tmax0 <- raster("layers/avg_1930_tmax0.asc", crs=CRS)
tmean0 <- raster("layers/avg_1930_tmean0.asc", crs=CRS)
tmin0 <- raster("layers/avg_1930_tmin0.asc", crs=CRS)
ppt1 <- raster("layers/avg_2014_ppt0.asc", crs=CRS)
tmax1 <- raster("layers/avg_2014_tmax0.asc", crs=CRS)
tmean1 <- raster("layers/avg_2014_tmean0.asc", crs=CRS)
tmin1 <- raster("layers/avg_2014_tmin0.asc", crs=CRS)

## create stack of non-correlated layers (as determined by layerPrep.R)
predictors <- stack(alt, bio2, bio3, bio5, bio6, bio8, bio9, bio12, bio13, bio14, bio19)
predictors0<- stack(ppt0)
predictors1<- stack(ppt1,tmax1)

# plot each layer individually
plot(predictors)
plot(predictors0)
plot(predictors1)

