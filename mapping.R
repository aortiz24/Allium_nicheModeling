### making maps and preparing shapefiles

## load libraries
library(fields)
library(dplyr)
library(dismo)
library(maptools) 

##using file made from textbook source
#importing species csv files into R
alliumcanadense<-read.csv("alliumdataset_map_data.csv")

#remove missing data in alliumcanadense
alliumcanadense <- na.omit(alliumcanadense)

#assign scientific name to an object
target1<-c("Allium canadense var. canadense")

#filtered allium canadense canadense csv file
alliumcanadense1<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target1)

#assign scientific name to an object
target2<-c("Allium canadense var. ecristatum")

#filtered allium canadense ecristatum csv file
alliumcanadense2<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target2)

#assign scientific name to an object
target3<-c("Allium canadense var. Fraseri")

#filtered allium canadense Fraseri csv file
alliumcanadense3<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target3)

#assign scientific name to an object
target4<-c("Allium canadense var. hyacinthoides")

#filtered allium canadense hyacinthoides csv file
alliumcanadense4<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target4)

#assign scientific name to an object
target5<-c("Allium canadense var. lavendulare")

#filtered allium canadense lavendulare csv file
alliumcanadense5<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target5)

#assign scientific name to an object
target6<-c("Allium canadense var. mobilense")

#filtered allium canadense mobilense csv file
alliumcanadense6<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
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
  select(Subspecies,Latitude,Longitude) %>%
  filter(Subspecies == target7)

#assign scientific name to an object
target8<-c("fraseri")

#filtered allium canadense fraseri csv file
alliumcanadense8<-alliumcanadense0 %>%
  select(Subspecies,Latitude,Longitude) %>%
  filter(Subspecies == target8)

#assign scientific name to an object
target9<-c("mobilense")

#filtered allium canadense mobilense csv file
alliumcanadense9<-alliumcanadense0 %>%
  select(Subspecies,Latitude,Longitude) %>%
  filter(Subspecies == target9)

#assign scientific name to an object
target10<-c("hyacinthoides")

#filtered allium canadense hyacinthoides csv file
alliumcanadense10<-alliumcanadense0 %>%
  select(Subspecies,Latitude,Longitude) %>%
  filter(Subspecies == target10)

## quick and dirty plot on map (could also plot points first and add map)
US(xlim=c(-85,-77), ylim=c(26,37))
points(alliumcanadense1$Longitude, alliumcanadense1$Latitude, col='purple', pch=20, cex=2)
points(alliumcanadense5$Longitude, alliumcanadense5$Latitude, col='red', pch=20, cex=2)
points(alliumcanadense2$Longitude, alliumcanadense2$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense7$Longitude, alliumcanadense7$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense3$Longitude, alliumcanadense3$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense8$Longitude, alliumcanadense8$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense4$Longitude, alliumcanadense4$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense10$Longitude, alliumcanadense10$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense6$Longitude, alliumcanadense6$Latitude, col='dark green', pch=20, cex=2)
points(alliumcanadense9$Longitude, alliumcanadense9$Latitude, col='dark green', pch=20, cex=2)

## a slightly more refined map (using built-in state outlines)
midUS <- c("texas", "louisiana", "oklahoma", "arkansas", "kansas", "missouri", "nebraska", "iowa")
map(database="state", regions = midUS, interior=T, lwd=2)
points(alliumcanadense1$Longitude, alliumcanadense1$Latitude, col='purple', pch=20, cex=2)
points(alliumcanadense5$Longitude, alliumcanadense5$Latitude, col='red', pch=20, cex=2)
points(alliumcanadense2$Longitude, alliumcanadense2$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense7$Longitude, alliumcanadense7$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense3$Longitude, alliumcanadense3$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense8$Longitude, alliumcanadense8$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense4$Longitude, alliumcanadense4$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense10$Longitude, alliumcanadense10$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense6$Longitude, alliumcanadense6$Latitude, col='dark green', pch=20, cex=2)
points(alliumcanadense9$Longitude, alliumcanadense9$Latitude, col='dark green', pch=20, cex=2)

## using US census shapefiles, save custom (best) shapefile for modeling later
# download, unzip all state shapefiles to new directory
download.file("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_state_20m.zip", "cb_2015_us_state_20m.zip")
dir.create("shapefiles")
unzip("cb_2015_us_state_20m.zip", exdir="shapefiles")
# load shapefiles and set projection
state <- readShapePoly("shapefiles/cb_2015_us_state_20m.shp") 
projection(state) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
# extract shapefiles of interest and save to file 
midUSCap <- c("Texas", "Louisiana", "Oklahoma", "Arkansas", "Kansas", "Missouri", "Nebraska", "Iowa")
middleUS <- state[as.character(state@data$NAME) %in% midUSCap, ]
writeSpatialShape(middleUS, "shapefiles/middleUS")

# map using custom shapefile and save to file
dir.create("figures")
pdf(file="figures/midUSmapping1.pdf")
map(middleUS)
points(alliumcanadense1$Longitude, alliumcanadense1$Latitude, col='purple', pch=20, cex=2)
points(alliumcanadense5$Longitude, alliumcanadense5$Latitude, col='red', pch=20, cex=2)
points(alliumcanadense2$Longitude, alliumcanadense2$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense7$Longitude, alliumcanadense7$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense3$Longitude, alliumcanadense3$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense8$Longitude, alliumcanadense8$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense4$Longitude, alliumcanadense4$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense10$Longitude, alliumcanadense10$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense6$Longitude, alliumcanadense6$Latitude, col='dark green', pch=20, cex=2)
points(alliumcanadense9$Longitude, alliumcanadense9$Latitude, col='dark green', pch=20, cex=2)
dev.off()
