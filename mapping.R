### making maps and preparing shapefiles

## load libraries
library(fields)
library(dplyr)
library(dismo)
library(maptools) 

##using file made from Ownbey textbook source
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

## quick and dirty plot on map (could also plot points first and add map)
US(xlim=c(-85,-77), ylim=c(26,37))
points(alliumcanadense1$Longitude, alliumcanadense1$Latitude, col='purple', pch=20, cex=2)
points(alliumcanadense2$Longitude, alliumcanadense2$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense3$Longitude, alliumcanadense3$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense4$Longitude, alliumcanadense4$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense5$Longitude, alliumcanadense5$Latitude, col='red', pch=20, cex=2)
points(alliumcanadense6$Longitude, alliumcanadense6$Latitude, col='dark green', pch=20, cex=2)


## a slightly more refined map (using built-in state outlines)
midUS <- c("texas", "louisiana", "oklahoma", "arkansas", "kansas", "missouri", "nebraska", "iowa")
map(database="state", regions = midUS, interior=T, lwd=2)
points(alliumcanadense1$Longitude, alliumcanadense1$Latitude, col='purple', pch=20, cex=2)
points(alliumcanadense2$Longitude, alliumcanadense2$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense3$Longitude, alliumcanadense3$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense4$Longitude, alliumcanadense4$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense5$Longitude, alliumcanadense5$Latitude, col='red', pch=20, cex=2)
points(alliumcanadense6$Longitude, alliumcanadense6$Latitude, col='dark green', pch=20, cex=2)

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
pdf(file="figures/midUSmappingOwnbey.pdf")
map(middleUS)
points(alliumcanadense1$Longitude, alliumcanadense1$Latitude, col='purple', pch=20, cex=2)
points(alliumcanadense2$Longitude, alliumcanadense2$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense3$Longitude, alliumcanadense3$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense4$Longitude, alliumcanadense4$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense5$Longitude, alliumcanadense5$Latitude, col='red', pch=20, cex=2)
points(alliumcanadense6$Longitude, alliumcanadense6$Latitude, col='dark green', pch=20, cex=2)
dev.off()

##Revision to eliminate states with less than 3 occurrence points
# load shapefiles and set projection to revised shapefile of 4 states
state <- readShapePoly("shapefiles/cb_2015_us_state_20m.shp") 
projection(state) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
# extract shapefiles of interest and save to file 
midUSCap1 <- c("Texas", "Oklahoma", "Kansas", "Missouri")
middleUSrevised <- state[as.character(state@data$NAME) %in% midUSCap1, ]
writeSpatialShape(middleUSrevised, "shapefiles/middleUSrevised")

# map using custom shapefile of 4 states and save to file
dir.create("figures")
pdf(file="figures/midUSmappingRevised.pdf")
map(middleUSrevised)
points(alliumcanadense1$Longitude, alliumcanadense1$Latitude, col='purple', pch=20, cex=2)
points(alliumcanadense2$Longitude, alliumcanadense2$Latitude, col='orange', pch=20, cex=2)
points(alliumcanadense3$Longitude, alliumcanadense3$Latitude, col='blue', pch=20, cex=2)
points(alliumcanadense4$Longitude, alliumcanadense4$Latitude, col='gray', pch=20, cex=2)
points(alliumcanadense5$Longitude, alliumcanadense5$Latitude, col='red', pch=20, cex=2)
points(alliumcanadense6$Longitude, alliumcanadense6$Latitude, col='dark green', pch=20, cex=2)
dev.off()
