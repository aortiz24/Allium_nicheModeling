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
  dplyr::select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target1)
#assign variety name to object
canadense <- (alliumcanadense1)
canadense <-canadense[c(5,10:13,16:20),]

#assign scientific name to an object
target2<-c("Allium canadense var. ecristatum")
#filtered allium canadense ecristatum csv file
alliumcanadense2<-alliumcanadense %>%
  dplyr::select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target2)
#assign variety name to object
ecristatum <- (alliumcanadense2)

#assign scientific name to an object
target3<-c("Allium canadense var. Fraseri")
#filtered allium canadense Fraseri csv file
alliumcanadense3<-alliumcanadense %>%
  dplyr::select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target3)
#assign variety name to object
fraseri <- (alliumcanadense3)

#assign scientific name to an object
target4<-c("Allium canadense var. hyacinthoides")
#filtered allium canadense hyacinthoides csv file
alliumcanadense4<-alliumcanadense %>%
  dplyr::select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target4)
#assign variety name to object
hyacinthoides <- (alliumcanadense4)

#assign scientific name to an object
target5<-c("Allium canadense var. lavendulare")
#filtered allium canadense lavendulare csv file
alliumcanadense5<-alliumcanadense %>%
  dplyr::select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target5)
#assign variety name to object
lavendulare <- (alliumcanadense5)
#remove point that was outside of climate layer extent
lavendulare<-lavendulare[c(1:6),]

#assign scientific name to an object
target6<-c("Allium canadense var. mobilense")
#filtered allium canadense mobilense csv file
alliumcanadense6<-alliumcanadense %>%
  dplyr::select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target6)
#assign variety name to object
mobilense <- (alliumcanadense6)
mobilense <- mobilense[c(3:15),]

#assign parentals(mobilense,fraseri) to a R object
parentals<-alliumcanadense %>%
  dplyr::select(Taxon,Latitude,Longitude)
parentals<-parentals[c(5,10:13,16:20,22:39,43:55),]

#assign hybrids(hyacinthoides,ecristatum,lavendulare) to a R object
hybrids<- alliumcanadense %>%
  dplyr::select(Taxon,Latitude,Longitude)
hybrids<-hybrids[c(5,10:13,16:20,22:39,43:55),]

##prepare varieties,parentals,and hybrids for modeling
canadense <- canadense[,c(3,2)]
lavendulare <- lavendulare[,c(3,2)]
ecristatum<- ecristatum[,c(3,2)]
fraseri<- fraseri[,c(3,2)]
hyacinthoides<- hyacinthoides[,c(3,2)]
mobilense<- mobilense[,c(3,2)]
parentals<- parentals[,c(3,2)]
hybrids<- hybrids[,c(3,2)]

#layers ending in 9 are for PRISM1929
#layers ending in 11 are for PRISM2011
# import layers with CRS specified
CRS <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
ppt9 <- raster("layers/ppt9.asc", crs=CRS)
tmax9 <- raster("layers/tmax9.asc", crs=CRS)
tmean9 <- raster("layers/tmean9.asc", crs=CRS)
tmin9 <- raster("layers/tmin9.asc", crs=CRS)
vpdmax9 <- raster("layers/vpdmax9.asc", crs=CRS)
vpdmin9 <- raster("layers/vpdmin9.asc", crs=CRS)
tdmean9 <- raster("layers/tdmean9.asc", crs=CRS)
ppt11 <- raster("layers/ppt11.asc", crs=CRS)
tmax11 <- raster("layers/tmax11.asc", crs=CRS)
tmean11 <- raster("layers/tmean11.asc", crs=CRS)
tmin11 <- raster("layers/tmin11.asc", crs=CRS)
vpdmax11 <- raster("layers/vpdmax11.asc", crs=CRS)
vpdmin11 <- raster("layers/vpdmin11.asc", crs=CRS)
tdmean11 <- raster("layers/tdmean11.asc", crs=CRS)

## create stack of non-correlated layers (as determined by layerPrep.R)
predictors9<- stack(tmean9, ppt9, vpdmax9)
predictors11<- stack(tmean11, ppt11, vpdmin11, tdmean11)

## Default maxent modeling
# run maxent for canadense (default parameters for dismo)
maxCan9 <- maxent(predictors9, canadense)
maxCan9 # views results in browser window
response(maxCan9) # show response curves for each layer
rCan9 <- predict(maxCan9, predictors9) # create model
plot(rCan9) # plot predictive model
#dev.copy2pdf(file="figures/MaxCanadense1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(canadense) # add points to predictive model
writeRaster(rCan9, "models/canadense1929.grd")

# run maxent for lavendulare (default parameters for dismo)
maxLav9 <- maxent(predictors9, lavendulare)
maxLav9 # views results in browser window
response(maxLav9) # show response curves for each layer
rLav9 <- predict(maxLav9, predictors9) # create model
plot(rLav9) # plot predictive model
#dev.copy2pdf(file="figures/MaxLavendulare1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(lavendulare) # add points to predictive model
writeRaster(rLav9, "models/lavendulare1929.grd")

# run maxent for ecristatum (default parameters for dismo)
maxEcr9 <- maxent(predictors9, ecristatum) 
maxEcr9 # views results in browser window
response(maxEcr9) 
rEcr9 <- predict(maxEcr9, predictors9) 
plot(rEcr9)
#dev.copy2pdf(file="figures/MaxEcristatum1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(ecristatum)
writeRaster(rEcr9, "models/ecristatum1929.grd")

# run maxent for fraseri (default parameters for dismo)
maxFra9 <- maxent(predictors9, fraseri)
maxFra9 # views results in browser window
response(maxFra9) # show response curves for each layer
rFra9 <- predict(maxFra9, predictors9) # create model
plot(rFra9) # plot predictive model
#dev.copy2pdf(file="figures/MaxFraseri1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(fraseri) # add points to predictive model
writeRaster(rFra9, "models/fraseri1929.grd")

# run maxent for hyacinthoides (default parameters for dismo)
maxHya9 <- maxent(predictors9, hyacinthoides) 
maxHya9 
response(maxHya9) 
rHya9 <- predict(maxHya9, predictors9) 
plot(rHya9)
#dev.copy2pdf(file="figures/MaxHyacinthoides1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(hyacinthoides)
writeRaster(rHya9, "models/hyacinthoides1929.grd")

# run maxent for mobilense (default parameters for dismo)
maxMob9 <- maxent(predictors9, mobilense)
maxMob9 # views results in browser window
response(maxMob9) # show response curves for each layer
rMob9 <- predict(maxMob9, predictors9) # create model
plot(rMob9) # plot predictive model
#dev.copy2pdf(file="figures/MaxMobilense1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(mobilense) # add points to predictive model
writeRaster(rMob9, "models/mobilense1929.grd")

# run maxent for parentals (default parameters for dismo)maxPar9 <- maxent(predictors9, parentals)
maxPar9 # views results in browser window
response(maxPar9) # show response curves for each layer
rPar9 <- predict(maxPar9, predictors9) # create model
plot(rPar9) # plot predictive model
#dev.copy2pdf(file="figures/MaxParentals1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(parentals) # add points to predictive model
writeRaster(rPar9, "models/parentals1929.grd")

# run maxent for hybrids (default parameters for dismo)
maxHyb9 <- maxent(predictors9, hybrids) 
maxHyb9 
response(maxHyb9) 
rHyb9 <- predict(maxHyb9, predictors9) 
plot(rHyb9)
#dev.copy2pdf(file="figures/MaxHybrids1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(hybrids)
writeRaster(rHyb9, "models/hybrids1929.grd")

## Advanced modeling
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxCanAdv9 <- maxent(
  x=predictors9,
  p=canadense,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxCanAdv9 #view output as html
dir.create("models/canadense1929Maxent")
# save output files
file.copy(maxCanAdv9@path, "models/canadense1929Maxent/", recursive=TRUE)
response(maxCanAdv9) # show response curves for each layer
rCanAdv9 <- predict(maxCanAdv9, predictors9) # create model
plot(rCanAdv9) # plot predictive model
#dev.copy2pdf(file="figures/AdvCanadense1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(canadense) # add points to predictive model
writeRaster(rCanAdv9, "models/canadenseAdv1929.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxLavAdv9 <- maxent(
  x=predictors9,
  p=lavendulare,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxLavAdv9 #view output as html
dir.create("models/lavendulare1929Maxent")
# save output files
file.copy(maxLavAdv9@path, "models/lavendulare1929Maxent/", recursive=TRUE)
response(maxLavAdv9) # show response curves for each layer
rLavAdv9 <- predict(maxLavAdv9, predictors9) # create model
plot(rLavAdv9) # plot predictive model
#dev.copy2pdf(file="figures/AdvLavendulare1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(lavendulare) # add points to predictive model
writeRaster(rLavAdv9, "models/lavendulareAdv1929.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxEcrAdv9 <- maxent(
  x=predictors9,
  p=ecristatum,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxEcrAdv9 #view output as html
dir.create("models/ecristatum1929Maxent")
# save output files
file.copy(maxEcrAdv9@path, "models/ecristatum1929Maxent/", recursive=TRUE)
response(maxEcrAdv9) # show response curves for each layer
rEcrAdv9 <- predict(maxEcrAdv9, predictors9) # create model
plot(rEcrAdv9) # plot predictive model
#dev.copy2pdf(file="figures/AdvEcristatum1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(ecristatum) # add points to predictive model
writeRaster(rEcrAdv9, "models/ecristatumAdv1929.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxFraAdv9 <- maxent(
  x=predictors9,
  p=fraseri,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxFraAdv9 #view output as html
dir.create("models/fraseri1929Maxent")
# save output files
file.copy(maxFraAdv9@path, "models/fraseri1929Maxent/", recursive=TRUE)
response(maxFraAdv9) # show response curves for each layer
rFraAdv9 <- predict(maxFraAdv9, predictors9) # create model
plot(rFraAdv9) # plot predictive model
#dev.copy2pdf(file="figures/AdvFraseri1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(fraseri) # add points to predictive model
writeRaster(rFraAdv9, "models/fraseriAdv1929.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHyaAdv9 <- maxent(
  x=predictors9,
  p=hyacinthoides,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxHyaAdv9 #view output as html
dir.create("models/hyacinthoides1929Maxent")
# save output files
file.copy(maxHyaAdv9@path, "models/hyacinthoides1929Maxent/", recursive=TRUE)
response(maxHyaAdv9) # show response curves for each layer
rHyaAdv9 <- predict(maxHyaAdv9, predictors9) # create model
plot(rHyaAdv9) # plot predictive model
#dev.copy2pdf(file="figures/AdvHyacinthoides1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(hyacinthoides) # add points to predictive model
writeRaster(rHyaAdv9, "models/hyacinthoidesAdv1929.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxMobAdv9 <- maxent(
  x=predictors9,
  p=mobilense,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxMobAdv9 #view output as html
dir.create("models/mobilense1929Maxent")
# save output files
file.copy(maxMobAdv9@path, "models/mobilense1929Maxent/", recursive=TRUE)
response(maxMobAdv9) # show response curves for each layer
rMobAdv9 <- predict(maxMobAdv9, predictors9) # create model
plot(rMobAdv9) # plot predictive model
#dev.copy2pdf(file="figures/AdvMobilense1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(mobilense) # add points to predictive model
writeRaster(rMobAdv9, "models/mobilenseAdv1929.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxParAdv9 <- maxent(
  x=predictors9,
  p=parentals,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxParAdv9 #view output as html
dir.create("models/parentals1929Maxent")
# save output files
file.copy(maxParAdv9@path, "models/parentals1929Maxent/", recursive=TRUE)
response(maxParAdv9) # show response curves for each layer
rParAdv9 <- predict(maxParAdv9, predictors9) # create model
plot(rParAdv9) # plot predictive model
#dev.copy2pdf(file="figures/AdvParentals1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(parentals) # add points to predictive model
writeRaster(rParAdv9, "models/parentalsAdv1929.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHybAdv9 <- maxent(
  x=predictors9,
  p=hybrids,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxHybAdv9 #view output as html
dir.create("models/hybrids1929Maxent")
# save output files
file.copy(maxHybAdv9@path, "models/hybrids1929Maxent/", recursive=TRUE)
response(maxHybAdv9) # show response curves for each layer
rHybAdv9 <- predict(maxHybAdv9, predictors9) # create model
plot(rHybAdv9) # plot predictive model
#dev.copy2pdf(file="figures/AdvHybrids1929.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(hybrids) # add points to predictive model
writeRaster(rHybAdv9, "models/hybridsAdv1929.grd")

## Default maxent modeling
# run maxent for canadense (default parameters for dismo)
maxCan11 <- maxent(predictors11, canadense)
maxCan11 # views results in browser window
response(maxCan11) # show response curves for each layer
rCan11 <- predict(maxCan11, predictors11) # create model
plot(rCan11) # plot predictive model
#dev.copy2pdf(file="figures/MaxCanadense2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(canadense) # add points to predictive model
writeRaster(rCan11, "models/canadense2011.grd")

# run maxent for lavendulare (default parameters for dismo)
maxLav11 <- maxent(predictors11, lavendulare)
maxLav11 # views results in browser window
response(maxLav11) # show response curves for each layer
rLav11 <- predict(maxLav11, predictors11) # create model
plot(rLav11) # plot predictive model
#dev.copy2pdf(file="figures/MaxLavendulare2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(lavendulare) # add points to predictive model
writeRaster(rLav11, "models/lavendulare2011.grd")

# run maxent for ecristatum (default parameters for dismo)
maxEcr11 <- maxent(predictors11, ecristatum) 
maxEcr11 # views results in browser window
response(maxEcr11) 
rEcr11 <- predict(maxEcr11, predictors11) 
plot(rEcr11)
#dev.copy2pdf(file="figures/MaxEcristatum2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(ecristatum)
writeRaster(rEcr11, "models/ecristatum2011.grd")

# run maxent for fraseri (default parameters for dismo)
maxFra11 <- maxent(predictors11, fraseri)
maxFra11 # views results in browser window
response(maxFra11) # show response curves for each layer
rFra11 <- predict(maxFra11, predictors11) # create model
plot(rFra11) # plot predictive model
#dev.copy2pdf(file="figures/MaxFraseri2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(fraseri) # add points to predictive model
writeRaster(rFra11, "models/fraseri2011.grd")

# run maxent for hyacinthoides (default parameters for dismo)
maxHya11 <- maxent(predictors11, hyacinthoides) 
maxHya11 
response(maxHya11) 
rHya11 <- predict(maxHya11, predictors11) 
plot(rHya11)
#dev.copy2pdf(file="figures/MaxHyacinthoides2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(hyacinthoides)
writeRaster(rHya11, "models/hyacinthoides2011.grd")

# run maxent for mobilense (default parameters for dismo)
maxMob11 <- maxent(predictors11, mobilense)
maxMob11 # views results in browser window
response(maxMob11) # show response curves for each layer
rMob11 <- predict(maxMob11, predictors11) # create model
plot(rMob11) # plot predictive model
#dev.copy2pdf(file="figures/MaxMobilense2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(mobilense) # add points to predictive model
writeRaster(rMob11, "models/mobilense2011.grd")

# run maxent for parentals (default parameters for dismo)
maxPar11 <- maxent(predictors11, parentals)
maxPar11 # views results in browser window
response(maxPar11) # show response curves for each layer
rPar11 <- predict(maxPar11, predictors11) # create model
plot(rPar11) # plot predictive model
#dev.copy2pdf(file="figures/MaxParentals2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(parentals) # add points to predictive model
writeRaster(rPar11, "models/parentals2011.grd")

# run maxent for hybrids (default parameters for dismo)
maxHyb11 <- maxent(predictors11, hybrids) 
maxHyb11 # views results in browser window
response(maxHyb11) # show response curves for each layer
rHyb11 <- predict(maxHyb11, predictors11) # create model
plot(rHyb11) # plot predictive model
#dev.copy2pdf(file="figures/MaxHybrids2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(hybrids) # add points to predictive model
writeRaster(rHyb11, "models/hybrids2011.grd")

## Advanced modeling
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxCanAdv11 <- maxent(
  x=predictors11,
  p=canadense,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxCanAdv11 #view output as html
dir.create("models/canadense2011Maxent")
# save output files
file.copy(maxCanAdv11@path, "models/canadense2011Maxent/", recursive=TRUE)
response(maxCanAdv11) # show response curves for each layer
rCanAdv11 <- predict(maxCanAdv11, predictors11) # create model
plot(rCanAdv11) # plot predictive model
#dev.copy2pdf(file="figures/AdvCanadense2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(canadense) # add points to predictive model
writeRaster(rCanAdv11, "models/canadenseAdv2011.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxLavAdv11 <- maxent(
  x=predictors11,
  p=lavendulare,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxLavAdv11 #view output as html
dir.create("models/lavendulare2011Maxent")
# save output files
file.copy(maxLavAdv11@path, "models/lavendulare2011Maxent/", recursive=TRUE)
response(maxLavAdv11) # show response curves for each layer
rLavAdv11 <- predict(maxLavAdv11, predictors11) # create model
plot(rLavAdv11) # plot predictive model
#dev.copy2pdf(file="figures/AdvLavendulare2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(lavendulare) # add points to predictive model
writeRaster(rLavAdv11, "models/lavendulareAdv2011.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxEcrAdv11 <- maxent(
  x=predictors11,
  p=ecristatum,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxEcrAdv11 #view output as html
dir.create("models/ecristatum2011Maxent")
# save output files
file.copy(maxEcrAdv11@path, "models/ecristatum2011Maxent/", recursive=TRUE)
response(maxEcrAdv11) # show response curves for each layer
rEcrAdv11 <- predict(maxEcrAdv11, predictors11) # create model
plot(rEcrAdv11) # plot predictive model
#dev.copy2pdf(file="figures/AdvEcristatum2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(ecristatum) # add points to predictive model
writeRaster(rEcrAdv11, "models/ecristatumAdv2011.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxFraAdv11 <- maxent(
  x=predictors11,
  p=fraseri,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxFraAdv11 #view output as html
dir.create("models/fraseri2011Maxent")
# save output files
file.copy(maxFraAdv11@path, "models/fraseri2011Maxent/", recursive=TRUE)
response(maxFraAdv11) # show response curves for each layer
rFraAdv11 <- predict(maxFraAdv11, predictors11) # create model
plot(rFraAdv11) # plot predictive model
#dev.copy2pdf(file="figures/AdvFraseri2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(fraseri) # add points to predictive model
writeRaster(rFraAdv11, "models/fraseriAdv2011.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHyaAdv11 <- maxent(
  x=predictors11,
  p=hyacinthoides,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxHyaAdv11 #view output as html
dir.create("models/hyacinthoides2011Maxent")
# save output files
file.copy(maxHyaAdv11@path, "models/hyacinthoides2011Maxent/", recursive=TRUE)
response(maxHyaAdv11) # show response curves for each layer
rHyaAdv11 <- predict(maxHyaAdv11, predictors11) # create model
plot(rHyaAdv11) # plot predictive model
#dev.copy2pdf(file="figures/AdvHyacinthoides2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(hyacinthoides) # add points to predictive model
writeRaster(rHyaAdv11, "models/hyacinthoidesAdv2011.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxMobAdv11 <- maxent(
  x=predictors11,
  p=mobilense,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxMobAdv11 #view output as html
dir.create("models/mobilense2011Maxent")
# save output files
file.copy(maxMobAdv11@path, "models/mobilense2011Maxent/", recursive=TRUE)
response(maxMobAdv11) # show response curves for each layer
rMobAdv11 <- predict(maxMobAdv11, predictors11) # create model
plot(rMobAdv11) # plot predictive model
#dev.copy2pdf(file="figures/AdvMobilense2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(mobilense) # add points to predictive model
writeRaster(rMobAdv11, "models/mobilenseAdv2011.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxParAdv11 <- maxent(
  x=predictors11,
  p=parentals,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxParAdv11 #view output as html
dir.create("models/parentals2011Maxent")
# save output files
file.copy(maxParAdv11@path, "models/parentals2011Maxent/", recursive=TRUE)
response(maxParAdv11) # show response curves for each layer
rParAdv11 <- predict(maxParAdv11, predictors11) # create model
plot(rParAdv11) # plot predictive model
#dev.copy2pdf(file="figures/AdvParentals2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(parentals) # add points to predictive model
writeRaster(rParAdv11, "models/parentalsAdv2011.grd")

# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHybAdv11 <- maxent(
  x=predictors11,
  p=hybrids,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'maximumiterations=1000', #default=500
    "-J" #jackknife = true
  )
)
maxHybAdv11 #view output as html
dir.create("models/hybrids2011Maxent")
# save output files
file.copy(maxHybAdv11@path, "models/hybrids2011Maxent/", recursive=TRUE)
response(maxHybAdv11) # show response curves for each layer
rHybAdv11 <- predict(maxHybAdv11, predictors11) # create model
plot(rHybAdv11) # plot predictive model
#dev.copy2pdf(file="figures/AdvHybrids2011.pdf", width = 7, height = 5) #save plot as pdf to the figures directory
points(hybrids) # add points to predictive model
writeRaster(rHybAdv11, "models/hybridsAdv2011.grd")
