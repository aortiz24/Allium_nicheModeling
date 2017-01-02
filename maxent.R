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
target1<-c("Allium lavendulare var. lavendulare")

#filtered allium lavendulare lavendulare csv file
alliumcanadense1<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target1)

#assign scientific name to an object
target2<-c("Allium lavendulare var. ecristatum")

#filtered allium lavendulare ecristatum csv file
alliumcanadense2<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target2)

#assign scientific name to an object
target3<-c("Allium lavendulare var. Fraseri")

#filtered allium lavendulare Fraseri csv file
alliumcanadense3<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target3)

#assign scientific name to an object
target4<-c("Allium lavendulare var. hyacinthoides")

#filtered allium lavendulare hyacinthoides csv file
alliumcanadense4<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target4)

#assign scientific name to an object
target5<-c("Allium lavendulare var. lavendulare")

#filtered allium lavendulare lavendulare csv file
alliumcanadense5<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target5)

#assign scientific name to an object
target6<-c("Allium lavendulare var. mobilense")

#filtered allium lavendulare mobilense csv file
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

#filtered allium lavendulare ecristatum csv file
alliumcanadense7<-alliumcanadense0 %>%
  select(Subspecies,Collector,Latitude,Longitude) %>%
  filter(Subspecies == target7)

#assign scientific name to an object
target8<-c("fraseri")

#filtered allium lavendulare fraseri csv file
alliumcanadense8<-alliumcanadense0 %>%
  select(Subspecies,Collector,Latitude,Longitude) %>%
  filter(Subspecies == target8)

#assign scientific name to an object
target9<-c("mobilense")

#filtered allium lavendulare mobilense csv file
alliumcanadense9<-alliumcanadense0 %>%
  select(Subspecies,Collector,Latitude,Longitude) %>%
  filter(Subspecies == target9)

#assign scientific name to an object
target10<-c("hyacinthoides")

#filtered allium lavendulare hyacinthoides csv file
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
lavendulare <- alliumcanadense1[,c(3,2)]
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

### basic bioclim modeling with Bioclim layers for varieties, parentals, and hybrids
# extract layer data for each point
canPts <- extract(predictors, lavendulare)
# create bioclim model
canBC <- bioclim(canPts)
# predict bioclim model
canBCpredict <- predict(predictors, canBC)
# plot bioclim model
plot(canBCpredict)

# extract layer data for each point
lavPts <- extract(predictors, lavendulare)
# create bioclim model
lavBC <- bioclim(lavPts)
# predict bioclim model
lavBCpredict <- predict(predictors, lavBC)
# plot bioclim model
plot(lavBCpredict)

# extract layer data for each point
ecrPts <- extract(predictors, ecristatum)
# create bioclim model
ecrBC <- bioclim(ecrPts)
# predict bioclim model
ecrBCpredict <- predict(predictors, ecrBC)
# plot bioclim model
plot(ecrBCpredict)

# extract layer data for each point
fraPts <- extract(predictors, fraseri)
# create bioclim model
fraBC <- bioclim(fraPts)
# predict bioclim model
fraBCpredict <- predict(predictors, fraBC)
# plot bioclim model
plot(fraBCpredict)

# extract layer data for each point
hyaPts <- extract(predictors, hyacinthoides)
# create bioclim model
hyaBC <- bioclim(hyaPts)
# predict bioclim model
hyaBCpredict <- predict(predictors, hyaBC)
# plot bioclim model
plot(hyaBCpredict)

# extract layer data for each point
mobPts <- extract(predictors, mobilense)
# create bioclim model
mobBC <- bioclim(mobPts)
# predict bioclim model
mobBCpredict <- predict(predictors, mobBC)
# plot bioclim model
plot(mobBCpredict)

# extract layer data for each point
parPts <- extract(predictors, parentals)
# create bioclim model
parBC <- bioclim(parPts)
# predict bioclim model
parBCpredict <- predict(predictors, parBC)
# plot bioclim model
plot(parBCpredict)

# extract layer data for each point
hybPts <- extract(predictors, hybrids)
# create bioclim model
hybBC <- bioclim(hybPts)
# predict bioclim model
hybBCpredict <- predict(predictors, hybBC)
# plot bioclim model
plot(hybBCpredict)

## Default maxent modeling
# run maxent for lavendulare (default parameters for dismo)
maxCan <- maxent(predictors, lavendulare)
maxCan # views results in browser window
response(maxCan) # show response curves for each layer
rCan <- predict(maxCan, predictors) # create model
plot(rCan) # plot predictive model
points(lavendulare) # add points to predictive model
writeRaster(rCan, "models/lavendulare.grd")

# run maxent for lavendulare (default parameters for dismo)
maxLav <- maxent(predictors, lavendulare)
maxLav # views results in browser window
response(maxLav) # show response curves for each layer
rLav <- predict(maxLav, predictors) # create model
plot(rLav) # plot predictive model
points(lavendulare) # add points to predictive model
writeRaster(rLav, "models/lavendulare.grd")

# run maxent for ecristatum (default parameters for dismo)
maxEcr <- maxent(predictors, ecristatum) 
maxEcr 
response(maxEcr) 
rEcr <- predict(maxEcr, predictors) 
plot(rEcr)
points(ecristatum)
writeRaster(rEcr, "models/ecristatum.grd")

# run maxent for fraseri (default parameters for dismo)
maxFra <- maxent(predictors, fraseri)
maxFra # views results in browser window
response(maxFra) # show response curves for each layer
rFra <- predict(maxFra, predictors) # create model
plot(rFra) # plot predictive model
points(fraseri) # add points to predictive model
writeRaster(rFra, "models/fraseri.grd")

# run maxent for hyacinthoides (default parameters for dismo)
maxHya <- maxent(predictors, hyacinthoides) 
maxHya 
response(maxHya) 
rHya <- predict(maxHya, predictors) 
plot(rHya)
points(hyacinthoides)
writeRaster(rHya, "models/hyacinthoides.grd")

# run maxent for mobilense (default parameters for dismo)
maxMob <- maxent(predictors, mobilense)
maxMob # views results in browser window
response(maxMob) # show response curves for each layer
rMob <- predict(maxMob, predictors) # create model
plot(rMob) # plot predictive model
points(mobilense) # add points to predictive model
writeRaster(rMob, "models/mobilense.grd")

# run maxent for parentals (default parameters for dismo)
maxPar <- maxent(predictors, parentals)
maxPar # views results in browser window
response(maxPar) # show response curves for each layer
rPar <- predict(maxPar, predictors) # create model
plot(rPar) # plot predictive model
points(parentals) # add points to predictive model
writeRaster(rPar, "models/parentals.grd")

# run maxent for hybrids (default parameters for dismo)
maxHyb <- maxent(predictors, hybrids) 
maxHyb 
response(maxHyb) 
rHyb <- predict(maxHyb, predictors) 
plot(rHyb)
points(hybrids)
writeRaster(rHyb, "models/hybrids.grd")

## Advanced modeling
# develop testing and training sets for lavendulare
fold <- kfold(lavendulare, k=5) #split occurence points into 5 sets
canTest <- lavendulare[fold == 1, ] #take 20% (1/5) for testing
canTrain <- lavendulare[fold != 1, ] #leave 40% for training
# fit training model for lavendulare
maxCanTrain <- maxent(predictors, canTrain) #fit maxent model
maxCanTrain #view results in html
rCanTrain <- predict(maxCanTrain, predictors) #predict full model
plot(rCanTrain) #visualize full model
points(lavendulare) #add points to plot
# testing model for lavendulare
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for lavendulare
maxCanTest <- evaluate(maxCanTrain, p=canTest, a=bg, x=predictors)
maxCanTest #print results
threshold(maxCanTest) #identify threshold for presence or absence
plot(maxCanTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for lavendulare
pvtest <- data.frame(extract(predictors, canTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxCanTest2 <- evaluate(maxCanTrain, p=pvtest, a=avtest)
maxCanTest2
# Alternative 2: predict to testing points for lavendulare
testp <- predict(maxCanTrain, pvtest)
testa <- predict(maxCanTrain, avtest)
maxCanTest3 <- evaluate(p=testp, a=testa)
maxCanTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxCanAdv <- maxent(
  x=predictors,
  p=lavendulare,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxCanAdv #view output as html

# develop testing and training sets for lavendulare
fold <- kfold(lavendulare, k=5) #split occurence points into 5 sets
lavTest <- lavendulare[fold == 1, ] #take 20% (1/5) for testing
lavTrain <- lavendulare[fold != 1, ] #leave 40% for training
# fit training model for lavendulare
maxLavTrain <- maxent(predictors, lavTrain) #fit maxent model
maxLavTrain #view results in html
rLavTrain <- predict(maxLavTrain, predictors) #predict full model
plot(rLavTrain) #visualize full model
points(lavendulare) #add points to plot
# testing model for lavendulare
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for lavendulare
maxLavTest <- evaluate(maxLavTrain, p=lavTest, a=bg, x=predictors)
maxLavTest #print results
threshold(maxLavTest) #identify threshold for presence or absence
plot(maxLavTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for lavendulare
pvtest <- data.frame(extract(predictors, lavTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxLavTest2 <- evaluate(maxLavTrain, p=pvtest, a=avtest)
maxLavTest2
# Alternative 2: predict to testing points for lavendulare
testp <- predict(maxLavTrain, pvtest)
testa <- predict(maxLavTrain, avtest)
maxLavTest3 <- evaluate(p=testp, a=testa)
maxLavTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxLavAdv <- maxent(
  x=predictors,
  p=lavendulare,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxLavAdv #view output as html

# develop testing and training sets for ecristatum
fold <- kfold(ecristatum, k=5) #split occurence points into 5 sets
ecrTest <- ecristatum[fold == 1, ] #take 20% (1/5) for testing
ecrTrain <- ecristatum[fold != 1, ] #leave 40% for training
# fit training model for ecristatum
maxEcrTrain <- maxent(predictors, ecrTrain) #fit maxent model
maxEcrTrain #view results in html
rEcrTrain <- predict(maxEcrTrain, predictors) #predict full model
plot(rEcrTrain) #visualize full model
points(ecristatum) #add points to plot
# testing model for ecristatum
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for ecristatum
maxEcrTest <- evaluate(maxEcrTrain, p=ecrTest, a=bg, x=predictors)
maxEcrTest #print results
threshold(maxEcrTest) #identify threshold for presence or absence
plot(maxEcrTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for ecristatum
pvtest <- data.frame(extract(predictors, ecrTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxEcrTest2 <- evaluate(maxEcrTrain, p=pvtest, a=avtest)
maxEcrTest2
# Alternative 2: predict to testing points for ecristatum
testp <- predict(maxEcrTrain, pvtest)
testa <- predict(maxEcrTrain, avtest)
maxEcrTest3 <- evaluate(p=testp, a=testa)
maxEcrTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxEcrAdv <- maxent(
  x=predictors,
  p=ecristatum,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxEcrAdv #view output as html

# develop testing and training sets for fraseri
fold <- kfold(fraseri, k=5) #split occurence points into 5 sets
fraTest <- fraseri[fold == 1, ] #take 20% (1/5) for testing
fraTrain <- fraseri[fold != 1, ] #leave 40% for training
# fit training model for fraseri
maxFraTrain <- maxent(predictors, fraTrain) #fit maxent model
maxFraTrain #view results in html
rFraTrain <- predict(maxFraTrain, predictors) #predict full model
plot(rFraTrain) #visualize full model
points(fraseri) #add points to plot
# testing model for fraseri
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for fraseri
maxFraTest <- evaluate(maxFraTrain, p=fraTest, a=bg, x=predictors)
maxFraTest #print results
threshold(maxFraTest) #identify threshold for presence or absence
plot(maxFraTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for fraseri
pvtest <- data.frame(extract(predictors, fraTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxFraTest2 <- evaluate(maxFraTrain, p=pvtest, a=avtest)
maxFraTest2
# Alternative 2: predict to testing points for fraseri
testp <- predict(maxFraTrain, pvtest)
testa <- predict(maxFraTrain, avtest)
maxFraTest3 <- evaluate(p=testp, a=testa)
maxFraTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxFraAdv <- maxent(
  x=predictors,
  p=fraseri,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxFraAdv #view output as html

# develop testing and training sets for hyacinthoides
fold <- kfold(hyacinthoides, k=5) #split occurence points into 5 sets
hyaTest <- hyacinthoides[fold == 1, ] #take 20% (1/5) for testing
hyaTrain <- hyacinthoides[fold != 1, ] #leave 40% for training
# fit training model for hyacinthoides
maxHyaTrain <- maxent(predictors, hyaTrain) #fit maxent model
maxHyaTrain #view results in html
rHyaTrain <- predict(maxHyaTrain, predictors) #predict full model
plot(rHyaTrain) #visualize full model
points(hyacinthoides) #add points to plot
# testing model for hyacinthoides
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for hyacinthoides
maxHyaTest <- evaluate(maxHyaTrain, p=hyaTest, a=bg, x=predictors)
maxHyaTest #print results
threshold(maxHyaTest) #identify threshold for presence or absence
plot(maxHyaTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hyacinthoides
pvtest <- data.frame(extract(predictors, hyaTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxHyaTest2 <- evaluate(maxHyaTrain, p=pvtest, a=avtest)
maxHyaTest2
# Alternative 2: predict to testing points for hyacinthoides
testp <- predict(maxHyaTrain, pvtest)
testa <- predict(maxHyaTrain, avtest)
maxHyaTest3 <- evaluate(p=testp, a=testa)
maxHyaTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHyaAdv <- maxent(
  x=predictors,
  p=hyacinthoides,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxHyaAdv #view output as html

# develop testing and training sets for parentals
fold <- kfold(parentals, k=5) #split occurence points into 5 sets
parTest <- parentals[fold == 1, ] #take 20% (1/5) for testing
parTrain <- parentals[fold != 1, ] #leave 40% for training
# fit training model for parentals
maxParTrain <- maxent(predictors, parTrain) #fit maxent model
maxParTrain #view results in html
rParTrain <- predict(maxParTrain, predictors) #predict full model
plot(rParTrain) #visualize full model
points(parentals) #add points to plot
# testing model for parentals
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for parentals
maxParTest <- evaluate(maxParTrain, p=parTest, a=bg, x=predictors)
maxParTest #print results
threshold(maxParTest) #identify threshold for presence or absence
plot(maxParTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for parentals
pvtest <- data.frame(extract(predictors, parTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxParTest2 <- evaluate(maxParTrain, p=pvtest, a=avtest)
maxParTest2
# Alternative 2: predict to testing points for parentals
testp <- predict(maxParTrain, pvtest)
testa <- predict(maxParTrain, avtest)
maxParTest3 <- evaluate(p=testp, a=testa)
maxParTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxParAdv <- maxent(
  x=predictors,
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
    'maximumiterations=1000' #default=500
  )
)
maxParAdv #view output as html

# develop testing and training sets for hybrids
fold <- kfold(hybrids, k=5) #split occurence points into 5 sets
hybTest <- hybrids[fold == 1, ] #take 20% (1/5) for testing
hybTrain <- hybrids[fold != 1, ] #leave 40% for training
# fit training model for hybrids
maxHybTrain <- maxent(predictors, hybTrain) #fit maxent model
maxHybTrain #view results in html
rHybTrain <- predict(maxHybTrain, predictors) #predict full model
plot(rHybTrain) #visualize full model
points(hybrids) #add points to plot
# testing model for hybrids
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for hybrids
maxHybTest <- evaluate(maxHybTrain, p=hybTest, a=bg, x=predictors)
maxHybTest #print results
threshold(maxHybTest) #identify threshold for presence or absence
plot(maxHybTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hybrids
pvtest <- data.frame(extract(predictors, hybTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxHybTest2 <- evaluate(maxHybTrain, p=pvtest, a=avtest)
maxHybTest2
# Alternative 2: predict to testing points for hybrids
testp <- predict(maxHybTrain, pvtest)
testa <- predict(maxHybTrain, avtest)
maxHybTest3 <- evaluate(p=testp, a=testa)
maxHybTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHybAdv <- maxent(
  x=predictors,
  p=hybrids,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxHybAdv #view output as html

### basic bioclim modeling with PRISM 1930 layers for varieties, parentals, and hybrids
# extract layer data for each point
canPts0 <- extract(predictors0, lavendulare)
# create bioclim model
canBC0 <- bioclim(canPts0)
# predict bioclim model
canBCpredict0 <- predict(predictors0, canBC0)
# plot bioclim model
plot(canBCpredict0)

# extract layer data for each point
lavPts0 <- extract(predictors0, lavendulare)
# create bioclim model
lavBC0 <- bioclim(lavPts0)
# predict bioclim model
lavBCpredict0 <- predict(predictors0, lavBC0)
# plot bioclim model
plot(lavBCpredict0)

# extract layer data for each point
ecrPts0 <- extract(predictors0, ecristatum)
# create bioclim model
ecrBC0 <- bioclim(ecrPts0)
# predict bioclim model
ecrBCpredict0 <- predict(predictors0, ecrBC0)
# plot bioclim model
plot(ecrBCpredict0)

# extract layer data for each point
fraPts0 <- extract(predictors0, fraseri)
# create bioclim model
fraBC0 <- bioclim(fraPts0)
# predict bioclim model
fraBCpredict0 <- predict(predictors0, fraBC0)
# plot bioclim model
plot(fraBCpredict0)

# extract layer data for each point
hyaPts0 <- extract(predictors0, hyacinthoides)
# create bioclim model
hyaBC0 <- bioclim(hyaPts0)
# predict bioclim model
hyaBCpredict0 <- predict(predictors0, hyaBC0)
# plot bioclim model
plot(hyaBCpredict0)

# extract layer data for each point
mobPts0 <- extract(predictors0, mobilense)
# create bioclim model
mobBC0 <- bioclim(mobPts0)
# predict bioclim model
mobBCpredict0 <- predict(predictors0, mobBC0)
# plot bioclim model
plot(mobBCpredict0)

# extract layer data for each point
parPts0 <- extract(predictors0, parentals)
# create bioclim model
parBC0 <- bioclim(parPts0)
# predict bioclim model
parBCpredict0 <- predict(predictors0, parBC0)
# plot bioclim model
plot(parBCpredict0)

# extract layer data for each point
hybPts0 <- extract(predictors0, hybrids)
# create bioclim model
hybBC0 <- bioclim(hybPts0)
# predict bioclim model
hybBCpredict0 <- predict(predictors0, hybBC0)
# plot bioclim model
plot(hybBCpredict0)

## Default maxent modeling
# run maxent for lavendulare (default parameters for dismo)
maxCan0 <- maxent(predictors0, lavendulare)
maxCan0 # views results in browser window
response(maxCan0) # show response curves for each layer
rCan0 <- predict(maxCan0, predictors0) # create model
plot(rCan0) # plot predictive model
points(lavendulare) # add points to predictive model
writeRaster(rCan0, "models/lavendulare1930.grd")

# run maxent for lavendulare (default parameters for dismo)
maxLav0 <- maxent(predictors0, lavendulare)
maxLav0 # views results in browser window
response(maxLav0) # show response curves for each layer
rLav0 <- predict(maxLav0, predictors0) # create model
plot(rLav0) # plot predictive model
points(lavendulare) # add points to predictive model
writeRaster(rLav0, "models/lavendulare1930.grd")

# run maxent for ecristatum (default parameters for dismo)
maxEcr0 <- maxent(predictors0, ecristatum) 
maxEcr0 # views results in browser window
response(maxEcr0) 
rEcr0 <- predict(maxEcr0, predictors0) 
plot(rEcr0)
points(ecristatum)
writeRaster(rEcr0, "models/ecristatum1930.grd")

# run maxent for fraseri (default parameters for dismo)
maxFra0 <- maxent(predictors0, fraseri)
maxFra0 # views results in browser window
response(maxFra0) # show response curves for each layer
rFra0 <- predict(maxFra0, predictors0) # create model
plot(rFra0) # plot predictive model
points(fraseri) # add points to predictive model
writeRaster(rFra0, "models/fraseri1930.grd")

# run maxent for hyacinthoides (default parameters for dismo)
maxHya0 <- maxent(predictors0, hyacinthoides) 
maxHya0 
response(maxHya0) 
rHya0 <- predict(maxHya0, predictors0) 
plot(rHya0)
points(hyacinthoides)
writeRaster(rHya0, "models/hyacinthoides1930.grd")

# run maxent for mobilense (default parameters for dismo)
maxMob0 <- maxent(predictors0, mobilense)
maxMob0 # views results in browser window
response(maxMob0) # show response curves for each layer
rMob0 <- predict(maxMob0, predictors0) # create model
plot(rMob0) # plot predictive model
points(mobilense) # add points to predictive model
writeRaster(rMob0, "models/mobilense1930.grd")

# run maxent for parentals (default parameters for dismo)
maxPar0 <- maxent(predictors0, parentals)
maxPar0 # views results in browser window
response(maxPar0) # show response curves for each layer
rPar0 <- predict(maxPar0, predictors0) # create model
plot(rPar0) # plot predictive model
points(parentals) # add points to predictive model
writeRaster(rPar0, "models/parentals1930.grd")

# run maxent for hybrids (default parameters for dismo)
maxHyb0 <- maxent(predictors0, hybrids) 
maxHyb0 
response(maxHyb0) 
rHyb0 <- predict(maxHyb0, predictors0) 
plot(rHyb0)
points(hybrids)
writeRaster(rHyb0, "models/hybrids1930.grd")

## Advanced modeling
# develop testing and training sets for lavendulare
fold <- kfold(lavendulare, k=5) #split occurence points into 5 sets
lavTest0 <- lavendulare[fold == 1, ] #take 20% (1/5) for testing
lavTrain0 <- lavendulare[fold != 1, ] #leave 40% for training
# fit training model for lavendulare
maxlavTrain0 <- maxent(predictors0, lavTrain0) #fit maxent model
maxlavTrain0 #view results in html
rlavTrain0 <- predict(maxCan0Train, predictors0) #predict full model
plot(rlavTrain0) #visualize full model
points(lavendulare) #add points to plot
# testing model for lavendulare
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for lavendulare
maxlavTest0 <- evaluate(maxlavTrain0, p=lavTest0, a=bg, x=predictors0)
maxlavTest0 #print results
threshold(maxlavTest0) #identify threshold for presence or absence
plot(maxlavTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for lavendulare
pvtest0 <- data.frame(extract(predictors0, lavTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxLavTest20 <- evaluate(maxCanTrain, p=pvtest0, a=avtest0)
maxLavTest20
# Alternative 2: predict to testing points for lavendulare
testp0 <- predict(maxlavTrain0, pvtest0)
testa0 <- predict(maxlavTrain0, avtest0)
maxLavTest30 <- evaluate(p=testp0, a=testa0)
maxLavTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxCanAdv0 <- maxent(
  x=predictors0,
  p=lavendulare,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxCanAdv0 #view output as html

# develop testing and training sets for lavendulare
fold <- kfold(lavendulare, k=5) #split occurence points into 5 sets
lavTest0 <- lavendulare[fold == 1, ] #take 20% (1/5) for testing
lavTrain0 <- lavendulare[fold != 1, ] #leave 40% for training
# fit training model for lavendulare
maxLavTrain0 <- maxent(predictors0, lavTrain0) #fit maxent model
maxLavTrain0 #view results in html
rLavTrain0 <- predict(maxLavTrain0, predictors0) #predict full model
plot(rLavTrain0) #visualize full model
points(lavendulare) #add points to plot
# testing model for lavendulare
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for lavendulare
maxLavTest0 <- evaluate(maxLavTrain0, p=lavTest0, a=bg, x=predictors0)
maxLavTest0 #print results
threshold(maxLavTest0) #identify threshold for presence or absence
plot(maxLavTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for lavendulare
pvtest0 <- data.frame(extract(predictors0, lavTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxLavTest20 <- evaluate(maxLavTrain0, p=pvtest0, a=avtest0)
maxLavTest20
# Alternative 2: predict to testing points for lavendulare
testp0 <- predict(maxLavTrain0, pvtest0)
testa0 <- predict(maxLavTrain0, avtest0)
maxLavTest30 <- evaluate(p=testp0, a=testa0)
maxLavTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxLavAdv0 <- maxent(
  x=predictors0,
  p=lavendulare,
  removeDuplicates=TRUE,
  nbg=10000,
  args=c(
    'randomseed=true', #default=false
    'threads=2', #default=1
    'responsecurves=true', #default=false
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxLavAdv0 #view output as html