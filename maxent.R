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

lavendulare <- (alliumcanadense5)

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

#merging occurrence data for all 6 varieties
combined1<- merge(lavendulare, ecristatum, by="Collector", all=TRUE)
combined2<- merge(combined1, fraseri, by="Collector", all=TRUE)
combined3<- merge(combined2, hyacinthoides, by="Collector", all=TRUE)
combined4<- merge(combined3, mobilense, by="Collector", all=TRUE)
combined<- merge(combined4, canadense, by="Collector", all=TRUE)

##prepare varieties,parentals,and hybrids for modeling
canadense <- alliumcanadense1[,c(3,2)]
lavendulare <- alliumcanadense5[,c(3,2)]
ecristatum<- ecristatum[,c(3,2)]
fraseri<- fraseri[,c(3,2)]
hyacinthoides<- hyacinthoides[,c(3,2)]
mobilense<- mobilense[,c(3,2)]
parentals<- parentals[,c(3,2)]
hybrids<- hybrids[,c(3,2)]
combined<- combined[,c(3,2)]

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
canPts <- extract(predictors, canadense)
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

# extract layer data for each point
comPts <- extract(predictors, combined)
# create bioclim model
comBC <- bioclim(comPts)
# predict bioclim model
comBCpredict <- predict(predictors, comBC)
# plot bioclim model
plot(comBCpredict)

## Default maxent modeling
# run maxent for canadense (default parameters for dismo)
maxCan <- maxent(predictors, canadense)
maxCan # views results in browser window
response(maxCan) # show response curves for each layer
rCan <- predict(maxCan, predictors) # create model
plot(rCan) # plot predictive model
points(canadense) # add points to predictive model
writeRaster(rCan, "models/canadense.grd")

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

# run maxent for combined (default parameters for dismo)
maxCom <- maxent(predictors, combined)
maxCom # views results in browser window
response(maxCom) # show response curves for each layer
rCom <- predict(maxCom, predictors) # create model
plot(rCom) # plot predictive model
points(combined) # add points to predictive model
writeRaster(rCom, "models/combined.grd")

## Advanced modeling
# develop testing and training sets for canadense
fold <- kfold(canadense, k=5) #split occurence points into 5 sets
canTest <- canadense[fold == 1, ] #take 20% (1/5) for testing
canTrain <- canadense[fold != 1, ] #leave 40% for training
# fit training model for canadense
maxCanTrain <- maxent(predictors, canTrain) #fit maxent model
maxCanTrain #view results in html
rCanTrain <- predict(maxCanTrain, predictors) #predict full model
plot(rCanTrain) #visualize full model
points(canadense) #add points to plot
# testing model for canadense
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for canadense
maxCanTest <- evaluate(maxCanTrain, p=canTest, a=bg, x=predictors)
maxCanTest #print results
threshold(maxCanTest) #identify threshold for presence or absence
plot(maxCanTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for canadense
pvtest <- data.frame(extract(predictors, canTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxCanTest2 <- evaluate(maxCanTrain, p=pvtest, a=avtest)
maxCanTest2
# Alternative 2: predict to testing points for canadense
testp <- predict(maxCanTrain, pvtest)
testa <- predict(maxCanTrain, avtest)
maxCanTest3 <- evaluate(p=testp, a=testa)
maxCanTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxCanAdv <- maxent(
  x=predictors,
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

# develop testing and training sets for mobilense
fold <- kfold(mobilense, k=5) #split occurence points into 5 sets
mobTest <- mobilense[fold == 1, ] #take 20% (1/5) for testing
mobTrain <- mobilense[fold != 1, ] #leave 40% for training
# fit training model for mobilense
maxMobTrain <- maxent(predictors, mobTrain) #fit maxent model
maxMobTrain #view results in html
rMobTrain <- predict(maxMobTrain, predictors) #predict full model
plot(rMobTrain) #visualize full model
points(mobilense) #add points to plot
# testing model for mobilense
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for mobilense
maxMobTest <- evaluate(maxMobTrain, p=mobTest, a=bg, x=predictors)
maxMobTest #print results
threshold(maxMobTest) #identify threshold for presence or absence
plot(maxMobTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for mobilense
pvtest <- data.frame(extract(predictors, mobTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxMobTest2 <- evaluate(maxMobTrain, p=pvtest, a=avtest)
maxMobTest2
# Alternative 2: predict to testing points for mobilense
testp <- predict(maxMobTrain, pvtest)
testa <- predict(maxMobTrain, avtest)
maxMobTest3 <- evaluate(p=testp, a=testa)
maxMobTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxMobAdv <- maxent(
  x=predictors,
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
    'maximumiterations=1000' #default=500
  )
)
maxMobAdv #view output as html

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

# develop testing and training sets for combined
fold <- kfold(combined, k=5) #split occurence points into 5 sets
comTest <- combined[fold == 1, ] #take 20% (1/5) for testing
comTrain <- combined[fold != 1, ] #leave 40% for training
# fit training model for combined
maxComTrain <- maxent(predictors, comTrain) #fit maxent model
maxComTrain #view results in html
rComTrain <- predict(maxComTrain, predictors) #predict full model
plot(rComTrain) #visualize full model
points(combined) #add points to plot
# testing model for combined
# extract background points
bg <- randomPoints(predictors, 1000)
# cross-validate model for combined
maxComTest <- evaluate(maxComTrain, p=comTest, a=bg, x=predictors)
maxComTest #print results
threshold(maxComTest) #identify threshold for presence or absence
plot(maxComTest, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for combined
pvtest <- data.frame(extract(predictors, comTest))
avtest <- data.frame(extract(predictors, bg))
# cross-validate model
maxComTest2 <- evaluate(maxComTrain, p=pvtest, a=avtest)
maxComTest2
# Alternative 2: predict to testing points for combined
testp <- predict(maxComTrain, pvtest)
testa <- predict(maxComTrain, avtest)
maxComTest3 <- evaluate(p=testp, a=testa)
maxComTest3
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxComAdv <- maxent(
  x=predictors,
  p=combined,
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
maxComAdv #view output as html

### basic bioclim modeling with PRISM 1930 layers for varieties, parentals, and hybrids
# extract layer data for each point
canPts0 <- extract(predictors0, canadense)
# create bioclim model
canBC0 <- bioclim(canPts0)
# predict bioclim model
canBCpredict0 <- predict(predictors0, canBC0)
# plot bioclim model
plot(canBCpredict0)

# extract layer data for each point
lavPts0 <- extract(predictors0, canadense)
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

# extract layer data for each point
comPts0 <- extract(predictors0, combined)
# create bioclim model
comBC0 <- bioclim(comPts0)
# predict bioclim model
comBCpredict0 <- predict(predictors0, comBC0)
# plot bioclim model
plot(comBCpredict0)

## Default maxent modeling
# run maxent for canadense (default parameters for dismo)
maxCan0 <- maxent(predictors0, canadense)
maxCan0 # views results in browser window
response(maxCan0) # show response curves for each layer
rCan0 <- predict(maxCan0, predictors0) # create model
plot(rCan0) # plot predictive model
points(canadense) # add points to predictive model
writeRaster(rCan0, "models/canadense1930.grd")

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

# run maxent for combined (default parameters for dismo)
maxCom0 <- maxent(predictors0, combined)
maxCom0 # views results in browser window
response(maxCom0) # show response curves for each layer
rCom0 <- predict(maxCom0, predictors0) # create model
plot(rCom0) # plot predictive model
points(combined) # add points to predictive model
writeRaster(rCom0, "models/combined1930.grd")

## Advanced modeling
# develop testing and training sets for canadense
fold <- kfold(canadense, k=5) #split occurence points into 5 sets
canTest0 <- canadense[fold == 1, ] #take 20% (1/5) for testing
canTrain0 <- canadense[fold != 1, ] #leave 40% for training
# fit training model for canadense
maxCanTrain0 <- maxent(predictors0, canTrain0) #fit maxent model
maxCanTrain0 #view results in html
rCanTrain0 <- predict(maxCanTrain0, predictors0) #predict full model
plot(rCanTrain0) #visualize full model
points(canadense) #add points to plot
# testing model for canadense
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for canadense
maxCanTest0 <- evaluate(maxlavTrain0, p=lavTest0, a=bg0, x=predictors0)
maxCanTest0 #print results
threshold(maxCanTest0) #identify threshold for presence or absence
plot(maxlavTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for canadense
pvtest0 <- data.frame(extract(predictors0, lavTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxCanTest20 <- evaluate(maxCanTrain, p=pvtest0, a=avtest0)
maxCanTest20
# Alternative 2: predict to testing points for canadense
testp0 <- predict(maxCanTrain0, pvtest0)
testa0 <- predict(maxCanTrain0, avtest0)
maxCanTest30 <- evaluate(p=testp0, a=testa0)
maxCanTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxCanAdv0 <- maxent(
  x=predictors0,
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
maxLavTest0 <- evaluate(maxLavTrain0, p=lavTest0, a=bg0, x=predictors0)
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

# develop testing and training sets for ecristatum
fold <- kfold(ecristatum, k=5) #split occurence points into 5 sets
ecrTest0 <- ecristatum[fold == 1, ] #take 20% (1/5) for testing
ecrTrain0 <- ecristatum[fold != 1, ] #leave 40% for training
# fit training model for ecristatum
maxEcrTrain0 <- maxent(predictors0, ecrTrain0) #fit maxent model
maxEcrTrain0 #view results in html
rEcrTrain0 <- predict(maxEcrTrain0, predictors0) #predict full model
plot(rEcrTrain0) #visualize full model
points(ecristatum) #add points to plot
# testing model for ecristatum
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for ecristatum
maxEcrTest0 <- evaluate(maxEcrTrain0, p=ecrTest0, a=bg0, x=predictors0)
maxEcrTest0 #print results
threshold(maxEcrTest0) #identify threshold for presence or absence
plot(maxEcrTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for ecristatum
pvtest0 <- data.frame(extract(predictors0, ecrTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxEcrTest20 <- evaluate(maxEcrTrain0, p=pvtest0, a=avtest0)
maxEcrTest20
# Alternative 2: predict to testing points for ecristatum
testp0 <- predict(maxEcrTrain0, pvtest0)
testa0 <- predict(maxEcrTrain0, avtest0)
maxEcrTest30 <- evaluate(p=testp0, a=testa0)
maxEcrTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxEcrAdv0 <- maxent(
  x=predictors0,
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
maxEcrAdv0 #view output as html

# develop testing and training sets for fraseri
fold <- kfold(fraseri, k=5) #split occurence points into 5 sets
fraTest0 <- fraseri[fold == 1, ] #take 20% (1/5) for testing
fraTrain0 <- fraseri[fold != 1, ] #leave 40% for training
# fit training model for fraseri
maxFraTrain0 <- maxent(predictors0, fraTrain0) #fit maxent model
maxFraTrain0 #view results in html
rFraTrain0 <- predict(maxFraTrain0, predictors0) #predict full model
plot(rFraTrain0) #visualize full model
points(fraseri) #add points to plot
# testing model for fraseri
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for fraseri
maxFraTest0 <- evaluate(maxFraTrain0, p=fraTest0, a=bg0, x=predictors0)
maxFraTest0 #print results
threshold(maxFraTest0) #identify threshold for presence or absence
plot(maxFraTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for fraseri
pvtest0 <- data.frame(extract(predictors0, fraTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxFraTest20 <- evaluate(maxFraTrain0, p=pvtest0, a=avtest0)
maxFraTest20
# Alternative 2: predict to testing points for fraseri
testp0 <- predict(maxFraTrain0, pvtest0)
testa0 <- predict(maxFraTrain0, avtest0)
maxFraTest30 <- evaluate(p=testp0, a=testa0)
maxFraTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxFraAdv0 <- maxent(
  x=predictors0,
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
maxFraAdv0 #view output as html

# develop testing and training sets for hyacinthoides
fold <- kfold(hyacinthoides, k=5) #split occurence points into 5 sets
hyaTest0 <- hyacinthoides[fold == 1, ] #take 20% (1/5) for testing
hyaTrain0 <- hyacinthoides[fold != 1, ] #leave 40% for training
# fit training model for hyacinthoides
maxHyaTrain0 <- maxent(predictors0, hyaTrain0) #fit maxent model
maxHyaTrain0 #view results in html
rHyaTrain0 <- predict(maxHyaTrain0, predictors0) #predict full model
plot(rHyaTrain0) #visualize full model
points(hyacinthoides) #add points to plot
# testing model for hyacinthoides
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for hyacinthoides
maxHyaTest0 <- evaluate(maxHyaTrain0, p=hyaTest0, a=bg0, x=predictors0)
maxHyaTest0 #print results
threshold(maxHyaTest0) #identify threshold for presence or absence
plot(maxHyaTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hyacinthoides
pvtest0 <- data.frame(extract(predictors0, hyaTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxHyaTest20 <- evaluate(maxHyaTrain0, p=pvtest0, a=avtest0)
maxHyaTest20
# Alternative 2: predict to testing points for hyacinthoides
testp0 <- predict(maxHyaTrain0, pvtest0)
testa0 <- predict(maxHyaTrain0, avtest0)
maxHyaTest30 <- evaluate(p=testp0, a=testa0)
maxHyaTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHyaAdv0 <- maxent(
  x=predictors0,
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
maxHyaAdv0 #view output as html

# develop testing and training sets for mobilense
fold <- kfold(mobilense, k=5) #split occurence points into 5 sets
mobTest0 <- mobilense[fold == 1, ] #take 20% (1/5) for testing
mobTrain0 <- mobilense[fold != 1, ] #leave 40% for training
# fit training model for mobilense
maxMobTrain0 <- maxent(predictors0, mobTrain0) #fit maxent model
maxMobTrain0 #view results in html
rMobTrain0 <- predict(maxMobTrain0, predictors0) #predict full model
plot(rMobTrain0) #visualize full model
points(mobilense) #add points to plot
# testing model for mobilense
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for mobilense
maxMobTest0 <- evaluate(maxMobTrain0, p=mobTest0, a=bg0, x=predictors0)
maxMobTest0 #print results
threshold(maxMobTest0) #identify threshold for presence or absence
plot(maxMobTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for mobilense
pvtest0 <- data.frame(extract(predictors0, mobTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxMobTest20 <- evaluate(maxMobTrain0, p=pvtest0, a=avtest0)
maxMobTest20
# Alternative 2: predict to testing points for mobilense
testp0 <- predict(maxMobTrain0, pvtest0)
testa0 <- predict(maxMobTrain0, avtest0)
maxMobTest30 <- evaluate(p=testp0, a=testa0)
maxMobTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxMobAdv0 <- maxent(
  x=predictors0,
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
    'maximumiterations=1000' #default=500
  )
)
maxMobAdv0 #view output as html

# develop testing and training sets for parentals
fold <- kfold(parentals, k=5) #split occurence points into 5 sets
parTest0 <- parentals[fold == 1, ] #take 20% (1/5) for testing
parTrain0 <- parentals[fold != 1, ] #leave 40% for training
# fit training model for parentals
maxParTrain0 <- maxent(predictors0, parTrain0) #fit maxent model
maxParTrain0 #view results in html
rParTrain0 <- predict(maxParTrain0, predictors0) #predict full model
plot(rParTrain0) #visualize full model
points(parentals) #add points to plot
# testing model for parentals
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for parentals
maxParTest0 <- evaluate(maxParTrain0, p=parTest0, a=bg0, x=predictors0)
maxParTest0 #print results
threshold(maxParTest0) #identify threshold for presence or absence
plot(maxParTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for parentals
pvtest0 <- data.frame(extract(predictors0, parTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxParTest20 <- evaluate(maxParTrain0, p=pvtest0, a=avtest0)
maxParTest20
# Alternative 2: predict to testing points for parentals
testp0 <- predict(maxParTrain0, pvtest0)
testa0 <- predict(maxParTrain0, avtest0)
maxParTest30 <- evaluate(p=testp0, a=testa0)
maxParTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxParAdv0 <- maxent(
  x=predictors0,
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
maxParAdv0 #view output as html

# develop testing and training sets for hybrids
fold <- kfold(hybrids, k=5) #split occurence points into 5 sets
hybTest0 <- hybrids[fold == 1, ] #take 20% (1/5) for testing
hybTrain0 <- hybrids[fold != 1, ] #leave 40% for training
# fit training model for hybrids
maxHybTrain0 <- maxent(predictors0, hybTrain0) #fit maxent model
maxHybTrain0 #view results in html
rHybTrain0 <- predict(maxHybTrain0, predictors0) #predict full model
plot(rHybTrain0) #visualize full model
points(hybrids) #add points to plot
# testing model for hybrids
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for hybrids
maxHybTest0 <- evaluate(maxHybTrain0, p=hybTest0, a=bg0, x=predictors0)
maxHybTest0 #print results
threshold(maxHybTest0) #identify threshold for presence or absence
plot(maxHybTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hybrids
pvtest0 <- data.frame(extract(predictors0, hybTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxHybTest20 <- evaluate(maxHybTrain0, p=pvtest0, a=avtest0)
maxHybTest20
# Alternative 2: predict to testing points for hybrids
testp0 <- predict(maxHybTrain0, pvtest0)
testa0 <- predict(maxHybTrain0, avtest0)
maxHybTest30 <- evaluate(p=testp0, a=testa0)
maxHybTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHybAdv0 <- maxent(
  x=predictors0,
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
maxHybAdv0 #view output as html

# develop testing and training sets for combined
fold <- kfold(combined, k=5) #split occurence points into 5 sets
comTest0 <- combined[fold == 1, ] #take 20% (1/5) for testing
comTrain0 <- combined[fold != 1, ] #leave 40% for training
# fit training model for combined
maxComTrain0 <- maxent(predictors0, comTrain0) #fit maxent model
maxComTrain0 #view results in html
rComTrain0 <- predict(maxComTrain0, predictors0) #predict full model
plot(rComTrain0) #visualize full model
points(combined) #add points to plot
# testing model for combined
# extract background points
bg0 <- randomPoints(predictors0, 1000)
# cross-validate model for combined
maxComTest0 <- evaluate(maxComTrain0, p=comTest0, a=bg0, x=predictors0)
maxComTest0 #print results
threshold(maxComTest0) #identify threshold for presence or absence
plot(maxComTest0, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for combined
pvtest0 <- data.frame(extract(predictors0, comTest0))
avtest0 <- data.frame(extract(predictors0, bg0))
# cross-validate model
maxComTest20 <- evaluate(maxComTrain0, p=pvtest0, a=avtest0)
maxComTest20
# Alternative 2: predict to testing points for combined
testp0 <- predict(maxComTrain0, pvtest0)
testa0 <- predict(maxComTrain0, avtest0)
maxComTest30 <- evaluate(p=testp0, a=testa0)
maxComTest30
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxComAdv0 <- maxent(
  x=predictors0,
  p=combined,
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
maxComAdv0 #view output as html

### basic bioclim modeling with PRISM 2014 layers for varieties, parentals, and hybrids
# extract layer data for each point
canPts1 <- extract(predictors1, canadense)
# create bioclim model
canBC1 <- bioclim(canPts1)
# predict bioclim model
canBCpredict1 <- predict(predictors1, canBC1)
# plot bioclim model
plot(canBCpredict1)

# extract layer data for each point
lavPts1 <- extract(predictors1, canadense)
# create bioclim model
lavBC1 <- bioclim(lavPts1)
# predict bioclim model
lavBCpredict1 <- predict(predictors1, lavBC1)
# plot bioclim model
plot(lavBCpredict1)

# extract layer data for each point
ecrPts1 <- extract(predictors1, ecristatum)
# create bioclim model
ecrBC1 <- bioclim(ecrPts1)
# predict bioclim model
ecrBCpredict1 <- predict(predictors1, ecrBC1)
# plot bioclim model
plot(ecrBCpredict1)

# extract layer data for each point
fraPts1 <- extract(predictors1, fraseri)
# create bioclim model
fraBC1 <- bioclim(fraPts1)
# predict bioclim model
fraBCpredict1 <- predict(predictors1, fraBC1)
# plot bioclim model
plot(fraBCpredict1)

# extract layer data for each point
hyaPts1 <- extract(predictors1, hyacinthoides)
# create bioclim model
hyaBC1 <- bioclim(hyaPts1)
# predict bioclim model
hyaBCpredict1 <- predict(predictors1, hyaBC1)
# plot bioclim model
plot(hyaBCpredict1)

# extract layer data for each point
mobPts1 <- extract(predictors1, mobilense)
# create bioclim model
mobBC1 <- bioclim(mobPts1)
# predict bioclim model
mobBCpredict1 <- predict(predictors1, mobBC1)
# plot bioclim model
plot(mobBCpredict1)

# extract layer data for each point
parPts1 <- extract(predictors1, parentals)
# create bioclim model
parBC1 <- bioclim(parPts1)
# predict bioclim model
parBCpredict1 <- predict(predictors1, parBC1)
# plot bioclim model
plot(parBCpredict1)

# extract layer data for each point
hybPts1 <- extract(predictors1, hybrids)
# create bioclim model
hybBC1 <- bioclim(hybPts1)
# predict bioclim model
hybBCpredict1 <- predict(predictors1, hybBC1)
# plot bioclim model
plot(hybBCpredict1)

# extract layer data for each point
comPts1 <- extract(predictors1, combined)
# create bioclim model
comBC1 <- bioclim(comPts1)
# predict bioclim model
comBCpredict1 <- predict(predictors1, comBC1)
# plot bioclim model
plot(comBCpredict1)

## Default maxent modeling
# run maxent for canadense (default parameters for dismo)
maxCan1 <- maxent(predictors1, canadense)
maxCan1 # views results in browser window
response(maxCan1) # show response curves for each layer
rCan1 <- predict(maxCan1, predictors1) # create model
plot(rCan1) # plot predictive model
points(canadense) # add points to predictive model
writeRaster(rCan1, "models/canadense2014.grd")

# run maxent for lavendulare (default parameters for dismo)
maxLav1 <- maxent(predictors1, lavendulare)
maxLav1 # views results in browser window
response(maxLav1) # show response curves for each layer
rLav1 <- predict(maxLav1, predictors1) # create model
plot(rLav1) # plot predictive model
points(lavendulare) # add points to predictive model
writeRaster(rLav1, "models/lavendulare2014.grd")

# run maxent for ecristatum (default parameters for dismo)
maxEcr1 <- maxent(predictors1, ecristatum) 
maxEcr1 # views results in browser window
response(maxEcr1) 
rEcr1 <- predict(maxEcr1, predictors1) 
plot(rEcr1)
points(ecristatum)
writeRaster(rEcr1, "models/ecristatum2014.grd")

# run maxent for fraseri (default parameters for dismo)
maxFra1 <- maxent(predictors1, fraseri)
maxFra1 # views results in browser window
response(maxFra1) # show response curves for each layer
rFra1 <- predict(maxFra1, predictors1) # create model
plot(rFra1) # plot predictive model
points(fraseri) # add points to predictive model
writeRaster(rFra1, "models/fraseri2014.grd")

# run maxent for hyacinthoides (default parameters for dismo)
maxHya1 <- maxent(predictors1, hyacinthoides) 
maxHya1 
response(maxHya1) 
rHya1 <- predict(maxHya1, predictors1) 
plot(rHya1)
points(hyacinthoides)
writeRaster(rHya1, "models/hyacinthoides2014.grd")

# run maxent for mobilense (default parameters for dismo)
maxMob1 <- maxent(predictors1, mobilense)
maxMob1 # views results in browser window
response(maxMob1) # show response curves for each layer
rMob1 <- predict(maxMob1, predictors1) # create model
plot(rMob1) # plot predictive model
points(mobilense) # add points to predictive model
writeRaster(rMob1, "models/mobilense2014.grd")

# run maxent for parentals (default parameters for dismo)
maxPar1 <- maxent(predictors1, parentals)
maxPar1 # views results in browser window
response(maxPar1) # show response curves for each layer
rPar1 <- predict(maxPar1, predictors1) # create model
plot(rPar1) # plot predictive model
points(parentals) # add points to predictive model
writeRaster(rPar1, "models/parentals2014.grd")

# run maxent for hybrids (default parameters for dismo)
maxHyb1 <- maxent(predictors1, hybrids) 
maxHyb1 
response(maxHyb1) 
rHyb1 <- predict(maxHyb1, predictors1) 
plot(rHyb1)
points(hybrids)
writeRaster(rHyb1, "models/hybrids2014.grd")

# run maxent for combined (default parameters for dismo)
maxCom1 <- maxent(predictors1, combined)
maxCom1 # views results in browser window
response(maxCom1) # show response curves for each layer
rCom1 <- predict(maxCom1, predictors1) # create model
plot(rCom1) # plot predictive model
points(combined) # add points to predictive model
writeRaster(rCom1, "models/combined2014.grd")

## Advanced modeling
# develop testing and training sets for canadense
fold <- kfold(canadense, k=5) #split occurence points into 5 sets
canTest1 <- canadense[fold == 1, ] #take 20% (1/5) for testing
canTrain1 <- canadense[fold != 1, ] #leave 40% for training
# fit training model for canadense
maxCanTrain1 <- maxent(predictors1, canTrain1) #fit maxent model
maxCanTrain1 #view results in html
rCanTrain1 <- predict(maxCan1Train, predictors1) #predict full model
plot(rCanTrain1) #visualize full model
points(canadense) #add points to plot
# testing model for canadense
# extract background points
bg1 <- randomPoints(predictors1, 1000)
# cross-validate model for canadense
maxCanTest1 <- evaluate(maxlavTrain1, p=lavTest1, a=bg1, x=predictors1)
maxCanTest1 #print results
threshold(maxCanTest1) #identify threshold for presence or absence
plot(maxlavTest1, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for canadense
pvtest1 <- data.frame(extract(predictors1, lavTest1))
avtest1 <- data.frame(extract(predictors1, bg1))
# cross-validate model
maxCanTest21 <- evaluate(maxCanTrain, p=pvtest1, a=avtest1)
maxCanTest21
# Alternative 2: predict to testing points for canadense
testp1 <- predict(maxCanTrain1, pvtest1)
testa1 <- predict(maxCanTrain1, avtest1)
maxCanTest31 <- evaluate(p=testp1, a=testa1)
maxCanTest31
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxCanAdv1 <- maxent(
  x=predictors1,
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
    'maximumiterations=1000' #default=500
  )
)
maxCanAdv1 #view output as html

# develop testing and training sets for lavendulare
fold <- kfold(lavendulare, k=5) #split occurence points into 5 sets
lavTest1 <- lavendulare[fold == 1, ] #take 20% (1/5) for testing
lavTrain1 <- lavendulare[fold != 1, ] #leave 40% for training
# fit training model for lavendulare
maxLavTrain1 <- maxent(predictors1, lavTrain1) #fit maxent model
maxLavTrain1 #view results in html
rLavTrain1 <- predict(maxLavTrain1, predictors1) #predict full model
plot(rLavTrain1) #visualize full model
points(lavendulare) #add points to plot
# testing model for lavendulare
# extract background points
bg1 <- randomPoints(predictors1, 1000)
# cross-validate model for lavendulare
maxLavTest1 <- evaluate(maxLavTrain1, p=lavTest1, a=bg1, x=predictors1)
maxLavTest1 #print results
threshold(maxLavTest1) #identify threshold for presence or absence
plot(maxLavTest1, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for lavendulare
pvtest1 <- data.frame(extract(predictors1, lavTest1))
avtest1 <- data.frame(extract(predictors1, bg1))
# cross-validate model
maxLavTest21 <- evaluate(maxLavTrain1, p=pvtest1, a=avtest1)
maxLavTest21
# Alternative 2: predict to testing points for lavendulare
testp1 <- predict(maxLavTrain1, pvtest1)
testa1 <- predict(maxLavTrain1, avtest1)
maxLavTest31 <- evaluate(p=testp1, a=testa1)
maxLavTest31
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxLavAdv1 <- maxent(
  x=predictors1,
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
maxLavAdv1 #view output as html

# develop testing and training sets for ecristatum
fold <- kfold(ecristatum, k=5) #split occurence points into 5 sets
ecrTest1 <- ecristatum[fold == 1, ] #take 20% (1/5) for testing
ecrTrain1 <- ecristatum[fold != 1, ] #leave 40% for training
# fit training model for ecristatum
maxEcrTrain1 <- maxent(predictors1, ecrTrain1) #fit maxent model
maxEcrTrain1 #view results in html
rEcrTrain1 <- predict(maxEcrTrain1, predictors1) #predict full model
plot(rEcrTrain1) #visualize full model
points(ecristatum) #add points to plot
# testing model for ecristatum
# extract background points
bg1 <- randomPoints(predictors1, 1000)
# cross-validate model for ecristatum
maxEcrTest1 <- evaluate(maxEcrTrain1, p=ecrTest1, a=bg1, x=predictors1)
maxEcrTest1 #print results
threshold(maxEcrTest1) #identify threshold for presence or absence
plot(maxEcrTest1, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for ecristatum
pvtest1 <- data.frame(extract(predictors1, ecrTest1))
avtest1 <- data.frame(extract(predictors1, bg1))
# cross-validate model
maxEcrTest21 <- evaluate(maxEcrTrain1, p=pvtest1, a=avtest1)
maxEcrTest21
# Alternative 2: predict to testing points for ecristatum
testp1 <- predict(maxEcrTrain1, pvtest1)
testa1 <- predict(maxEcrTrain1, avtest1)
maxEcrTest31 <- evaluate(p=testp1, a=testa1)
maxEcrTest31
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxEcrAdv1 <- maxent(
  x=predictors1,
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
maxEcrAdv1 #view output as html

# develop testing and training sets for fraseri
fold <- kfold(fraseri, k=5) #split occurence points into 5 sets
fraTest1 <- fraseri[fold == 1, ] #take 20% (1/5) for testing
fraTrain1 <- fraseri[fold != 1, ] #leave 40% for training
# fit training model for fraseri
maxFraTrain1 <- maxent(predictors1, fraTrain1) #fit maxent model
maxFraTrain1 #view results in html
rFraTrain1 <- predict(maxFraTrain1, predictors1) #predict full model
plot(rFraTrain1) #visualize full model
points(fraseri) #add points to plot
# testing model for fraseri
# extract background points
bg1 <- randomPoints(predictors1, 1000)
# cross-validate model for fraseri
maxFraTest1 <- evaluate(maxFraTrain1, p=fraTest1, a=bg1, x=predictors1)
maxFraTest1 #print results
threshold(maxFraTest1) #identify threshold for presence or absence
plot(maxFraTest1, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for fraseri
pvtest1 <- data.frame(extract(predictors1, fraTest1))
avtest1 <- data.frame(extract(predictors1, bg1))
# cross-validate model
maxFraTest21 <- evaluate(maxFraTrain1, p=pvtest1, a=avtest1)
maxFraTest21
# Alternative 2: predict to testing points for fraseri
testp1 <- predict(maxFraTrain1, pvtest1)
testa1 <- predict(maxFraTrain1, avtest1)
maxFraTest31 <- evaluate(p=testp1, a=testa1)
maxFraTest31
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxFraAdv1 <- maxent(
  x=predictors1,
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
maxFraAdv1 #view output as html

# develop testing and training sets for hyacinthoides
fold <- kfold(hyacinthoides, k=5) #split occurence points into 5 sets
hyaTest1 <- hyacinthoides[fold == 1, ] #take 20% (1/5) for testing
hyaTrain1 <- hyacinthoides[fold != 1, ] #leave 40% for training
# fit training model for hyacinthoides
maxHyaTrain1 <- maxent(predictors1, hyaTrain1) #fit maxent model
maxHyaTrain1 #view results in html
rHyaTrain1 <- predict(maxHyaTrain1, predictors1) #predict full model
plot(rHyaTrain1) #visualize full model
points(hyacinthoides) #add points to plot
# testing model for hyacinthoides
# extract background points
bg1 <- randomPoints(predictors1, 1000)
# cross-validate model for hyacinthoides
maxHyaTest1 <- evaluate(maxHyaTrain1, p=hyaTest1, a=bg1, x=predictors1)
maxHyaTest1 #print results
threshold(maxHyaTest1) #identify threshold for presence or absence
plot(maxHyaTest1, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hyacinthoides
pvtest1 <- data.frame(extract(predictors1, hyaTest1))
avtest1 <- data.frame(extract(predictors1, bg1))
# cross-validate model
maxHyaTest21 <- evaluate(maxHyaTrain1, p=pvtest1, a=avtest1)
maxHyaTest21
# Alternative 2: predict to testing points for hyacinthoides
testp1 <- predict(maxHyaTrain1, pvtest1)
testa1 <- predict(maxHyaTrain1, avtest1)
maxHyaTest31 <- evaluate(p=testp1, a=testa1)
maxHyaTest31
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHyaAdv1 <- maxent(
  x=predictors1,
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
maxHyaAdv1 #view output as html

# develop testing and training sets for mobilense
fold <- kfold(mobilense, k=5) #split occurence points into 5 sets
mobTest1 <- mobilense[fold == 1, ] #take 20% (1/5) for testing
mobTrain1 <- mobilense[fold != 1, ] #leave 40% for training
# fit training model for mobilense
maxMobTrain1 <- maxent(predictors1, mobTrain1) #fit maxent model
maxMobTrain1 #view results in html
rMobTrain1 <- predict(maxMobTrain1, predictors1) #predict full model
plot(rMobTrain1) #visualize full model
points(mobilense) #add points to plot
# testing model for mobilense
# extract background points
bg1 <- randomPoints(predictors1, 1000)
# cross-validate model for mobilense
maxMobTest1 <- evaluate(maxMobTrain1, p=mobTest1, a=bg1, x=predictors1)
maxMobTest1 #print results
threshold(maxMobTest1) #identify threshold for presence or absence
plot(maxMobTest1, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for mobilense
pvtest1 <- data.frame(extract(predictors1, mobTest1))
avtest1 <- data.frame(extract(predictors1, bg1))
# cross-validate model
maxMobTest21 <- evaluate(maxMobTrain1, p=pvtest1, a=avtest1)
maxMobTest21
# Alternative 2: predict to testing points for mobilense
testp1 <- predict(maxMobTrain1, pvtest1)
testa1 <- predict(maxMobTrain1, avtest1)
maxMobTest31 <- evaluate(p=testp1, a=testa1)
maxMobTest31
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxMobAdv1 <- maxent(
  x=predictors1,
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
    'maximumiterations=1000' #default=500
  )
)
maxMobAdv1 #view output as html

# develop testing and training sets for parentals
fold <- kfold(parentals, k=5) #split occurence points into 5 sets
parTest1 <- parentals[fold == 1, ] #take 20% (1/5) for testing
parTrain1 <- parentals[fold != 1, ] #leave 40% for training
# fit training model for parentals
maxParTrain1 <- maxent(predictors1, parTrain1) #fit maxent model
maxParTrain1 #view results in html
rParTrain1 <- predict(maxParTrain1, predictors1) #predict full model
plot(rParTrain1) #visualize full model
points(parentals) #add points to plot
# testing model for parentals
# extract background points
bg1 <- randomPoints(predictors1, 1000)
# cross-validate model for parentals
maxParTest1 <- evaluate(maxParTrain1, p=parTest1, a=bg1, x=predictors1)
maxParTest1 #print results
threshold(maxParTest1) #identify threshold for presence or absence
plot(maxParTest1, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for parentals
pvtest1 <- data.frame(extract(predictors1, parTest1))
avtest1 <- data.frame(extract(predictors1, bg1))
# cross-validate model
maxParTest21 <- evaluate(maxParTrain1, p=pvtest1, a=avtest1)
maxParTest21
# Alternative 2: predict to testing points for parentals
testp1 <- predict(maxParTrain1, pvtest1)
testa1 <- predict(maxParTrain1, avtest1)
maxParTest31 <- evaluate(p=testp1, a=testa1)
maxParTest31
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxParAdv1 <- maxent(
  x=predictors1,
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
maxParAdv1 #view output as html

# develop testing and training sets for hybrids
fold <- kfold(hybrids, k=5) #split occurence points into 5 sets
hybTest1 <- hybrids[fold == 1, ] #take 20% (1/5) for testing
hybTrain1 <- hybrids[fold != 1, ] #leave 40% for training
# fit training model for hybrids
maxHybTrain1 <- maxent(predictors1, hybTrain1) #fit maxent model
maxHybTrain1 #view results in html
rHybTrain1 <- predict(maxHybTrain1, predictors1) #predict full model
plot(rHybTrain1) #visualize full model
points(hybrids) #add points to plot
# testing model for hybrids
# extract background points
bg1 <- randomPoints(predictors1, 1000)
# cross-validate model for hybrids
maxHybTest1 <- evaluate(maxHybTrain1, p=hybTest1, a=bg1, x=predictors1)
maxHybTest1 #print results
threshold(maxHybTest1) #identify threshold for presence or absence
plot(maxHybTest1, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hybrids
pvtest1 <- data.frame(extract(predictors1, hybTest1))
avtest1 <- data.frame(extract(predictors1, bg1))
# cross-validate model
maxHybTest21 <- evaluate(maxHybTrain1, p=pvtest1, a=avtest1)
maxHybTest21
# Alternative 2: predict to testing points for hybrids
testp1 <- predict(maxHybTrain1, pvtest1)
testa1 <- predict(maxHybTrain1, avtest1)
maxHybTest31 <- evaluate(p=testp1, a=testa1)
maxHybTest31
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxHybAdv1 <- maxent(
  x=predictors1,
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
maxHybAdv1 #view output as html

# develop testing and training sets for combined
fold <- kfold(combined, k=5) #split occurence points into 5 sets
comTest1 <- combined[fold == 1, ] #take 20% (1/5) for testing
comTrain1 <- combined[fold != 1, ] #leave 40% for training
# fit training model for combined
maxComTrain1 <- maxent(predictors1, comTrain1) #fit maxent model
maxComTrain1 #view results in html
rComTrain1 <- predict(maxComTrain1, predictors1) #predict full model
plot(rComTrain1) #visualize full model
points(combined) #add points to plot
# testing model for combined
# extract background points
bg1 <- randomPoints(predictors1, 1000)
# cross-validate model for combined
maxComTest1 <- evaluate(maxComTrain1, p=comTest1, a=bg1, x=predictors1)
maxComTest1 #print results
threshold(maxComTest1) #identify threshold for presence or absence
plot(maxComTest1, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for combined
pvtest1 <- data.frame(extract(predictors1, comTest1))
avtest1 <- data.frame(extract(predictors1, bg1))
# cross-validate model
maxComTest21 <- evaluate(maxComTrain1, p=pvtest1, a=avtest1)
maxComTest21
# Alternative 2: predict to testing points for combined
testp1 <- predict(maxComTrain1, pvtest1)
testa1 <- predict(maxComTrain1, avtest1)
maxComTest31 <- evaluate(p=testp1, a=testa1)
maxComTest31
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxComAdv1 <- maxent(
  x=predictors1,
  p=combined,
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
maxComAdv1 #view output as html