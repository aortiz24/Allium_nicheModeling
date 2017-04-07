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
#assign variety name to object
canadense <- (alliumcanadense1)

#assign scientific name to an object
target2<-c("Allium canadense var. ecristatum")
#filtered allium canadense ecristatum csv file
alliumcanadense2<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target2)
#assign variety name to object
ecristatum <- (alliumcanadense2)

#assign scientific name to an object
target3<-c("Allium canadense var. Fraseri")
#filtered allium canadense Fraseri csv file
alliumcanadense3<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target3)
#assign variety name to object
fraseri <- (alliumcanadense3)

#assign scientific name to an object
target4<-c("Allium canadense var. hyacinthoides")
#filtered allium canadense hyacinthoides csv file
alliumcanadense4<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target4)
#assign variety name to object
hyacinthoides <- (alliumcanadense4)

#assign scientific name to an object
target5<-c("Allium canadense var. lavendulare")
#filtered allium canadense lavendulare csv file
alliumcanadense5<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target5)
#assign variety name to object
lavendulare <- (alliumcanadense5)

#assign scientific name to an object
target6<-c("Allium canadense var. mobilense")
#filtered allium canadense mobilense csv file
alliumcanadense6<-alliumcanadense %>%
  select(Taxon,Collector,Latitude,Longitude) %>%
  filter(Taxon == target6)
#assign variety name to object
mobilense <- (alliumcanadense6)

##merging occurrence data
#merging occurrence data for parentals(mobilense,fraseri) into one R object
parentals<- merge(mobilense, fraseri, by="Collector", all=TRUE)

#merging occurrence data for hybrids(hyacinthoides,ecristatum,lavendulare) into one R object
hybrids<- merge(hyacinthoides, ecristatum, by="Collector", all=TRUE)

#merging occurrence data for all 6 varieties
combined1<- merge(lavendulare, ecristatum, by="Collector", all=TRUE)
combined2<- merge(combined1, fraseri, by="Collector", all=TRUE)
combined3<- merge(combined2, hyacinthoides, by="Collector", all=TRUE)
combined4<- merge(combined3, mobilense, by="Collector", all=TRUE)
combined<- merge(combined4, canadense, by="Collector", all=TRUE)

##prepare varieties,parentals,and hybrids for modeling
canadense <- canadense[,c(3,2)]
lavendulare <- lavendulare[,c(3,2)]
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
ppt0 <- raster("layers/avg_1930_ppt0.asc", crs=CRS)
tmax0 <- raster("layers/avg_1930_tmax0.asc", crs=CRS)
tmean0 <- raster("layers/avg_1930_tmean0.asc", crs=CRS)
tmin0 <- raster("layers/avg_1930_tmin0.asc", crs=CRS)
ppt1 <- raster("layers/avg_2014_ppt0.asc", crs=CRS)
tmax1 <- raster("layers/avg_2014_tmax0.asc", crs=CRS)
tmean1 <- raster("layers/avg_2014_tmean0.asc", crs=CRS)
tmin1 <- raster("layers/avg_2014_tmin0.asc", crs=CRS)
ppt9 <- raster("layers/ppt9.asc")
tmax9 <- raster("layers/tmax9.asc")
tmean9 <- raster("layers/tmean9.asc")
tmin9 <- raster("layers/tmin9.asc")
vpdmax9 <- raster("layers/vpdmax9.asc")
vpdmin9 <- raster("layers/vpdmin9.asc")
tdmean9 <- raster("layers/tdmean9.asc")
ppt11 <- raster("layers/ppt11.asc")
tmax11 <- raster("layers/tmax11.asc")
tmean11 <- raster("layers/tmean11.asc")
tmin11 <- raster("layers/tmin11.asc")
vpdmax11 <- raster("layers/vpdmax11.asc")
vpdmin11 <- raster("layers/vpdmin11.asc")
tdmean11 <- raster("layers/tdmean11.asc")

## create stack of non-correlated layers (as determined by layerPrep.R)
predictors0<- stack(ppt0)
predictors1<- stack(ppt1,tmax1)
predictors9<- stack(tmin9, ppt9, vpdmin9)
predictors11<- stack(ppt11, tdmean11)

# plot each layer individually
plot(predictors0)
plot(predictors1)
plot(predictors9)
plot(predictors11)

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

### basic bioclim modeling with PRISM 1929 layers for varieties, parentals, and hybrids
# extract layer data for each point
canPts9 <- extract(predictors9, canadense)
# create bioclim model
canBC9 <- bioclim(canPts9)
# predict bioclim model
canBCpredict9 <- predict(predictors9, canBC9)
# plot bioclim model
plot(canBCpredict9)

# extract layer data for each point
lavPts9 <- extract(predictors9, canadense)
# create bioclim model
lavBC9 <- bioclim(lavPts9)
# predict bioclim model
lavBCpredict9 <- predict(predictors9, lavBC9)
# plot bioclim model
plot(lavBCpredict9)

# extract layer data for each point
ecrPts9 <- extract(predictors9, ecristatum)
# create bioclim model
ecrBC9 <- bioclim(ecrPts9)
# predict bioclim model
ecrBCpredict9 <- predict(predictors9, ecrBC9)
# plot bioclim model
plot(ecrBCpredict9)

# extract layer data for each point
fraPts9 <- extract(predictors9, fraseri)
# create bioclim model
fraBC9 <- bioclim(fraPts9)
# predict bioclim model
fraBCpredict9 <- predict(predictors9, fraBC9)
# plot bioclim model
plot(fraBCpredict9)

# extract layer data for each point
hyaPts9 <- extract(predictors9, hyacinthoides)
# create bioclim model
hyaBC9 <- bioclim(hyaPts9)
# predict bioclim model
hyaBCpredict9 <- predict(predictors9, hyaBC9)
# plot bioclim model
plot(hyaBCpredict9)

# extract layer data for each point
mobPts9 <- extract(predictors9, mobilense)
# create bioclim model
mobBC9 <- bioclim(mobPts9)
# predict bioclim model
mobBCpredict9 <- predict(predictors9, mobBC9)
# plot bioclim model
plot(mobBCpredict9)

# extract layer data for each point
parPts9 <- extract(predictors9, parentals)
# create bioclim model
parBC9 <- bioclim(parPts9)
# predict bioclim model
parBCpredict9 <- predict(predictors9, parBC9)
# plot bioclim model
plot(parBCpredict9)

# extract layer data for each point
hybPts9 <- extract(predictors9, hybrids)
# create bioclim model
hybBC9 <- bioclim(hybPts9)
# predict bioclim model
hybBCpredict9 <- predict(predictors9, hybBC9)
# plot bioclim model
plot(hybBCpredict9)

# extract layer data for each point
comPts9 <- extract(predictors9, combined)
# create bioclim model
comBC9 <- bioclim(comPts9)
# predict bioclim model
comBCpredict9 <- predict(predictors9, comBC9)
# plot bioclim model
plot(comBCpredict9)

## Default maxent modeling
# run maxent for canadense (default parameters for dismo)
maxCan9 <- maxent(predictors9, canadense)
maxCan9 # views results in browser window
response(maxCan9) # show response curves for each layer
rCan9 <- predict(maxCan9, predictors9) # create model
plot(rCan9) # plot predictive model
points(canadense) # add points to predictive model
writeRaster(rCan9, "models/canadense1929.grd")

# run maxent for lavendulare (default parameters for dismo)
maxLav9 <- maxent(predictors9, lavendulare)
maxLav9 # views results in browser window
response(maxLav9) # show response curves for each layer
rLav9 <- predict(maxLav9, predictors9) # create model
plot(rLav9) # plot predictive model
points(lavendulare) # add points to predictive model
writeRaster(rLav9, "models/lavendulare1929.grd")

# run maxent for ecristatum (default parameters for dismo)
maxEcr9 <- maxent(predictors9, ecristatum) 
maxEcr9 # views results in browser window
response(maxEcr9) 
rEcr9 <- predict(maxEcr9, predictors9) 
plot(rEcr9)
points(ecristatum)
writeRaster(rEcr9, "models/ecristatum1929.grd")

# run maxent for fraseri (default parameters for dismo)
maxFra9 <- maxent(predictors9, fraseri)
maxFra9 # views results in browser window
response(maxFra9) # show response curves for each layer
rFra9 <- predict(maxFra9, predictors9) # create model
plot(rFra9) # plot predictive model
points(fraseri) # add points to predictive model
writeRaster(rFra9, "models/fraseri1929.grd")

# run maxent for hyacinthoides (default parameters for dismo)
maxHya9 <- maxent(predictors9, hyacinthoides) 
maxHya9 
response(maxHya9) 
rHya9 <- predict(maxHya9, predictors9) 
plot(rHya9)
points(hyacinthoides)
writeRaster(rHya9, "models/hyacinthoides1929.grd")

# run maxent for mobilense (default parameters for dismo)
maxMob9 <- maxent(predictors9, mobilense)
maxMob9 # views results in browser window
response(maxMob9) # show response curves for each layer
rMob9 <- predict(maxMob9, predictors9) # create model
plot(rMob9) # plot predictive model
points(mobilense) # add points to predictive model
writeRaster(rMob9, "models/mobilense1929.grd")

# run maxent for parentals (default parameters for dismo)
maxPar9 <- maxent(predictors9, parentals)
maxPar9 # views results in browser window
response(maxPar9) # show response curves for each layer
rPar9 <- predict(maxPar9, predictors9) # create model
plot(rPar9) # plot predictive model
points(parentals) # add points to predictive model
writeRaster(rPar9, "models/parentals1929.grd")

# run maxent for hybrids (default parameters for dismo)
maxHyb9 <- maxent(predictors9, hybrids) 
maxHyb9 
response(maxHyb9) 
rHyb9 <- predict(maxHyb9, predictors9) 
plot(rHyb9)
points(hybrids)
writeRaster(rHyb9, "models/hybrids1929.grd")

# run maxent for combined (default parameters for dismo)
maxCom9 <- maxent(predictors9, combined)
maxCom9 # views results in browser window
response(maxCom9) # show response curves for each layer
rCom9 <- predict(maxCom9, predictors9) # create model
plot(rCom9) # plot predictive model
points(combined) # add points to predictive model
writeRaster(rCom9, "models/combined1929.grd")

## Advanced modeling
# develop testing and training sets for canadense
fold <- kfold(canadense, k=5) #split occurence points into 5 sets
canTest9 <- canadense[fold == 1, ] #take 20% (1/5) for testing
canTrain9 <- canadense[fold != 1, ] #leave 40% for training
# fit training model for canadense
maxCanTrain9 <- maxent(predictors9, canTrain9) #fit maxent model
maxCanTrain9 #view results in html
rCanTrain9 <- predict(maxCanTrain9, predictors9) #predict full model
plot(rCanTrain9) #visualize full model
points(canadense) #add points to plot
# testing model for canadense
# extract background points
bg9 <- randomPoints(predictors9, 1000)
# cross-validate model for canadense
maxCanTest9 <- evaluate(maxlavTrain9, p=lavTest9, a=bg9, x=predictors9)
maxCanTest9 #print results
threshold(maxCanTest9) #identify threshold for presence or absence
plot(maxlavTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for canadense
pvtest9 <- data.frame(extract(predictors9, lavTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxCanTest29 <- evaluate(maxCanTrain, p=pvtest9, a=avtest9)
maxCanTest29
# Alternative 2: predict to testing points for canadense
testp9 <- predict(maxCanTrain9, pvtest9)
testa9 <- predict(maxCanTrain9, avtest9)
maxCanTest39 <- evaluate(p=testp9, a=testa9)
maxCanTest39
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
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxCanAdv9 #view output as html

# develop testing and training sets for lavendulare
fold <- kfold(lavendulare, k=5) #split occurence points into 5 sets
lavTest9 <- lavendulare[fold == 1, ] #take 20% (1/5) for testing
lavTrain9 <- lavendulare[fold != 1, ] #leave 40% for training
# fit training model for lavendulare
maxLavTrain9 <- maxent(predictors9, lavTrain9) #fit maxent model
maxLavTrain9 #view results in html
rLavTrain9 <- predict(maxLavTrain9, predictors9) #predict full model
plot(rLavTrain9) #visualize full model
points(lavendulare) #add points to plot
# testing model for lavendulare
# extract background points
bg9 <- randomPoints(predictors9, 1000)
# cross-validate model for lavendulare
maxLavTest9 <- evaluate(maxLavTrain9, p=lavTest9, a=bg9, x=predictors9)
maxLavTest9 #print results
threshold(maxLavTest9) #identify threshold for presence or absence
plot(maxLavTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for lavendulare
pvtest9 <- data.frame(extract(predictors9, lavTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxLavTest29 <- evaluate(maxLavTrain9, p=pvtest9, a=avtest9)
maxLavTest29
# Alternative 2: predict to testing points for lavendulare
testp9 <- predict(maxLavTrain9, pvtest9)
testa9 <- predict(maxLavTrain9, avtest9)
maxLavTest39 <- evaluate(p=testp9, a=testa9)
maxLavTest39
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
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxLavAdv9 #view output as html

# develop testing and training sets for ecristatum
fold <- kfold(ecristatum, k=5) #split occurence points into 5 sets
ecrTest9 <- ecristatum[fold == 1, ] #take 20% (1/5) for testing
ecrTrain9 <- ecristatum[fold != 1, ] #leave 40% for training
# fit training model for ecristatum
maxEcrTrain9 <- maxent(predictors9, ecrTrain9) #fit maxent model
maxEcrTrain9 #view results in html
rEcrTrain9 <- predict(maxEcrTrain9, predictors9) #predict full model
plot(rEcrTrain9) #visualize full model
points(ecristatum) #add points to plot
# testing model for ecristatum
# extract background points
bg9 <- randomPoints(predictors9, 1000)
# cross-validate model for ecristatum
maxEcrTest9 <- evaluate(maxEcrTrain9, p=ecrTest9, a=bg9, x=predictors9)
maxEcrTest9 #print results
threshold(maxEcrTest9) #identify threshold for presence or absence
plot(maxEcrTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for ecristatum
pvtest9 <- data.frame(extract(predictors9, ecrTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxEcrTest29 <- evaluate(maxEcrTrain9, p=pvtest9, a=avtest9)
maxEcrTest29
# Alternative 2: predict to testing points for ecristatum
testp9 <- predict(maxEcrTrain9, pvtest9)
testa9 <- predict(maxEcrTrain9, avtest9)
maxEcrTest39 <- evaluate(p=testp9, a=testa9)
maxEcrTest39
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
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxEcrAdv9 #view output as html

# develop testing and training sets for fraseri
fold <- kfold(fraseri, k=5) #split occurence points into 5 sets
fraTest9 <- fraseri[fold == 1, ] #take 20% (1/5) for testing
fraTrain9 <- fraseri[fold != 1, ] #leave 40% for training
# fit training model for fraseri
maxFraTrain9 <- maxent(predictors9, fraTrain9) #fit maxent model
maxFraTrain9 #view results in html
rFraTrain9 <- predict(maxFraTrain9, predictors9) #predict full model
plot(rFraTrain9) #visualize full model
points(fraseri) #add points to plot
# testing model for fraseri
# extract background points
bg9 <- randomPoints(predictors9, 1000)
# cross-validate model for fraseri
maxFraTest9 <- evaluate(maxFraTrain9, p=fraTest9, a=bg9, x=predictors9)
maxFraTest9 #print results
threshold(maxFraTest9) #identify threshold for presence or absence
plot(maxFraTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for fraseri
pvtest9 <- data.frame(extract(predictors9, fraTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxFraTest29 <- evaluate(maxFraTrain9, p=pvtest9, a=avtest9)
maxFraTest29
# Alternative 2: predict to testing points for fraseri
testp9 <- predict(maxFraTrain9, pvtest9)
testa9 <- predict(maxFraTrain9, avtest9)
maxFraTest39 <- evaluate(p=testp9, a=testa9)
maxFraTest39
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
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxFraAdv9 #view output as html

# develop testing and training sets for hyacinthoides
fold <- kfold(hyacinthoides, k=5) #split occurence points into 5 sets
hyaTest9 <- hyacinthoides[fold == 1, ] #take 20% (1/5) for testing
hyaTrain9 <- hyacinthoides[fold != 1, ] #leave 40% for training
# fit training model for hyacinthoides
maxHyaTrain9 <- maxent(predictors9, hyaTrain9) #fit maxent model
maxHyaTrain9 #view results in html
rHyaTrain9 <- predict(maxHyaTrain9, predictors9) #predict full model
plot(rHyaTrain9) #visualize full model
points(hyacinthoides) #add points to plot
# testing model for hyacinthoides
# extract background points
bg9 <- randomPoints(predictors9, 1000)
# cross-validate model for hyacinthoides
maxHyaTest9 <- evaluate(maxHyaTrain9, p=hyaTest9, a=bg9, x=predictors9)
maxHyaTest9 #print results
threshold(maxHyaTest9) #identify threshold for presence or absence
plot(maxHyaTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hyacinthoides
pvtest9 <- data.frame(extract(predictors9, hyaTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxHyaTest29 <- evaluate(maxHyaTrain9, p=pvtest9, a=avtest9)
maxHyaTest29
# Alternative 2: predict to testing points for hyacinthoides
testp9 <- predict(maxHyaTrain9, pvtest9)
testa9 <- predict(maxHyaTrain9, avtest9)
maxHyaTest39 <- evaluate(p=testp9, a=testa9)
maxHyaTest39
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
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxHyaAdv9 #view output as html

# develop testing and training sets for mobilense
fold <- kfold(mobilense, k=5) #split occurence points into 5 sets
mobTest9 <- mobilense[fold == 1, ] #take 20% (1/5) for testing
mobTrain9 <- mobilense[fold != 1, ] #leave 40% for training
# fit training model for mobilense
maxMobTrain9 <- maxent(predictors9, mobTrain9) #fit maxent model
maxMobTrain9 #view results in html
rMobTrain9 <- predict(maxMobTrain9, predictors9) #predict full model
plot(rMobTrain9) #visualize full model
points(mobilense) #add points to plot
# testing model for mobilense
# extract background points
bg9 <- randomPoints(predictors9, 1000)
# cross-validate model for mobilense
maxMobTest9 <- evaluate(maxMobTrain9, p=mobTest9, a=bg9, x=predictors9)
maxMobTest9 #print results
threshold(maxMobTest9) #identify threshold for presence or absence
plot(maxMobTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for mobilense
pvtest9 <- data.frame(extract(predictors9, mobTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxMobTest29 <- evaluate(maxMobTrain9, p=pvtest9, a=avtest9)
maxMobTest29
# Alternative 2: predict to testing points for mobilense
testp9 <- predict(maxMobTrain9, pvtest9)
testa9 <- predict(maxMobTrain9, avtest9)
maxMobTest39 <- evaluate(p=testp9, a=testa9)
maxMobTest39
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
    'maximumiterations=1000' #default=500
  )
)
maxMobAdv9 #view output as html

# develop testing and training sets for parentals
fold <- kfold(parentals, k=5) #split occurence points into 5 sets
parTest9 <- parentals[fold == 1, ] #take 20% (1/5) for testing
parTrain9 <- parentals[fold != 1, ] #leave 40% for training
# fit training model for parentals
maxParTrain9 <- maxent(predictors9, parTrain9) #fit maxent model
maxParTrain9 #view results in html
rParTrain9 <- predict(maxParTrain9, predictors9) #predict full model
plot(rParTrain9) #visualize full model
points(parentals) #add points to plot
# testing model for parentals
# extract background points
bg9 <- randomPoints(predictors9, 1000)
# cross-validate model for parentals
maxParTest9 <- evaluate(maxParTrain9, p=parTest9, a=bg9, x=predictors9)
maxParTest9 #print results
threshold(maxParTest9) #identify threshold for presence or absence
plot(maxParTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for parentals
pvtest9 <- data.frame(extract(predictors9, parTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxParTest29 <- evaluate(maxParTrain9, p=pvtest9, a=avtest9)
maxParTest29
# Alternative 2: predict to testing points for parentals
testp9 <- predict(maxParTrain9, pvtest9)
testa9 <- predict(maxParTrain9, avtest9)
maxParTest39 <- evaluate(p=testp9, a=testa9)
maxParTest39
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
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxParAdv9 #view output as html

# develop testing and training sets for hybrids
fold <- kfold(hybrids, k=5) #split occurence points into 5 sets
hybTest9 <- hybrids[fold == 1, ] #take 20% (1/5) for testing
hybTrain9 <- hybrids[fold != 1, ] #leave 40% for training
# fit training model for hybrids
maxHybTrain9 <- maxent(predictors9, hybTrain9) #fit maxent model
maxHybTrain9 #view results in html
rHybTrain9 <- predict(maxHybTrain9, predictors9) #predict full model
plot(rHybTrain9) #visualize full model
points(hybrids) #add points to plot
# testing model for hybrids
# extract background points
bg9 <- randomPoints(predictors9, 1000)
# cross-validate model for hybrids
maxHybTest9 <- evaluate(maxHybTrain9, p=hybTest9, a=bg9, x=predictors9)
maxHybTest9 #print results
threshold(maxHybTest9) #identify threshold for presence or absence
plot(maxHybTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hybrids
pvtest9 <- data.frame(extract(predictors9, hybTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxHybTest29 <- evaluate(maxHybTrain9, p=pvtest9, a=avtest9)
maxHybTest29
# Alternative 2: predict to testing points for hybrids
testp9 <- predict(maxHybTrain9, pvtest9)
testa9 <- predict(maxHybTrain9, avtest9)
maxHybTest39 <- evaluate(p=testp9, a=testa9)
maxHybTest39
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
    'jackknife=true', #default=false
    'replicates=10', #default=1
    'replicatetype=crossvalidate',
    'maximumiterations=1000' #default=500
  )
)
maxHybAdv9 #view output as html

# develop testing and training sets for combined
fold <- kfold(combined, k=5) #split occurence points into 5 sets
comTest9 <- combined[fold == 1, ] #take 20% (1/5) for testing
comTrain9 <- combined[fold != 1, ] #leave 40% for training
# fit training model for combined
maxComTrain9 <- maxent(predictors9, comTrain9) #fit maxent model
maxComTrain9 #view results in html
rComTrain9 <- predict(maxComTrain9, predictors9) #predict full model
plot(rComTrain9) #visualize full model
points(combined) #add points to plot
# testing model for combined
# extract background points
bg9 <- randomPoints(predictors9, 1000)
# cross-validate model for combined
maxComTest9 <- evaluate(maxComTrain9, p=comTest9, a=bg9, x=predictors9)
maxComTest9 #print results
threshold(maxComTest9) #identify threshold for presence or absence
plot(maxComTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for combined
pvtest9 <- data.frame(extract(predictors9, comTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxComTest29 <- evaluate(maxComTrain9, p=pvtest9, a=avtest9)
maxComTest29
# Alternative 2: predict to testing points for combined
testp9 <- predict(maxComTrain9, pvtest9)
testa9 <- predict(maxComTrain9, avtest9)
maxComTest39 <- evaluate(p=testp9, a=testa9)
maxComTest39
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxComAdv9 <- maxent(
  x=predictors9,
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
maxComAdv9 #view output as html
