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
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target1)
#assign variety name to object
canadense <- (alliumcanadense1)

#assign scientific name to an object
target2<-c("Allium canadense var. ecristatum")
#filtered allium canadense ecristatum csv file
alliumcanadense2<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target2)
#assign variety name to object
ecristatum <- (alliumcanadense2)

#assign scientific name to an object
target3<-c("Allium canadense var. Fraseri")
#filtered allium canadense Fraseri csv file
alliumcanadense3<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target3)
#assign variety name to object
fraseri <- (alliumcanadense3)

#assign scientific name to an object
target4<-c("Allium canadense var. hyacinthoides")
#filtered allium canadense hyacinthoides csv file
alliumcanadense4<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target4)
#assign variety name to object
hyacinthoides <- (alliumcanadense4)

#assign scientific name to an object
target5<-c("Allium canadense var. lavendulare")
#filtered allium canadense lavendulare csv file
alliumcanadense5<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target5)
#assign variety name to object
lavendulare <- (alliumcanadense5)
#remove point that was outside of climate layer extent
lavendulare<-lavendulare[c(1:6),]

#assign scientific name to an object
target6<-c("Allium canadense var. mobilense")
#filtered allium canadense mobilense csv file
alliumcanadense6<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target6)
#assign variety name to object
mobilense <- (alliumcanadense6)

#assign parentals(mobilense,fraseri) to a R object
parentals1<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude)
parentals<-parentals11[c(23:31,41:55),]
parentals<-na.omit(parentals)

#assign hybrids(hyacinthoides,ecristatum,lavendulare) to a R object
hybrids1<- alliumcanadense %>%
  select(Taxon,Latitude,Longitude)
hybrids<-hybrids11[c(22,32:40),]
hybrids<-na.omit(hybrids)

#assign scientific name to an object containing occurrence for all 6 varieties
combined<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude)

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

# plot each layer individually
plot(predictors9)
plot(predictors11)

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
maxCanTest9 <- evaluate(maxCanTrain9, p=canTest9, a=bg9, x=predictors9)
maxCanTest9 #print results
threshold(maxCanTest9) #identify threshold for presence or absence
plot(maxlavTest9, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for canadense
pvtest9 <- data.frame(extract(predictors9, canTest9))
avtest9 <- data.frame(extract(predictors9, bg9))
# cross-validate model
maxCanTest29 <- evaluate(maxCanTrain9, p=pvtest9, a=avtest9)
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
response(maxCanAdv9) # show response curves for each layer
rCanAdv9 <- predict(maxCanAdv9, predictors9) # create model
plot(rCanAdv9) # plot predictive model
points(canadense) # add points to predictive model
writeRaster(rCanAdv9, "models/canadenseAdv1929.grd")

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
    'maximumiterations=1000' #default=500
  )
)
maxLavAdv9 #view output as html
response(maxLavAdv9) # show response curves for each layer
rLavAdv9 <- predict(maxLavAdv9, predictors9) # create model
plot(rLavAdv9) # plot predictive model
points(lavendulare) # add points to predictive model
writeRaster(rLavAdv9, "models/lavendulareAdv1929.grd")

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
    'maximumiterations=1000' #default=500
  )
)
maxEcrAdv9 #view output as html
response(maxEcrAdv9) # show response curves for each layer
rEcrAdv9 <- predict(maxEcrAdv9, predictors9) # create model
plot(rEcrAdv9) # plot predictive model
points(ecristatum) # add points to predictive model
writeRaster(rEcrAdv9, "models/ecristatumAdv1929.grd")

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
    'maximumiterations=1000' #default=500
  )
)
maxFraAdv9 #view output as html
response(maxFraAdv9) # show response curves for each layer
rFraAdv9 <- predict(maxFraAdv9, predictors9) # create model
plot(rFraAdv9) # plot predictive model
points(fraseri) # add points to predictive model
writeRaster(rFraAdv9, "models/fraseriAdv1929.grd")

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
    'maximumiterations=1000' #default=500
  )
)
maxHyaAdv9 #view output as html
response(maxHyaAdv9) # show response curves for each layer
rHyaAdv9 <- predict(maxHyaAdv9, predictors9) # create model
plot(rHyaAdv9) # plot predictive model
points(hyacinthoides) # add points to predictive model
writeRaster(rHyaAdv9, "models/hyacinthoidesAdv1929.grd")

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
response(maxMobAdv9) # show response curves for each layer
rMobAdv9 <- predict(maxMobAdv9, predictors9) # create model
plot(rMobAdv9) # plot predictive model
points(mobilense) # add points to predictive model
writeRaster(rMobAdv9, "models/mobilenseAdv1929.grd")

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
response(maxParAdv9) # show response curves for each layer
rParAdv9 <- predict(maxParAdv9, predictors9) # create model
plot(rParAdv9) # plot predictive model
points(parentals) # add points to predictive model
writeRaster(rParAdv9, "models/parentalsAdv1929.grd")

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
    'maximumiterations=1000' #default=500
  )
)
maxHybAdv9 #view output as html
response(maxHybAdv9) # show response curves for each layer
rHybAdv9 <- predict(maxHybAdv9, predictors9) # create model
plot(rHybAdv9) # plot predictive model
points(hybrids) # add points to predictive model
writeRaster(rHybAdv9, "models/hybridsAdv1929.grd")

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
response(maxComAdv9) # show response curves for each layer
rComAdv9 <- predict(maxComAdv9, predictors9) # create model
plot(rComAdv9) # plot predictive model
points(combined) # add points to predictive model
writeRaster(rComAdv9, "models/combinedAdv1929.grd")

### basic bioclim modeling with PRISM 2011 layers for varieties, parentals, and hybrids
# extract layer data for each point
canPts11 <- extract(predictors11, canadense)
# create bioclim model
canBC11 <- bioclim(canPts11)
# predict bioclim model
canBCpredict11 <- predict(predictors11, canBC11)
# plot bioclim model
plot(canBCpredict11)

# extract layer data for each point
lavPts11 <- extract(predictors11, canadense)
# create bioclim model
lavBC11 <- bioclim(lavPts11)
# predict bioclim model
lavBCpredict11 <- predict(predictors11, lavBC11)
# plot bioclim model
plot(lavBCpredict11)

# extract layer data for each point
ecrPts11 <- extract(predictors11, ecristatum)
# create bioclim model
ecrBC11 <- bioclim(ecrPts11)
# predict bioclim model
ecrBCpredict11 <- predict(predictors11, ecrBC11)
# plot bioclim model
plot(ecrBCpredict11)

# extract layer data for each point
fraPts11 <- extract(predictors11, fraseri)
# create bioclim model
fraBC11 <- bioclim(fraPts11)
# predict bioclim model
fraBCpredict11 <- predict(predictors11, fraBC11)
# plot bioclim model
plot(fraBCpredict11)

# extract layer data for each point
hyaPts11 <- extract(predictors11, hyacinthoides)
# create bioclim model
hyaBC11 <- bioclim(hyaPts11)
# predict bioclim model
hyaBCpredict11 <- predict(predictors11, hyaBC11)
# plot bioclim model
plot(hyaBCpredict11)

# extract layer data for each point
mobPts11 <- extract(predictors11, mobilense)
# create bioclim model
mobBC11 <- bioclim(mobPts11)
# predict bioclim model
mobBCpredict11 <- predict(predictors11, mobBC11)
# plot bioclim model
plot(mobBCpredict11)

# extract layer data for each point
parPts11 <- extract(predictors11, parentals)
# create bioclim model
parBC11 <- bioclim(parPts11)
# predict bioclim model
parBCpredict11 <- predict(predictors11, parBC11)
# plot bioclim model
plot(parBCpredict11)

# extract layer data for each point
hybPts11 <- extract(predictors11, hybrids)
# create bioclim model
hybBC11 <- bioclim(hybPts11)
# predict bioclim model
hybBCpredict11 <- predict(predictors11, hybBC11)
# plot bioclim model
plot(hybBCpredict11)

# extract layer data for each point
comPts11 <- extract(predictors11, combined)
# create bioclim model
comBC11 <- bioclim(comPts11)
# predict bioclim model
comBCpredict11 <- predict(predictors11, comBC11)
# plot bioclim model
plot(comBCpredict11)

## Default maxent modeling
# run maxent for canadense (default parameters for dismo)
maxCan11 <- maxent(predictors11, canadense)
maxCan11 # views results in browser window
response(maxCan11) # show response curves for each layer
rCan11 <- predict(maxCan11, predictors11) # create model
plot(rCan11) # plot predictive model
points(canadense) # add points to predictive model
writeRaster(rCan11, "models/canadense2011.grd")

# run maxent for lavendulare (default parameters for dismo)
maxLav11 <- maxent(predictors11, lavendulare)
maxLav11 # views results in browser window
response(maxLav11) # show response curves for each layer
rLav11 <- predict(maxLav11, predictors11) # create model
plot(rLav11) # plot predictive model
points(lavendulare) # add points to predictive model
writeRaster(rLav11, "models/lavendulare2011.grd")

# run maxent for ecristatum (default parameters for dismo)
maxEcr11 <- maxent(predictors11, ecristatum) 
maxEcr11 # views results in browser window
response(maxEcr11) 
rEcr11 <- predict(maxEcr11, predictors11) 
plot(rEcr11)
points(ecristatum)
writeRaster(rEcr11, "models/ecristatum2011.grd")

# run maxent for fraseri (default parameters for dismo)
maxFra11 <- maxent(predictors11, fraseri)
maxFra11 # views results in browser window
response(maxFra11) # show response curves for each layer
rFra11 <- predict(maxFra11, predictors11) # create model
plot(rFra11) # plot predictive model
points(fraseri) # add points to predictive model
writeRaster(rFra11, "models/fraseri2011.grd")

# run maxent for hyacinthoides (default parameters for dismo)
maxHya11 <- maxent(predictors11, hyacinthoides) 
maxHya11 
response(maxHya11) 
rHya11 <- predict(maxHya11, predictors11) 
plot(rHya11)
points(hyacinthoides)
writeRaster(rHya11, "models/hyacinthoides2011.grd")

# run maxent for mobilense (default parameters for dismo)
maxMob11 <- maxent(predictors11, mobilense)
maxMob11 # views results in browser window
response(maxMob11) # show response curves for each layer
rMob11 <- predict(maxMob11, predictors11) # create model
plot(rMob11) # plot predictive model
points(mobilense) # add points to predictive model
writeRaster(rMob11, "models/mobilense2011.grd")

# run maxent for parentals (default parameters for dismo)
maxPar11 <- maxent(predictors11, parentals)
maxPar11 # views results in browser window
response(maxPar11) # show response curves for each layer
rPar11 <- predict(maxPar11, predictors11) # create model
plot(rPar11) # plot predictive model
points(parentals) # add points to predictive model
writeRaster(rPar11, "models/parentals2011.grd")

# run maxent for hybrids (default parameters for dismo)
maxHyb11 <- maxent(predictors11, hybrids) 
maxHyb11 
response(maxHyb11) 
rHyb11 <- predict(maxHyb11, predictors11) 
plot(rHyb11)
points(hybrids)
writeRaster(rHyb11, "models/hybrids2011.grd")

# run maxent for combined (default parameters for dismo)
maxCom11 <- maxent(predictors11, combined)
maxCom11 # views results in browser window
response(maxCom11) # show response curves for each layer
rCom11 <- predict(maxCom11, predictors11) # create model
plot(rCom11) # plot predictive model
points(combined) # add points to predictive model
writeRaster(rCom11, "models/combined2011.grd")

## Advanced modeling
# develop testing and training sets for canadense
fold <- kfold(canadense, k=5) #split occurence points into 5 sets
canTest11 <- canadense[fold == 1, ] #take 20% (1/5) for testing
canTrain11 <- canadense[fold != 1, ] #leave 40% for training
# fit training model for canadense
maxCanTrain11 <- maxent(predictors11, canTrain11) #fit maxent model
maxCanTrain11 #view results in html
rCanTrain11 <- predict(maxCanTrain11, predictors11) #predict full model
plot(rCanTrain11) #visualize full model
points(canadense) #add points to plot
# testing model for canadense
# extract background points
bg11 <- randomPoints(predictors11, 1000)
# cross-validate model for canadense
maxCanTest11 <- evaluate(maxlavTrain11, p=lavTest11, a=bg11, x=predictors11)
maxCanTest11 #print results
threshold(maxCanTest11) #identify threshold for presence or absence
plot(maxlavTest11, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for canadense
pvtest11 <- data.frame(extract(predictors11, lavTest11))
avtest11 <- data.frame(extract(predictors11, bg11))
# cross-validate model
maxCanTest211 <- evaluate(maxCanTrain, p=pvtest11, a=avtest11)
maxCanTest211
# Alternative 2: predict to testing points for canadense
testp11 <- predict(maxCanTrain11, pvtest11)
testa11 <- predict(maxCanTrain11, avtest11)
maxCanTest311 <- evaluate(p=testp11, a=testa11)
maxCanTest311
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
    'maximumiterations=1000' #default=500
  )
)
maxCanAdv11 #view output as html
response(maxCanAdv11) # show response curves for each layer
rCanAdv11 <- predict(maxCanAdv11, predictors11) # create model
plot(rCanAdv11) # plot predictive model
points(canadense) # add points to predictive model
writeRaster(rCanAdv11, "models/canadenseAdv2011.grd")

# develop testing and training sets for lavendulare
fold <- kfold(lavendulare, k=5) #split occurence points into 5 sets
lavTest11 <- lavendulare[fold == 1, ] #take 20% (1/5) for testing
lavTrain11 <- lavendulare[fold != 1, ] #leave 40% for training
# fit training model for lavendulare
maxLavTrain11 <- maxent(predictors11, lavTrain11) #fit maxent model
maxLavTrain11 #view results in html
rLavTrain11 <- predict(maxLavTrain11, predictors11) #predict full model
plot(rLavTrain11) #visualize full model
points(lavendulare) #add points to plot
# testing model for lavendulare
# extract background points
bg11 <- randomPoints(predictors11, 1000)
# cross-validate model for lavendulare
maxLavTest11 <- evaluate(maxLavTrain11, p=lavTest11, a=bg11, x=predictors11)
maxLavTest11 #print results
threshold(maxLavTest11) #identify threshold for presence or absence
plot(maxLavTest11, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for lavendulare
pvtest11 <- data.frame(extract(predictors11, lavTest11))
avtest11 <- data.frame(extract(predictors11, bg11))
# cross-validate model
maxLavTest211 <- evaluate(maxLavTrain11, p=pvtest11, a=avtest11)
maxLavTest211
# Alternative 2: predict to testing points for lavendulare
testp11 <- predict(maxLavTrain11, pvtest11)
testa11 <- predict(maxLavTrain11, avtest11)
maxLavTest311 <- evaluate(p=testp11, a=testa11)
maxLavTest311
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
    'maximumiterations=1000' #default=500
  )
)
maxLavAdv11 #view output as html
response(maxLavAdv11) # show response curves for each layer
rLavAdv11 <- predict(maxLavAdv11, predictors11) # create model
plot(rLavAdv11) # plot predictive model
points(lavendulare) # add points to predictive model
writeRaster(rLavAdv11, "models/lavendulareAdv2011.grd")

# develop testing and training sets for ecristatum
fold <- kfold(ecristatum, k=5) #split occurence points into 5 sets
ecrTest11 <- ecristatum[fold == 1, ] #take 20% (1/5) for testing
ecrTrain11 <- ecristatum[fold != 1, ] #leave 40% for training
# fit training model for ecristatum
maxEcrTrain11 <- maxent(predictors11, ecrTrain11) #fit maxent model
maxEcrTrain11 #view results in html
rEcrTrain11 <- predict(maxEcrTrain11, predictors11) #predict full model
plot(rEcrTrain11) #visualize full model
points(ecristatum) #add points to plot
# testing model for ecristatum
# extract background points
bg11 <- randomPoints(predictors11, 1000)
# cross-validate model for ecristatum
maxEcrTest11 <- evaluate(maxEcrTrain11, p=ecrTest11, a=bg11, x=predictors11)
maxEcrTest11 #print results
threshold(maxEcrTest11) #identify threshold for presence or absence
plot(maxEcrTest11, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for ecristatum
pvtest11 <- data.frame(extract(predictors11, ecrTest11))
avtest11 <- data.frame(extract(predictors11, bg11))
# cross-validate model
maxEcrTest211 <- evaluate(maxEcrTrain11, p=pvtest11, a=avtest11)
maxEcrTest211
# Alternative 2: predict to testing points for ecristatum
testp11 <- predict(maxEcrTrain11, pvtest11)
testa11 <- predict(maxEcrTrain11, avtest11)
maxEcrTest311 <- evaluate(p=testp11, a=testa11)
maxEcrTest311
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
    'maximumiterations=1000' #default=500
  )
)
maxEcrAdv11 #view output as html
response(maxEcrAdv11) # show response curves for each layer
rEcrAdv11 <- predict(maxEcrAdv11, predictors11) # create model
plot(rEcrAdv11) # plot predictive model
points(ecristatum) # add points to predictive model
writeRaster(rEcrAdv11, "models/ecristatumAdv2011.grd")

# develop testing and training sets for fraseri
fold <- kfold(fraseri, k=5) #split occurence points into 5 sets
fraTest11 <- fraseri[fold == 1, ] #take 20% (1/5) for testing
fraTrain11 <- fraseri[fold != 1, ] #leave 40% for training
# fit training model for fraseri
maxFraTrain11 <- maxent(predictors11, fraTrain11) #fit maxent model
maxFraTrain11 #view results in html
rFraTrain11 <- predict(maxFraTrain11, predictors11) #predict full model
plot(rFraTrain11) #visualize full model
points(fraseri) #add points to plot
# testing model for fraseri
# extract background points
bg11 <- randomPoints(predictors11, 1000)
# cross-validate model for fraseri
maxFraTest11 <- evaluate(maxFraTrain11, p=fraTest11, a=bg11, x=predictors11)
maxFraTest11 #print results
threshold(maxFraTest11) #identify threshold for presence or absence
plot(maxFraTest11, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for fraseri
pvtest11 <- data.frame(extract(predictors11, fraTest11))
avtest11 <- data.frame(extract(predictors11, bg11))
# cross-validate model
maxFraTest211 <- evaluate(maxFraTrain11, p=pvtest11, a=avtest11)
maxFraTest211
# Alternative 2: predict to testing points for fraseri
testp11 <- predict(maxFraTrain11, pvtest11)
testa11 <- predict(maxFraTrain11, avtest11)
maxFraTest311 <- evaluate(p=testp11, a=testa11)
maxFraTest311
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
    'maximumiterations=1000' #default=500
  )
)
maxFraAdv11 #view output as html
response(maxFraAdv11) # show response curves for each layer
rFraAdv11 <- predict(maxFraAdv11, predictors11) # create model
plot(rFraAdv11) # plot predictive model
points(fraseri) # add points to predictive model
writeRaster(rFraAdv11, "models/fraseriAdv2011.grd")

# develop testing and training sets for hyacinthoides
fold <- kfold(hyacinthoides, k=5) #split occurence points into 5 sets
hyaTest11 <- hyacinthoides[fold == 1, ] #take 20% (1/5) for testing
hyaTrain11 <- hyacinthoides[fold != 1, ] #leave 40% for training
# fit training model for hyacinthoides
maxHyaTrain11 <- maxent(predictors11, hyaTrain11) #fit maxent model
maxHyaTrain11 #view results in html
rHyaTrain11 <- predict(maxHyaTrain11, predictors11) #predict full model
plot(rHyaTrain11) #visualize full model
points(hyacinthoides) #add points to plot
# testing model for hyacinthoides
# extract background points
bg11 <- randomPoints(predictors11, 1000)
# cross-validate model for hyacinthoides
maxHyaTest11 <- evaluate(maxHyaTrain11, p=hyaTest11, a=bg11, x=predictors11)
maxHyaTest11 #print results
threshold(maxHyaTest11) #identify threshold for presence or absence
plot(maxHyaTest11, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hyacinthoides
pvtest11 <- data.frame(extract(predictors11, hyaTest11))
avtest11 <- data.frame(extract(predictors11, bg11))
# cross-validate model
maxHyaTest211 <- evaluate(maxHyaTrain11, p=pvtest11, a=avtest11)
maxHyaTest211
# Alternative 2: predict to testing points for hyacinthoides
testp11 <- predict(maxHyaTrain11, pvtest11)
testa11 <- predict(maxHyaTrain11, avtest11)
maxHyaTest311 <- evaluate(p=testp11, a=testa11)
maxHyaTest311
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
    'maximumiterations=1000' #default=500
  )
)
maxHyaAdv11 #view output as html
response(maxHyaAdv11) # show response curves for each layer
rHyaAdv11 <- predict(maxHyaAdv11, predictors11) # create model
plot(rHyaAdv11) # plot predictive model
points(hyacinthoides) # add points to predictive model
writeRaster(rHyaAdv11, "models/hyacinthoidesAdv2011.grd")

# develop testing and training sets for mobilense
fold <- kfold(mobilense, k=5) #split occurence points into 5 sets
mobTest11 <- mobilense[fold == 1, ] #take 20% (1/5) for testing
mobTrain11 <- mobilense[fold != 1, ] #leave 40% for training
# fit training model for mobilense
maxMobTrain11 <- maxent(predictors11, mobTrain11) #fit maxent model
maxMobTrain11 #view results in html
rMobTrain11 <- predict(maxMobTrain11, predictors11) #predict full model
plot(rMobTrain11) #visualize full model
points(mobilense) #add points to plot
# testing model for mobilense
# extract background points
bg11 <- randomPoints(predictors11, 1000)
# cross-validate model for mobilense
maxMobTest11 <- evaluate(maxMobTrain11, p=mobTest11, a=bg11, x=predictors11)
maxMobTest11 #print results
threshold(maxMobTest11) #identify threshold for presence or absence
plot(maxMobTest11, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for mobilense
pvtest11 <- data.frame(extract(predictors11, mobTest11))
avtest11 <- data.frame(extract(predictors11, bg11))
# cross-validate model
maxMobTest211 <- evaluate(maxMobTrain11, p=pvtest11, a=avtest11)
maxMobTest211
# Alternative 2: predict to testing points for mobilense
testp11 <- predict(maxMobTrain11, pvtest11)
testa11 <- predict(maxMobTrain11, avtest11)
maxMobTest311 <- evaluate(p=testp11, a=testa11)
maxMobTest311
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
    'maximumiterations=1000' #default=500
  )
)
maxMobAdv11 #view output as html
response(maxMobAdv11) # show response curves for each layer
rMobAdv11 <- predict(maxMobAdv11, predictors11) # create model
plot(rMobAdv11) # plot predictive model
points(mobilense) # add points to predictive model
writeRaster(rMobAdv11, "models/mobilenseAdv2011.grd")

# develop testing and training sets for parentals
fold <- kfold(parentals, k=5) #split occurence points into 5 sets
parTest11 <- parentals[fold == 1, ] #take 20% (1/5) for testing
parTrain11 <- parentals[fold != 1, ] #leave 40% for training
# fit training model for parentals
maxParTrain11 <- maxent(predictors11, parTrain11) #fit maxent model
maxParTrain11 #view results in html
rParTrain11 <- predict(maxParTrain11, predictors11) #predict full model
plot(rParTrain11) #visualize full model
points(parentals) #add points to plot
# testing model for parentals
# extract background points
bg11 <- randomPoints(predictors11, 1000)
# cross-validate model for parentals
maxParTest11 <- evaluate(maxParTrain11, p=parTest11, a=bg11, x=predictors11)
maxParTest11 #print results
threshold(maxParTest11) #identify threshold for presence or absence
plot(maxParTest11, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for parentals
pvtest11 <- data.frame(extract(predictors11, parTest11))
avtest11 <- data.frame(extract(predictors11, bg11))
# cross-validate model
maxParTest211 <- evaluate(maxParTrain11, p=pvtest11, a=avtest11)
maxParTest211
# Alternative 2: predict to testing points for parentals
testp11 <- predict(maxParTrain11, pvtest11)
testa11 <- predict(maxParTrain11, avtest11)
maxParTest311 <- evaluate(p=testp11, a=testa11)
maxParTest311
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
    'maximumiterations=1000' #default=500
  )
)
maxParAdv11 #view output as html
response(maxParAdv11) # show response curves for each layer
rParAdv11 <- predict(maxParAdv11, predictors11) # create model
plot(rParAdv11) # plot predictive model
points(parentals) # add points to predictive model
writeRaster(rParAdv11, "models/parentalsAdv2011.grd")

# develop testing and training sets for hybrids
fold <- kfold(hybrids, k=5) #split occurence points into 5 sets
hybTest11 <- hybrids[fold == 1, ] #take 20% (1/5) for testing
hybTrain11 <- hybrids[fold != 1, ] #leave 40% for training
# fit training model for hybrids
maxHybTrain11 <- maxent(predictors11, hybTrain11) #fit maxent model
maxHybTrain11 #view results in html
rHybTrain11 <- predict(maxHybTrain11, predictors11) #predict full model
plot(rHybTrain11) #visualize full model
points(hybrids) #add points to plot
# testing model for hybrids
# extract background points
bg11 <- randomPoints(predictors11, 1000)
# cross-validate model for hybrids
maxHybTest11 <- evaluate(maxHybTrain11, p=hybTest11, a=bg11, x=predictors11)
maxHybTest11 #print results
threshold(maxHybTest11) #identify threshold for presence or absence
plot(maxHybTest11, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for hybrids
pvtest11 <- data.frame(extract(predictors11, hybTest11))
avtest11 <- data.frame(extract(predictors11, bg11))
# cross-validate model
maxHybTest211 <- evaluate(maxHybTrain11, p=pvtest11, a=avtest11)
maxHybTest211
# Alternative 2: predict to testing points for hybrids
testp11 <- predict(maxHybTrain11, pvtest11)
testa11 <- predict(maxHybTrain11, avtest11)
maxHybTest311 <- evaluate(p=testp11, a=testa11)
maxHybTest311
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
    'maximumiterations=1000' #default=500
  )
)
maxHybAdv11 #view output as html
response(maxHybAdv11) # show response curves for each layer
rHybAdv11 <- predict(maxHybAdv11, predictors11) # create model
plot(rHybAdv11) # plot predictive model
points(hybrids) # add points to predictive model
writeRaster(rHybAdv11, "models/hybridsAdv2011.grd")

# develop testing and training sets for combined
fold <- kfold(combined, k=5) #split occurence points into 5 sets
comTest11 <- combined[fold == 1, ] #take 20% (1/5) for testing
comTrain11 <- combined[fold != 1, ] #leave 40% for training
# fit training model for combined
maxComTrain11 <- maxent(predictors11, comTrain11) #fit maxent model
maxComTrain11 #view results in html
rComTrain11 <- predict(maxComTrain11, predictors11) #predict full model
plot(rComTrain11) #visualize full model
points(combined) #add points to plot
# testing model for combined
# extract background points
bg11 <- randomPoints(predictors11, 1000)
# cross-validate model for combined
maxComTest11 <- evaluate(maxComTrain11, p=comTest11, a=bg11, x=predictors11)
maxComTest11 #print results
threshold(maxComTest11) #identify threshold for presence or absence
plot(maxComTest11, 'ROC') #plot AUC
# alternative methods for testing models (should give same answers)
# Alternative 1: another way to test model for combined
pvtest11 <- data.frame(extract(predictors11, comTest11))
avtest11 <- data.frame(extract(predictors11, bg11))
# cross-validate model
maxComTest211 <- evaluate(maxComTrain11, p=pvtest11, a=avtest11)
maxComTest211
# Alternative 2: predict to testing points for combined
testp11 <- predict(maxComTrain11, pvtest11)
testa11 <- predict(maxComTrain11, avtest11)
maxComTest311 <- evaluate(p=testp11, a=testa11)
maxComTest311
# maxent with jackknife, random seed, and response curves, followed by cross-validation
maxComAdv11 <- maxent(
  x=predictors11,
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
maxComAdv11 #view output as html
response(maxComAdv11) # show response curves for each layer
rComAdv11 <- predict(maxComAdv11, predictors11) # create model
plot(rComAdv11) # plot predictive model
points(combined) # add points to predictive model
writeRaster(rComAdv11, "models/combinedAdv2011.grd")