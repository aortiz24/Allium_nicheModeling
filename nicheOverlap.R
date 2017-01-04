##### evaluating niche overlap between taxa

library(dplyr)
library(raster)
library(dismo)
library(ecospat)
library(ENMeval)

#### multi-variate climate space comparisons (standard statistical testing, non-model based)
### import occurrence data and convert to format required by maxent
##using file made from textbook source
#importing species csv files into R
alliumcanadense<-read.csv("alliumdataset_map_data.csv")

#remove missing data in alliumcanadense
alliumcanadense <- na.omit(alliumcanadense)

#assign scientific name to an object
target1<-c("Allium canadense var. lavendulare")

#filtered allium canadense lavendulare csv file
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

# extract layer data for each point and add label
parPts <- raster::extract(predictors, parentals)
parPts <- cbind.data.frame(species="parentals", parPts)
parPts<-na.omit(parPts)#removing NA values
parPts0 <- raster::extract(predictors0, parentals)
parPts0 <- cbind.data.frame(species="parentals", parPts0)
parPts0<-na.omit(parPts0)#removing NA values
parPts1 <- raster::extract(predictors1, parentals)
parPts1 <- cbind.data.frame(species="parentals", parPts1)
parPts1<-na.omit(parPts1)#removing NA values
hybPts <- raster::extract(predictors, hybrids)
hybPts <- cbind.data.frame(species="hybrids", hybPts) #add column for hybrids
hybPts0 <- raster::extract(predictors0, hybrids)
hybPts0 <- cbind.data.frame(species="hybrids", hybPts0) #add column for hybrids
hybPts1 <- raster::extract(predictors1, hybrids)
hybPts1 <- cbind.data.frame(species="hybrids", hybPts1) #add column for hybrids

# combine parentals and hybrids
bothPts <- as.data.frame(rbind(parPts, hybPts))
bothPts0 <- as.data.frame(rbind(parPts0, hybPts0))
bothPts1 <- as.data.frame(rbind(parPts1, hybPts1))

# one-way ANOVA with Tukey's post-hoc (example from altitude)
aov.alt <- aov(alt ~ species, data=bothPts)
summary(aov.alt)
TukeyHSD(aov.alt)

###Using Bioclim weather data
##for loop of one-way ANOVA with Tukey's post-hoc(for all 11 uncorrelated weather variables)
bothPts <- as.data.frame(rbind(parPts, hybPts))#save dataset(made previously in script)as object for ANOVA analysis
bothPts #view dataset layout
1:ncol(bothPts) #displays how many columns are in dataset
AVz<- rep(NA,ncol(bothPts)) #creates a table called AVz with the same number of columns as the dataset. When it is created each cell will have an NA, then we will add data from the for loop in this table.
sink("anova_results/ANOVA-Tukey.txt")#creates a text file called ANOVA-Tukey.txt in your anova_results directory
for (i in 2:ncol(bothPts)) {
  column <-names(bothPts[i])
  AVz<-summary(aov(bothPts[,i]~species, data=bothPts))
  tk<-TukeyHSD((aov(bothPts[,i]~species, data=bothPts)))
  print(column)
  print(AVz)
  print(tk)
}
sink()

# principle component analysis(PCA)
bothNum <- bothPts[ ,-1] #remove species names
pca_both <- prcomp(bothNum, center = TRUE, scale. = TRUE) #PCA
print(pca_both) #print deviations and rotations
summary(pca_both) #print importance of components
plot(pca_both, type="l") #plot variances
ncomp <- 8 #specify number of components to load (representing 99% of variation)

## model-based approaches
# read in default maxent models
rPar <- raster("models/parentals.grd")
rHyb <- raster("models/hybrids.grd")
# assessing niche overlap by comparing parentals and hybrids using BioClim layers
nicheOverlap(rPar, rHyb, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar, rHyb, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

###Using PRISM 1930 weather data
##for loop of one-way ANOVA with Tukey's post-hoc(for all uncorrelated weather variables)
bothPts0 <- as.data.frame(rbind(parPts0, hybPts0))#save dataset(made previously in script)as object for ANOVA analysis
bothPts0 #view dataset layout
1:ncol(bothPts0) #displays how many columns are in dataset
AVz0<- rep(NA,ncol(bothPts0)) #creates a table called AVz with the same number of columns as the dataset. When it is created each cell will have an NA, then we will add data from the for loop in this table.
sink("anova_results/ANOVA-Tukey0.txt")#creates a text file called ANOVA-Tukey.txt in your anova_results directory
for (i in 2:ncol(bothPts0)) {
  column0 <-names(bothPts0[i])
  AVz0<-summary(aov(bothPts0[,i]~species, data=bothPts0))
  tk0<-TukeyHSD((aov(bothPts0[,i]~species, data=bothPts0)))
  print(column0)
  print(AVz0)
  print(tk0)
}
sink()

# principle component analysis(PCA)
bothNum0 <- bothPts0[ ,-1] #remove species names
pca_both0 <- prcomp(bothNum0, center = TRUE, scale. = TRUE) #PCA=Error because only has one weather variable
print(pca_both0) #print deviations and rotations=Error because only has one weather variable
summary(pca_both0) #print importance of components=Error because only has one weather variable
plot(pca_both0, type="l") #plot variances=Error because only has one weather variable
ncomp <- 1#specify number of components to load (representing 99% of variation)=Error because only has one weather variable

## model-based approaches
# read in default maxent models
rPar0 <- raster("models/parentals1930.grd")
rHyb0 <- raster("models/hybrids1930.grd")
# assessing niche overlap by comparing parentals and hybrids in 1930
nicheOverlap(rPar0, rHyb0, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar0, rHyb0, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

###Using PRISM 2014 weather data
##for loop of one-way ANOVA with Tukey's post-hoc(for all uncorrelated weather variables)
bothPts1 <- as.data.frame(rbind(parPts1, hybPts1))#save dataset(made previously in script)as object for ANOVA analysis
bothPts1 #view dataset layout
1:ncol(bothPts1) #displays how many columns are in dataset
AVz1<- rep(NA,ncol(bothPts1)) #creates a table called AVz with the same number of columns as the dataset. When it is created each cell will have an NA, then we will add data from the for loop in this table.
sink("anova_results/ANOVA-Tukey1.txt")#creates a text file called ANOVA-Tukey.txt in your anova_results directory
for (i in 2:ncol(bothPts1)) {
  column1 <-names(bothPts1[i])
  AVz1<-summary(aov(bothPts1[,i]~species, data=bothPts1))
  tk1<-TukeyHSD((aov(bothPts1[,i]~species, data=bothPts1)))
  print(column1)
  print(AVz1)
  print(tk1)
}
sink()

# principle component analysis(PCA)
bothNum1 <- bothPts1[ ,-1] #remove species names
pca_both1 <- prcomp(bothNum1, center = TRUE, scale. = TRUE) #PCA
print(pca_both1) #print deviations and rotations
summary(pca_both1) #print importance of components
plot(pca_both1, type="l") #plot variances
ncomp <- 2 #specify number of components to load (representing 99% of variation)

## model-based approaches
# read in default maxent models
rPar1 <- raster("models/parentals2014.grd")
rHyb1 <- raster("models/hybrids2014.grd")
# assessing niche overlap by comparing parentals and hybrids in 2014
nicheOverlap(rPar1, rHyb1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar1, rHyb1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in canadense niche from 1930 to 2014
nicheOverlap(rCan0, rCan1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rCan0, rCan1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in lavendulare niche from 1930 to 2014
nicheOverlap(rLav0, rLav1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rLav0, rLav1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in ecristatum niche from 1930 to 2014
nicheOverlap(rEcr0, rEcr1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rEcr0, rEcr1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in fraseri niche from 1930 to 2014
nicheOverlap(rFra0, rFra1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rFra0, rFra1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in hyacinthoides niche from 1930 to 2014
nicheOverlap(rHya0, rHya1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rHya0, rHya1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in mobilense niche from 1930 to 2014
nicheOverlap(rMob0, rMob1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rMob0, rMob1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in parentals niche from 1930 to 2014
nicheOverlap(rPar0, rPar1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar0, rPar1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in hybrids niche from 1930 to 2014
nicheOverlap(rHyb0, rHyb1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rHyb0, rHyb1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in canadense niche from BioClim to 2014
nicheOverlap(rCan, rCan1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rCan, rCan1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in lavendulare niche from BioClim to 2014
nicheOverlap(rLav, rLav1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rLav, rLav1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in ecristatum niche from BioClim to 2014
nicheOverlap(rEcr, rEcr1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rEcr, rEcr1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in fraseri niche from BioClim to 2014
nicheOverlap(rFra, rFra1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rFra, rFra1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in hyacinthoides niche from BioClim to 2014
nicheOverlap(rHya, rHya1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rHya, rHya1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in mobilense niche from BioClim to 2014
nicheOverlap(rMob, rMob1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rMob, rMob1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in parentals niche from BioClim to 2014
nicheOverlap(rPar, rPar1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar, rPar1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in hybrids niche from BioClim to 2014
nicheOverlap(rHyb, rHyb1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rHyb, rHyb1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

# assessing niche equivalency
#nicheEquivalency()