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

## create stack of non-correlated layers (as determined by layerPrep.R)
predictors0<- stack(ppt0)
predictors1<- stack(ppt1,tmax1)

# extract layer data for each point and add label
parPts0 <- raster::extract(predictors0, parentals)
parPts0 <- cbind.data.frame(species="parentals", parPts0)
parPts0<-na.omit(parPts0)#removing NA values
parPts1 <- raster::extract(predictors1, parentals)
parPts1 <- cbind.data.frame(species="parentals", parPts1)
parPts1<-na.omit(parPts1)#removing NA values
hybPts0 <- raster::extract(predictors0, hybrids)
hybPts0 <- cbind.data.frame(species="hybrids", hybPts0) #add column for hybrids
hybPts1 <- raster::extract(predictors1, hybrids)
hybPts1 <- cbind.data.frame(species="hybrids", hybPts1) #add column for hybrids

# combine parentals and hybrids
bothPts0 <- as.data.frame(rbind(parPts0, hybPts0))
bothPts1 <- as.data.frame(rbind(parPts1, hybPts1))

# one-way ANOVA with Tukey's post-hoc (example from altitude) for 1930
aov.alt0 <- aov(alt ~ species, data=bothPts0)
summary(aov.alt0)
TukeyHSD(aov.alt0)

# one-way ANOVA with Tukey's post-hoc (example from altitude) for 2014
aov.alt1 <- aov(alt ~ species, data=bothPts1)
summary(aov.alt1)
TukeyHSD(aov.alt1)

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

# assessing niche overlap by comparing parentals and hybrids in 2014
nicheOverlap(rPar1, rHyb1, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar1, rHyb1, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

# assessing niche equivalency
#nicheEquivalency()