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
target11<-c("Allium canadense var. canadense")
#filtered allium canadense canadense csv file
alliumcanadense11<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target11)
#assign variety name to object
canadense <- (alliumcanadense11)

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
lavendulare<-lavendulare[c(11:6),]

#assign scientific name to an object
target6<-c("Allium canadense var. mobilense")
#filtered allium canadense mobilense csv file
alliumcanadense6<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target6)
#assign variety name to object
mobilense <- (alliumcanadense6)

#assign parentals(mobilense,fraseri) to a R object
parentals11<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude)
parentals<-parentals11[c(23:311,411:511),]
#assign hybrids(hyacinthoides,ecristatum,lavendulare) to a R object
hybrids11<- alliumcanadense %>%
  select(Taxon,Latitude,Longitude)
hybrids<-hybrids11[c(22,32,33,35:39),]

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

#layers ending in 9 are for PRISM11929
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

# extract layer data for each point and add label
parPts9 <- raster::extract(predictors9, parentals)
parPts9 <- cbind.data.frame(species="parentals", parPts9)
parPts9<-na.omit(parPts9)#removing NA values
parPts11 <- raster::extract(predictors11, parentals)
parPts11 <- cbind.data.frame(species="parentals", parPts11)
parPts11<-na.omit(parPts11)#removing NA values
hybPts9 <- raster::extract(predictors9, hybrids)
hybPts9 <- cbind.data.frame(species="hybrids", hybPts9) #add column for hybrids
hybPts11 <- raster::extract(predictors11, hybrids)
hybPts11 <- cbind.data.frame(species="hybrids", hybPts11) #add column for hybrids

# combine parentals and hybrids
bothPts9 <- as.data.frame(rbind(parPts9, hybPts9))
bothPts11 <- as.data.frame(rbind(parPts11, hybPts11))

# one-way ANOVA with Tukey's post-hoc (example from altitude) for 1929
aov.alt9 <- aov(alt ~ species, data=bothPts9)
summary(aov.alt9)
TukeyHSD(aov.alt9)

# one-way ANOVA with Tukey's post-hoc (example from altitude) for 2011
aov.alt11 <- aov(alt ~ species, data=bothPts11)
summary(aov.alt11)
TukeyHSD(aov.alt11)

###Using PRISM 1929 weather data
##for loop of one-way ANOVA with Tukey's post-hoc(for all uncorrelated weather variables)
bothPts9 <- as.data.frame(rbind(parPts9, hybPts9))#save dataset(made previously in script)as object for ANOVA analysis
bothPts9 #view dataset layout
11:ncol(bothPts9) #displays how many columns are in dataset
AVz9<- rep(NA,ncol(bothPts9)) #creates a table called AVz with the same number of columns as the dataset. When it is created each cell will have an NA, then we will add data from the for loop in this table.
sink("anova_results/ANOVA-Tukey9.txt")#creates a text file called ANOVA-Tukey.txt in your anova_results directory
for (i in 2:ncol(bothPts9)) {
  column9 <-names(bothPts9[i])
  AVz9<-summary(aov(bothPts9[,i]~species, data=bothPts9))
  tk9<-TukeyHSD((aov(bothPts9[,i]~species, data=bothPts9)))
  print(column9)
  print(AVz9)
  print(tk9)
}
sink()

# principle component analysis(PCA)
bothNum9 <- bothPts9[ ,-1] #remove species names
pca_both9 <- prcomp(bothNum9, center = TRUE, scale. = TRUE) #PCA=Error because only has one weather variable
print(pca_both9) #print deviations and rotations=Error because only has one weather variable
summary(pca_both9) #print importance of components=Error because only has one weather variable
plot(pca_both9, type="l") #plot variances=Error because only has one weather variable
ncomp <- 1#specify number of components to load (representing 99% of variation)=Error because only has one weather variable

## model-based approaches
# read in default maxent models
rPar9 <- raster("models/parentals1929.grd")
rHyb9 <- raster("models/hybrids1929.grd")
# assessing niche overlap by comparing parentals and hybrids in 1929
nicheOverlap(rPar9, rHyb9, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar9, rHyb9, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

###Using PRISM 2011 weather data
##for loop of one-way ANOVA with Tukey's post-hoc(for all uncorrelated weather variables)
bothPts11 <- as.data.frame(rbind(parPts11, hybPts11))#save dataset(made previously in script)as object for ANOVA analysis
bothPts11 #view dataset layout
11:ncol(bothPts11) #displays how many columns are in dataset
AVz11<- rep(NA,ncol(bothPts11)) #creates a table called AVz with the same number of columns as the dataset. When it is created each cell will have an NA, then we will add data from the for loop in this table.
sink("anova_results/ANOVA-Tukey11.txt")#creates a text file called ANOVA-Tukey.txt in your anova_results directory
for (i in 2:ncol(bothPts11)) {
  column11 <-names(bothPts11[i])
  AVz11<-summary(aov(bothPts11[,i]~species, data=bothPts11))
  tk11<-TukeyHSD((aov(bothPts11[,i]~species, data=bothPts11)))
  print(column11)
  print(AVz11)
  print(tk11)
}
sink()

# principle component analysis(PCA)
bothNum11 <- bothPts11[ ,-1] #remove species names
pca_both11 <- prcomp(bothNum11, center = TRUE, scale. = TRUE) #PCA
print(pca_both11) #print deviations and rotations
summary(pca_both11) #print importance of components
plot(pca_both11, type="l") #plot variances
ncomp <- 2 #specify number of components to load (representing 99% of variation)

## model-based approaches
# read in default maxent models
rPar11 <- raster("models/parentals2011.grd")
rHyb11 <- raster("models/hybrids2011.grd")

#assessing changes in canadense niche from 1929 to 2011
nicheOverlap(rCan9, rCan11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rCan9, rCan11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in lavendulare niche from 1929 to 2011
nicheOverlap(rLav9, rLav11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rLav9, rLav11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in ecristatum niche from 1929 to 2011
nicheOverlap(rEcr9, rEcr11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rEcr9, rEcr11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in fraseri niche from 1929 to 2011
nicheOverlap(rFra9, rFra11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rFra9, rFra11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in hyacinthoides niche from 1929 to 2011
nicheOverlap(rHya9, rHya11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rHya9, rHya11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in mobilense niche from 1929 to 2011
nicheOverlap(rMob9, rMob11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rMob9, rMob11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in parentals niche from 1929 to 2011
nicheOverlap(rPar9, rPar11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar9, rPar11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in hybrids niche from 1929 to 2011
nicheOverlap(rHyb9, rHyb11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rHyb9, rHyb11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

# assessing niche overlap by comparing parentals and hybrids in 2011
nicheOverlap(rPar11, rHyb11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar11, rHyb11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

# assessing niche equivalency
#nicheEquivalency()