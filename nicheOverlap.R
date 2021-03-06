##### evaluating niche overlap between taxa
# load libraries
library(dplyr)
library(raster)
library(dismo)
library(ecospat)
library(ENMeval)
library(devtools)
library(ggbiplot)

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
  dplyr::select(Taxon,Latitude,Longitude) %>%
  filter(Taxon == target11)
#assign variety name to object
canadense <- (alliumcanadense11)
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
canPts9 <- raster::extract(predictors9, canadense)
canPts9 <- cbind.data.frame(species="canadense", canPts9) #add column for canadense
canPts11 <- raster::extract(predictors11, canadense)
canPts11 <- cbind.data.frame(species="canadense", canPts11) #add column for canadense
lavPts9 <- raster::extract(predictors9, lavendulare)
lavPts9 <- cbind.data.frame(species="lavendulare", lavPts9) #add column for lavendulare
lavPts11 <- raster::extract(predictors11, lavendulare)
lavPts11 <- cbind.data.frame(species="lavendulare", lavPts11) #add column for lavendulare
ecrPts9 <- raster::extract(predictors9, ecristatum)
ecrPts9 <- cbind.data.frame(species="ecristatum", ecrPts9) #add column for ecristatum
ecrPts11 <- raster::extract(predictors11, ecristatum)
ecrPts11 <- cbind.data.frame(species="ecristatum", ecrPts11) #add column for ecristatum
fraPts9 <- raster::extract(predictors9, fraseri)
fraPts9 <- cbind.data.frame(species="fraseri", fraPts9) #add column for fraseri
fraPts11 <- raster::extract(predictors11, fraseri)
fraPts11 <- cbind.data.frame(species="fraseri", fraPts11) #add column for fraseri
hyaPts9 <- raster::extract(predictors9, hyacinthoides)
hyaPts9 <- cbind.data.frame(species="hyacinthoides", hyaPts9) #add column for hyacinthoides
hyaPts11 <- raster::extract(predictors11, hyacinthoides)
hyaPts11 <- cbind.data.frame(species="hyacinthoides", hyaPts11) #add column for hyacinthoides
mobPts9 <- raster::extract(predictors9, mobilense)
mobPts9 <- cbind.data.frame(species="mobilense", mobPts9) #add column for mobilense
mobPts11 <- raster::extract(predictors11, mobilense)
mobPts11 <- cbind.data.frame(species="mobilense", mobPts11) #add column for mobilense
parPts9 <- raster::extract(predictors9, parentals)
parPts9 <- cbind.data.frame(species="parentals", parPts9)
parPts11 <- raster::extract(predictors11, parentals)
parPts11 <- cbind.data.frame(species="parentals", parPts11)
hybPts9 <- raster::extract(predictors9, hybrids)
hybPts9 <- cbind.data.frame(species="hybrids", hybPts9) #add column for hybrids
hybPts11 <- raster::extract(predictors11, hybrids)
hybPts11 <- cbind.data.frame(species="hybrids", hybPts11) #add column for hybrids

# assigned call varieties data to a different R object
bothPts9 <- as.data.frame(rbind(canPts9, lavPts9, ecrPts9, fraPts9, hyaPts9, mobPts9))
bothPts11 <- as.data.frame(rbind(canPts11, lavPts11, ecrPts11, fraPts11, hyaPts11, mobPts11))

# one-way ANOVA with Tukey's post-hoc (example from mean temperature) for 1929
aov.alt9 <- aov(species ~ tmean9, data=bothPts9)
summary(aov.alt9)
TukeyHSD(aov.alt9)

# one-way ANOVA with Tukey's post-hoc (example from mean temperature) for 2011
aov.alt11 <- aov(species ~ tmean11, data=bothPts11)
summary(aov.alt11)
TukeyHSD(aov.alt11)

###Using PRISM 1929 weather data
##for loop of one-way ANOVA with Tukey's post-hoc(for all uncorrelated weather variables)
bothPts9 <- as.data.frame(rbind(canPts9, lavPts9, ecrPts9, fraPts9, hyaPts9, mobPts9))#save dataset(made previously in script)as object for ANOVA analysis
bothPts9 #view dataset layout
ncol(bothPts9) #displays how many columns are in dataset
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
#open png file to save PCA plot in figures directory 
png(filename="figures/PCA_All1929.png")
varieties9 <- bothPts9[ ,1] #holds names of varieites and will be used in PCA plot
bothNum9 <- bothPts9[ ,-1] #remove species names
pca_both9 <- prcomp(bothNum9, center = TRUE, scale. = TRUE) #PCA
print(pca_both9) #print deviations and rotations
summary(pca_both9) #print importance of components
plot(pca_both9, type="l") #plot variances
ncomp <- 2#specify number of components to load (representing 99% of variation)
#make a PCA plot
ggbiplot(pca_both9, obs.scale =1, var.scale = 1, groups= varieties9, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top', panel.background = element_blank())
dev.off() #close png file

## model-based approaches
# read in default maxent models
rCan9 <- raster("models/canadense1929Maxent/canadense1929.grd")
rLav9 <- raster("models/lavendulare1929Maxent/lavendulare1929.grd")
rEcr9 <- raster("models/ecristatum1929Maxent/ecristatum1929.grd")
rFra9 <- raster("models/fraseri1929Maxent/fraseri1929.grd")
rHya9 <- raster("models/hyacinthoides1929Maxent/hyacinthoides1929.grd")
rMob9 <- raster("models/mobilense1929Maxent/mobilense1929.grd")
rPar9 <- raster("models/parentals1929Maxent/parentals1929.grd")
rHyb9 <- raster("models/hybrids1929Maxent/hybrids1929.grd")

###Using PRISM 2011 weather data
##for loop of one-way ANOVA with Tukey's post-hoc(for all uncorrelated weather variables)
bothPts11 <- as.data.frame(rbind(canPts11, lavPts11, ecrPts11, fraPts11, hyaPts11, mobPts11))#save dataset(made previously in script)as object for ANOVA analysis
bothPts11 #view dataset layout
ncol(bothPts11) #displays how many columns are in dataset
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
#open png file to save PCA plot in figures directory 
png(filename="figures/PCA_All2011.png")
varieties11 <- bothPts11[ ,1] #holds names of varieites and will be used in PCA plot 
bothNum11 <- bothPts11[ ,-1] #remove species names
pca_both11 <- prcomp(bothNum11, center = TRUE, scale. = TRUE) #PCA
print(pca_both11) #print deviations and rotations
summary(pca_both11) #print importance of components
plot(pca_both11, type="l") #plot variances
ncomp <- 3 #specify number of components to load (representing 99% of variation)
#make a PCA plot
ggbiplot(pca_both11, obs.scale =1, var.scale = 1, groups= varieties11, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top', panel.background = element_blank())
dev.off() #close png file

## model-based approaches
# read in default maxent models
rCan11 <- raster("models/canadense2011Maxent/canadense2011.grd")
rLav11 <- raster("models/lavendulare2011Maxent/lavendulare2011.grd")
rEcr11 <- raster("models/ecristatum2011Maxent/ecristatum2011.grd")
rFra11 <- raster("models/fraseri2011Maxent/fraseri2011.grd")
rHya11 <- raster("models/hyacinthoides2011Maxent/hyacinthoides2011.grd")
rMob11 <- raster("models/mobilense2011Maxent/mobilense2011.grd")
rPar11 <- raster("models/parentals2011Maxent/parentals2011.grd")
rHyb11 <- raster("models/hybrids2011Maxent/hybrids2011.grd")

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

# assessing niche overlap by comparing parentals and hybrids in 1929
nicheOverlap(rPar9, rHyb9, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar9, rHyb9, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

# assessing niche overlap by comparing parentals and hybrids in 2011
nicheOverlap(rPar11, rHyb11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rPar11, rHyb11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

## model-based approaches
# read in advanced maxent models
rCanAdv9 <- raster("models/canadense1929Maxent/canadenseAdv1929.grd")
rLavAdv9 <- raster("models/lavendulare1929Maxent/lavendulareAdv1929.grd")
rEcrAdv9 <- raster("models/ecristatum1929Maxent/ecristatumAdv1929.grd")
rFraAdv9 <- raster("models/fraseri1929Maxent/fraseriAdv1929.grd")
rHyaAdv9 <- raster("models/hyacinthoides1929Maxent/hyacinthoidesAdv1929.grd")
rMobAdv9 <- raster("models/mobilense1929Maxent/mobilenseAdv1929.grd")
rParAdv9 <- raster("models/parentals1929Maxent/parentalsAdv1929.grd")
rHybAdv9 <- raster("models/hybrids1929Maxent/hybridsAdv1929.grd")
rCanAdv11 <- raster("models/canadense2011Maxent/canadenseAdv2011.grd")
rLavAdv11 <- raster("models/lavendulare2011Maxent/lavendulareAdv2011.grd")
rEcrAdv11 <- raster("models/ecristatum2011Maxent/ecristatumAdv2011.grd")
rFraAdv11 <- raster("models/fraseri2011Maxent/fraseriAdv2011.grd")
rHyaAdv11 <- raster("models/hyacinthoides2011Maxent/hyacinthoidesAdv2011.grd")
rMobAdv11 <- raster("models/mobilense2011Maxent/mobilenseAdv2011.grd")
rParAdv11 <- raster("models/parentals2011Maxent/parentalsAdv2011.grd")
rHybAdv11 <- raster("models/hybrids2011Maxent/hybridsAdv2011.grd")

#assessing changes in canadense niche from 1929 to 2011
nicheOverlap(rCanAdv9, rCanAdv11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rCanAdv9, rCanAdv11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in lavendulare niche from 1929 to 2011
nicheOverlap(rLavAdv9, rLavAdv11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rLavAdv9, rLavAdv11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in ecristatum niche from 1929 to 2011
nicheOverlap(rEcrAdv9, rEcrAdv11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rEcrAdv9, rEcrAdv11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in fraseri niche from 1929 to 2011
nicheOverlap(rFraAdv9, rFraAdv11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rFraAdv9, rFraAdv11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in hyacinthoides niche from 1929 to 2011
nicheOverlap(rHyaAdv9, rHyaAdv11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rHyaAdv9, rHyaAdv11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in mobilense niche from 1929 to 2011
nicheOverlap(rMobAdv9, rMobAdv11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rMobAdv9, rMobAdv11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

# assessing niche overlap by comparing parentals and hybrids in 1929
nicheOverlap(rParAdv9, rHybAdv9, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rParAdv9, rHybAdv9, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

# assessing niche overlap by comparing parentals and hybrids in 2011
nicheOverlap(rParAdv11, rHybAdv11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rParAdv11, rHybAdv11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in parentals niche from 1929 to 2011
nicheOverlap(rParAdv9, rParAdv11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rParAdv9, rParAdv11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic

#assessing changes in hybrids niche from 1929 to 2011
nicheOverlap(rHybAdv9, rHybAdv11, stat='D', mask=TRUE, checkNegatives=TRUE) # D statistic
nicheOverlap(rHybAdv9, rHybAdv11, stat='I', mask=TRUE, checkNegatives=TRUE) # I statistic