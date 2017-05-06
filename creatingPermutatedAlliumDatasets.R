###Creating permuted Allium Datasets

library(dplyr)
library(raster)
library(dismo)
library(ENMeval)

# create directory for saving the permutation results later
dir.create("permutation_results")

##using file made from textbook source
#importing species csv file into R
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

##For loop for canadense - 1929 vs 2011
#one dataset will run 100 times with 1929 layers in maxent, and an I statistic will be calculated for each run
#the other dataset will run 100 times with 2011 layers in maxent, and an I statistic will be calculated for each run
#The critical value (the fifth lowest I statistic out of 100) will be used to conclude whether the niches are significantly different for 1929 and 2011
sink("permutation_results/canadense_permut_vals.csv")#creates a text file called canadense_permut_vals.csv in your permutation_results directory
for (i in 1:100){
  #making two objects for canadense that are permuted datasets:
  #x.permuted.object contains half of the canadense occurrence points and will be run in maxent with 1929 layers in for loop
  #x.permuted.object2 contains half of the canadense occurrence points and will be run in maxent with 2011 layers in for loop
  #assign 5 occurrence points from the canadense object to the x.permuted object and do not replace the values
  x.permuted<-sample(1:nrow(canadense), size = 5, replace = FALSE)
  #contains the row names of the canadense object in numerical order. The information in these rows will be put into x.permuted.
  x.permuted <- x.permuted[order(x.permuted)]
  #put the remaining row names of the canadense object into x.permuted2.
  x.permuted2 <- setdiff(1:nrow(canadense), x.permuted)
  #contains the row names of the canadense object in numerical order. The information in these rows will be put into x.permuted2.
  x.permuted2 <- x.permuted2[order(x.permuted2)]
  #import specific rows of canadense locality data into x.permuted.object and x.permuted.object2
  #creates paired datasets
  x.permuted.canadense1 <- canadense[(x.permuted),]
  x.permuted.canadense2 <- canadense[(x.permuted2),]
  
  #1929-runing maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxCanAdv9 <- maxent(
    x=predictors9,
    p=x.permuted.canadense1,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #1929-creating model
  rPermutedMaxCanAdv9 <- predict(permutedMaxCanAdv9, predictors9)
  
  #2011-maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxCanAdv11 <- maxent(
    x=predictors11,
    p=x.permuted.canadense2,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #2011-creating model
  rPermutedMaxCanAdv11 <- predict(permutedMaxCanAdv11, predictors11)
  
  #calculate nicheOverlap I statistic
  CanAdvIstat<-nicheOverlap(rPermutedMaxCanAdv9, rPermutedMaxCanAdv11, stat='I', mask=TRUE, checkNegatives=TRUE)
  
  #print the 100 I statistics in the first column in canadense_permut_vals.csv
  print(CanAdvIstat)
  }
sink()

#The critical value (the fifth lowest I statistic out of 100,you only got a value lower than this 5% of the time, P<0.05) will be used to conclude whether the niches are significantly different for 1929 and 2011
CanPermutIstats <- read.csv("permutation_results/canadense_permut_vals.csv")
#assign first 100 rows to the R object
CanPermutIstats <- CanPermutIstats[1:100,]
#ordering permuted 100 I statistic values from least to greatest
a<-sort(CanPermutIstats, decreasing = FALSE)
#writes the 100 I statistic values from least to greatest
write.csv(a, file="permutation_results/canadense_OrderedPermutIstats.csv")
#the critical value is the fifth lowest I statistic out of 100,
#you only get a value lower than this 5% of the time, P<0.05)
#When comparing the canadense niches in 1929 & 2011, the critical value is 0.8708627
a[5]

##For loop for lavendulare - 1929 vs 2011
#one dataset will run 100 times with 1929 layers in maxent, and an I statistic will be calculated for each run
#the other dataset will run 100 times with 2011 layers in maxent, and an I statistic will be calculated for each run
#The critical value (the fifth lowest I statistic out of 100) will be used to conclude whether the niches are significantly different for 1929 and 2011
sink("permutation_results/lavendulare_permut_vals.csv")#creates a text file called lavendulare_permut_vals.csv in your permutation_results directory
for (i in 1:100){
  #making two objects for lavendulare that are permuted datasets:
  #x.permuted.object contains half of the lavendulare occurrence points and will be run in maxent with 1929 layers in for loop
  #x.permuted.object2 contains half of the lavendulare occurrence points and will be run in maxent with 2011 layers in for loop
  #assign 3 occurrence points from the lavendulare object to the x.permuted object and do not replace the values
  x.permuted<-sample(1:nrow(lavendulare), size = 3, replace = FALSE)
  #contains the row names of the lavendulare object in numerical order. The information in these rows will be put into x.permuted.
  x.permuted <- x.permuted[order(x.permuted)]
  #put the remaining row names of the lavendulare object into x.permuted2.
  x.permuted2 <- setdiff(1:nrow(lavendulare), x.permuted)
  #contains the row names of the lavendulare object in numerical order. The information in these rows will be put into x.permuted2.
  x.permuted2 <- x.permuted2[order(x.permuted2)]
  #import specific rows of lavendulare locality data into x.permuted.object and x.permuted.object2
  #creates paired datasets
  x.permuted.lavendulare1 <- lavendulare[(x.permuted),]
  x.permuted.lavendulare2 <- lavendulare[(x.permuted2),]
  
  #1929-runing maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxLavAdv9 <- maxent(
    x=predictors9,
    p=x.permuted.lavendulare1,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #1929-creating model
  rPermutedMaxLavAdv9 <- predict(permutedMaxLavAdv9, predictors9)
  
  #2011-maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxLavAdv11 <- maxent(
    x=predictors11,
    p=x.permuted.lavendulare2,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #2011-creating model
  rPermutedMaxLavAdv11 <- predict(permutedMaxLavAdv11, predictors11)
  
  #calculate nicheOverlap I statistic
  LavAdvIstat<-nicheOverlap(rPermutedMaxLavAdv9, rPermutedMaxLavAdv11, stat='I', mask=TRUE, checkNegatives=TRUE)
  
  #print the 100 I statistics in the first column in lavendulare_permut_vals.csv
  print(LavAdvIstat)
}
sink()

#The critical value (the fifth lowest I statistic out of 100,you only got a value lower than this 5% of the time, P<0.05) will be used to conclude whether the niches are significantly different for 1929 and 2011
LavPermutIstats <- read.csv("permutation_results/lavendulare_permut_vals.csv")
#assign first 100 rows to the R object
LavPermutIstats <- LavPermutIstats[1:100,]
#ordering permuted 100 I statistic values from least to greatest
b<-sort(LavPermutIstats, decreasing = FALSE)
#writes the 100 I statistic values from least to greatest
write.csv(b, file="permutation_results/lavendulare_OrderedPermutIstats.csv")
#the critical value is the fifth lowest I statistic out of 100,
#you only get a value lower than this 5% of the time, P<0.05)
#When comparing the lavendulare niches in 1929 & 2011, the critical value is 0.7734544
b[5]

###CAUTION: nicheOverlap equals 1 when running for loop because ecristatum only has one occurrence point
##For loop for ecristatum - 1929 vs 2011
#one dataset will run 100 times with 1929 layers in maxent, and an I statistic will be calculated for each run
#the other dataset will run 100 times with 2011 layers in maxent, and an I statistic will be calculated for each run
#The critical value (the fifth lowest I statistic out of 100) will be used to conclude whether the niches are significantly different for 1929 and 2011
sink("permutation_results/ecristatum_permut_vals.csv")#creates a text file called ecristatum_permut_vals.csv in your permutation_results directory
for (i in 1:100){
  #making two objects for ecristatum that are permuted datasets:
  #x.permuted.object contains half of the ecristatum occurrence points and will be run in maxent with 1929 layers in for loop
  #x.permuted.object2 contains half of the ecristatum occurrence points and will be run in maxent with 2011 layers in for loop
  #assign 1 occurrence point from the ecristatum object to the x.permuted object and do not replace the values
  x.permuted<-sample(1:nrow(ecristatum), size = 1, replace = FALSE)
  #contains 1 occurrence point from the ecristatum object in numerical order. The information in these rows will be put into x.permuted.
  x.permuted <- x.permuted[order(x.permuted)]
  #put the remaining row names of the ecristatum object into x.permuted2.
  x.permuted2 <- sample(1:nrow(ecristatum), size = 1, replace = FALSE)
  #contains the row names of the ecristatum object in numerical order. The information in these rows will be put into x.permuted2.
  x.permuted2 <- x.permuted2[order(x.permuted2)]
  #import specific rows of ecristatum locality data into x.permuted.object and x.permuted.object2
  #creates paired datasets
  x.permuted.ecristatum1 <- ecristatum[(x.permuted),]
  x.permuted.ecristatum2 <- ecristatum[(x.permuted2),]
  
  #1929-runing maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxEcrAdv9 <- maxent(
    x=predictors9,
    p=x.permuted.ecristatum1,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #1929-creating model
  rPermutedMaxEcrAdv9 <- predict(permutedMaxEcrAdv9, predictors9)
  
  #2011-maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxEcrAdv11 <- maxent(
    x=predictors11,
    p=x.permuted.ecristatum2,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #2011-creating model
  rPermutedMaxEcrAdv11 <- predict(permutedMaxEcrAdv11, predictors11)
  
  #calculate nicheOverlap I statistic
  EcrAdvIstat<-nicheOverlap(rPermutedMaxEcrAdv9, rPermutedMaxEcrAdv11, stat='I', mask=TRUE, checkNegatives=TRUE)
  
  #print the 100 I statistics in the first column in ecristatum_permut_vals.csv
  print(EcrAdvIstat)
}
sink()

#The critical value (the fifth lowest I statistic out of 100,you only got a value lower than this 5% of the time, P<0.05) will be used to conclude whether the niches are significantly different for 1929 and 2011
EcrPermutIstats <- read.csv("permutation_results/ecristatum_permut_vals.csv")
#assign first 100 rows to the R object
EcrPermutIstats <- EcrPermutIstats[1:100,]
#ordering permuted 100 I statistic values from least to greatest
c<-sort(EcrPermutIstats, decreasing = FALSE)
#writes the 100 I statistic values from least to greatest
write.csv(c, file="permutation_results/ecristatum_OrderedPermutIstats.csv")
#the critical value is the fifth lowest I statistic out of 100,
#you only get a value lower than this 5% of the time, P<0.05)
#When comparing the ecristatum niches in 1929 & 2011, the critical value is 1
c[5]

##For loop for fraseri - 1929 vs 2011
#one dataset will run 100 times with 1929 layers in maxent, and an I statistic will be calculated for each run
#the other dataset will run 100 times with 2011 layers in maxent, and an I statistic will be calculated for each run
#The critical value (the fifth lowest I statistic out of 100) will be used to conclude whether the niches are significantly different for 1929 and 2011
sink("permutation_results/fraseri_permut_vals.csv")#creates a text file called fraseri_permut_vals.csv in your permutation_results directory
for (i in 1:100){
  #making two objects for fraseri that are permuted datasets:
  #x.permuted.object contains half of the fraseri occurrence points and will be run in maxent with 1929 layers in for loop
  #x.permuted.object2 contains half of the fraseri occurrence points and will be run in maxent with 2011 layers in for loop
  #assign 5 occurrence points from the fraseri object to the x.permuted object and do not replace the values
  x.permuted<-sample(1:nrow(fraseri), size = 5, replace = FALSE)
  #contains the row names of the fraseri object in numerical order. The information in these rows will be put into x.permuted.
  x.permuted <- x.permuted[order(x.permuted)]
  #put the remaining row names of the fraseri object into x.permuted2.
  x.permuted2 <- setdiff(1:nrow(fraseri), x.permuted)
  #contains the row names of the fraseri object in numerical order. The information in these rows will be put into x.permuted2.
  x.permuted2 <- x.permuted2[order(x.permuted2)]
  #import specific rows of fraseri locality data into x.permuted.object and x.permuted.object2
  #creates paired datasets
  x.permuted.fraseri1 <- fraseri[(x.permuted),]
  x.permuted.fraseri2 <- fraseri[(x.permuted2),]
  
  #1929-runing maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxFraAdv9 <- maxent(
    x=predictors9,
    p=x.permuted.fraseri1,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #1929-creating model
  rPermutedMaxFraAdv9 <- predict(permutedMaxFraAdv9, predictors9)
  
  #2011-maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxFraAdv11 <- maxent(
    x=predictors11,
    p=x.permuted.fraseri2,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #2011-creating model
  rPermutedMaxFraAdv11 <- predict(permutedMaxFraAdv11, predictors11)
  
  #calculate nicheOverlap I statistic
  FraAdvIstat<-nicheOverlap(rPermutedMaxFraAdv9, rPermutedMaxFraAdv11, stat='I', mask=TRUE, checkNegatives=TRUE)
  
  #print the 100 I statistics in the first column in fraseri_permut_vals.csv
  print(FraAdvIstat)
}
sink()

#The critical value (the fifth lowest I statistic out of 100,you only got a value lower than this 5% of the time, P<0.05) will be used to conclude whether the niches are significantly different for 1929 and 2011
FraPermutIstats <- read.csv("permutation_results/fraseri_permut_vals.csv")
#assign first 100 rows to the R object
FraPermutIstats <- FraPermutIstats[1:100,]
#ordering permuted 100 I statistic values from least to greatest
d<-sort(FraPermutIstats, decreasing = FALSE)
#writes the 100 I statistic values from least to greatest
write.csv(d, file="permutation_results/fraseri_OrderedPermutIstats.csv")
#the critical value is the fifth lowest I statistic out of 100,
#you only get a value lower than this 5% of the time, P<0.05)
#When comparing the fraseri niches in 1929 & 2011, the critical value is 0.8969706
d[5]

###CAUTION: nicheOverlap equals 1 when running for loop because hyacinthoides only has two occurrence points
##For loop for hyacinthoides - 1929 vs 2011
#one dataset will run 100 times with 1929 layers in maxent, and an I statistic will be calculated for each run
#the other dataset will run 100 times with 2011 layers in maxent, and an I statistic will be calculated for each run
#The critical value (the fifth lowest I statistic out of 100) will be used to conclude whether the niches are significantly different for 1929 and 2011
sink("permutation_results/hyacinthoides_permut_vals.csv")#creates a text file called hyacinthoides_permut_vals.csv in your permutation_results directory
for (i in 1:100){
  #making two objects for hyacinthoides that are permuted datasets:
  #x.permuted.object contains half of the hyacinthoides occurrence points and will be run in maxent with 1929 layers in for loop
  #x.permuted.object2 contains half of the hyacinthoides occurrence points and will be run in maxent with 2011 layers in for loop
  #assign 1 occurrence point from the hyacinthoides object to the x.permuted object and do not replace the values
  x.permuted<-sample(1:nrow(hyacinthoides), size = 1, replace = FALSE)
  #contains the row names of the hyacinthoides object in numerical order. The information in these rows will be put into x.permuted.
  x.permuted <- x.permuted[order(x.permuted)]
  #put the remaining row names of the hyacinthoides object into x.permuted2.
  x.permuted2 <- setdiff(1:nrow(hyacinthoides), x.permuted)
  #contains the row names of the hyacinthoides object in numerical order. The information in these rows will be put into x.permuted2.
  x.permuted2 <- x.permuted2[order(x.permuted2)]
  #import specific rows of hyacinthoides locality data into x.permuted.object and x.permuted.object2
  #creates paired datasets
  x.permuted.hyacinthoides1 <- hyacinthoides[(x.permuted),]
  x.permuted.hyacinthoides2 <- hyacinthoides[(x.permuted2),]
  
  #1929-runing maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxHyaAdv9 <- maxent(
    x=predictors9,
    p=x.permuted.hyacinthoides1,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #1929-creating model
  rPermutedMaxHyaAdv9 <- predict(permutedMaxHyaAdv9, predictors9)
  
  #2011-maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxHyaAdv11 <- maxent(
    x=predictors11,
    p=x.permuted.hyacinthoides2,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #2011-creating model
  rPermutedMaxHyaAdv11 <- predict(permutedMaxHyaAdv11, predictors11)
  
  #calculate nicheOverlap I statistic
  HyaAdvIstat<-nicheOverlap(rPermutedMaxHyaAdv9, rPermutedMaxHyaAdv11, stat='I', mask=TRUE, checkNegatives=TRUE)
  
  #print the 100 I statistics in the first column in hyacinthoides_permut_vals.csv
  print(HyaAdvIstat)
}
sink()

#The critical value (the fifth lowest I statistic out of 100,you only got a value lower than this 5% of the time, P<0.05) will be used to conclude whether the niches are significantly different for 1929 and 2011
HyaPermutIstats <- read.csv("permutation_results/hyacinthoides_permut_vals.csv")
#assign first 100 rows to the R object
HyaPermutIstats <- HyaPermutIstats[1:100,]
#ordering permuted 100 I statistic values from least to greatest
e<-sort(HyaPermutIstats, decreasing = FALSE)
#writes the 100 I statistic values from least to greatest
write.csv(e, file="permutation_results/hyacinthoides_OrderedPermutIstats.csv")
#the critical value is the fifth lowest I statistic out of 100,
#you only get a value lower than this 5% of the time, P<0.05)
#When comparing the hyacinthoides niches in 1929 & 2011, the critical value is 1
e[5]

##For loop for mobilense - 1929 vs 2011
#one dataset will run 100 times with 1929 layers in maxent, and an I statistic will be calculated for each run
#the other dataset will run 100 times with 2011 layers in maxent, and an I statistic will be calculated for each run
#The critical value (the fifth lowest I statistic out of 100) will be used to conclude whether the niches are significantly different for 1929 and 2011
sink("permutation_results/mobilense_permut_vals.csv")#creates a text file called mobilense_permut_vals.csv in your permutation_results directory
for (i in 1:100){
  #making two objects for mobilense that are permuted datasets:
  #x.permuted.object contains half of the mobilense occurrence points and will be run in maxent with 1929 layers in for loop
  #x.permuted.object2 contains half of the mobilense occurrence points and will be run in maxent with 2011 layers in for loop
  #assign 6 occurrence points from the mobilense object to the x.permuted object and do not replace the values
  x.permuted<-sample(1:nrow(mobilense), size = 6, replace = FALSE)
  #contains the row names of the mobilense object in numerical order. The information in these rows will be put into x.permuted.
  x.permuted <- x.permuted[order(x.permuted)]
  #put the remaining row names of the mobilense object into x.permuted2.
  x.permuted2 <- setdiff(1:nrow(mobilense), x.permuted)
  #contains the row names of the mobilense object in numerical order. The information in these rows will be put into x.permuted2.
  x.permuted2 <- x.permuted2[order(x.permuted2)]
  #import specific rows of mobilense locality data into x.permuted.object and x.permuted.object2
  #creates paired datasets
  x.permuted.mobilense1 <- mobilense[(x.permuted),]
  x.permuted.mobilense2 <- mobilense[(x.permuted2),]
  
  #1929-runing maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxMobAdv9 <- maxent(
    x=predictors9,
    p=x.permuted.mobilense1,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #1929-creating model
  rPermutedMaxMobAdv9 <- predict(permutedMaxMobAdv9, predictors9)
  
  #2011-maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxMobAdv11 <- maxent(
    x=predictors11,
    p=x.permuted.mobilense2,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #2011-creating model
  rPermutedMaxMobAdv11 <- predict(permutedMaxMobAdv11, predictors11)
  
  #calculate nicheOverlap I statistic
  MobAdvIstat<-nicheOverlap(rPermutedMaxMobAdv9, rPermutedMaxMobAdv11, stat='I', mask=TRUE, checkNegatives=TRUE)
  
  #print the 100 I statistics in the first column in mobilense_permut_vals.csv
  print(MobAdvIstat)
}
sink()

#The critical value (the fifth lowest I statistic out of 100,you only got a value lower than this 5% of the time, P<0.05) will be used to conclude whether the niches are significantly different for 1929 and 2011
MobPermutIstats <- read.csv("permutation_results/mobilense_permut_vals.csv")
#assign first 100 rows to the R object
MobPermutIstats <- MobPermutIstats[1:100,]
#ordering permuted 100 I statistic values from least to greatest
f<-sort(MobPermutIstats, decreasing = FALSE)
#writes the 100 I statistic values from least to greatest
write.csv(f, file="permutation_results/mobilense_OrderedPermutIstats.csv")
#the critical value is the fifth lowest I statistic out of 100,
#you only get a value lower than this 5% of the time, P<0.05)
#When comparing the mobilense niches in 1929 & 2011, the critical value is 0.8566591
f[5]

##For loop for parentals - 1929 vs 2011
#one dataset will run 100 times with 1929 layers in maxent, and an I statistic will be calculated for each run
#the other dataset will run 100 times with 2011 layers in maxent, and an I statistic will be calculated for each run
#The critical value (the fifth lowest I statistic out of 100) will be used to conclude whether the niches are significantly different for 1929 and 2011
sink("permutation_results/parentals_permut_vals.csv")#creates a text file called parentals_permut_vals.csv in your permutation_results directory
for (i in 1:100){
  #making two objects for parentals that are permuted datasets:
  #x.permuted.object contains half of the parentals occurrence points and will be run in maxent with 1929 layers in for loop
  #x.permuted.object2 contains half of the parentals occurrence points and will be run in maxent with 2011 layers in for loop
  #assign 20 occurrence points from the parentals object to the x.permuted object and do not replace the values
  x.permuted<-sample(1:nrow(parentals), size = 20, replace = FALSE)
  #contains the row names of the parentals object in numerical order. The information in these rows will be put into x.permuted.
  x.permuted <- x.permuted[order(x.permuted)]
  #put the remaining row names of the parentals object into x.permuted2.
  x.permuted2 <- setdiff(1:nrow(parentals), x.permuted)
  #contains the row names of the parentals object in numerical order. The information in these rows will be put into x.permuted2.
  x.permuted2 <- x.permuted2[order(x.permuted2)]
  #import specific rows of parentals locality data into x.permuted.object and x.permuted.object2
  #creates paired datasets
  x.permuted.parentals1 <- parentals[(x.permuted),]
  x.permuted.parentals2 <- parentals[(x.permuted2),]
  
  #1929-runing maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxParAdv9 <- maxent(
    x=predictors9,
    p=x.permuted.parentals1,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #1929-creating model
  rPermutedMaxParAdv9 <- predict(permutedMaxParAdv9, predictors9)
  
  #2011-maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxParAdv11 <- maxent(
    x=predictors11,
    p=x.permuted.parentals2,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #2011-creating model
  rPermutedMaxParAdv11 <- predict(permutedMaxParAdv11, predictors11)
  
  #calculate nicheOverlap I statistic
  ParAdvIstat<-nicheOverlap(rPermutedMaxParAdv9, rPermutedMaxParAdv11, stat='I', mask=TRUE, checkNegatives=TRUE)
  
  #print the 100 I statistics in the first column in parentals_permut_vals.csv
  print(ParAdvIstat)
}
sink()

#The critical value (the fifth lowest I statistic out of 100,you only got a value lower than this 5% of the time, P<0.05) will be used to conclude whether the niches are significantly different for 1929 and 2011
ParPermutIstats <- read.csv("permutation_results/parentals_permut_vals.csv")
#assign first 100 rows to the R object
ParPermutIstats <- ParPermutIstats[1:100,]
#ordering permuted 100 I statistic values from least to greatest
g<-sort(ParPermutIstats, decreasing = FALSE)
#writes the 100 I statistic values from least to greatest
write.csv(g, file="permutation_results/parentals_OrderedPermutIstats.csv")
#the critical value is the fifth lowest I statistic out of 100,
#you only get a value lower than this 5% of the time, P<0.05)
#When comparing the parentals niches in 1929 & 2011, the critical value is 0.9273717
g[5]

##For loop for hybrids - 1929 vs 2011
#one dataset will run 100 times with 1929 layers in maxent, and an I statistic will be calculated for each run
#the other dataset will run 100 times with 2011 layers in maxent, and an I statistic will be calculated for each run
#The critical value (the fifth lowest I statistic out of 100) will be used to conclude whether the niches are significantly different for 1929 and 2011
sink("permutation_results/hybrids_permut_vals.csv")#creates a text file called hybrids_permut_vals.csv in your permutation_results directory
for (i in 1:100){
  #making two objects for hybrids that are permuted datasets:
  #x.permuted.object contains half of the hybrids occurrence points and will be run in maxent with 1929 layers in for loop
  #x.permuted.object2 contains half of the hybrids occurrence points and will be run in maxent with 2011 layers in for loop
  #assign 20 occurrence points from the hybrids object to the x.permuted object and do not replace the values
  x.permuted<-sample(1:nrow(hybrids), size = 20, replace = FALSE)
  #contains the row names of the hybrids object in numerical order. The information in these rows will be put into x.permuted.
  x.permuted <- x.permuted[order(x.permuted)]
  #put the remaining row names of the hybrids object into x.permuted2.
  x.permuted2 <- setdiff(1:nrow(hybrids), x.permuted)
  #contains the row names of the hybrids object in numerical order. The information in these rows will be put into x.permuted2.
  x.permuted2 <- x.permuted2[order(x.permuted2)]
  #import specific rows of hybrids locality data into x.permuted.object and x.permuted.object2
  #creates paired datasets
  x.permuted.hybrids1 <- hybrids[(x.permuted),]
  x.permuted.hybrids2 <- hybrids[(x.permuted2),]
  
  #1929-runing maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxHybAdv9 <- maxent(
    x=predictors9,
    p=x.permuted.hybrids1,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #1929-creating model
  rPermutedMaxHybAdv9 <- predict(permutedMaxHybAdv9, predictors9)
  
  #2011-maxent with jackknife, random seed, and response curves, followed by cross-validation
  permutedMaxHybAdv11 <- maxent(
    x=predictors11,
    p=x.permuted.hybrids2,
    removeDuplicates=TRUE,
    nbg=10000,
    args=c(
      'randomseed=true', #default=false
      'threads=2', #default=1
      'responsecurves=true', #default=false
      'jackknife=true', #default=false
      'replicatetype=crossvalidate',
      'maximumiterations=1000' #default=500
    )
  )
  #2011-creating model
  rPermutedMaxHybAdv11 <- predict(permutedMaxHybAdv11, predictors11)
  
  #calculate nicheOverlap I statistic
  HybAdvIstat<-nicheOverlap(rPermutedMaxHybAdv9, rPermutedMaxHybAdv11, stat='I', mask=TRUE, checkNegatives=TRUE)
  
  #print the 100 I statistics in the first column in hybrids_permut_vals.csv
  print(HybAdvIstat)
}
sink()

#The critical value (the fifth lowest I statistic out of 100,you only got a value lower than this 5% of the time, P<0.05) will be used to conclude whether the niches are significantly different for 1929 and 2011
HybPermutIstats <- read.csv("permutation_results/hybrids_permut_vals.csv")
#assign first 100 rows to the R object
HybPermutIstats <- HybPermutIstats[1:100,]
#ordering permuted 100 I statistic values from least to greatest
h<-sort(HybPermutIstats, decreasing = FALSE)
#writes the 100 I statistic values from least to greatest
write.csv(h, file="permutation_results/hybrids_OrderedPermutIstats.csv")
#the critical value is the fifth lowest I statistic out of 100,
#you only get a value lower than this 5% of the time, P<0.05)
#When comparing the hybrids niches in 1929 & 2011, the critical value is 0.9447226
h[5]

