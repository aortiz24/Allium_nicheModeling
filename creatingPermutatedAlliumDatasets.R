###Creating permuted Allium Datasets

library(dplyr)
library(raster)
library(dismo)
library(ecospat)
library(ENMeval)

##using file made from textbook source
#importing species csv file into R
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
parentals11<-alliumcanadense %>%
  select(Taxon,Latitude,Longitude)
parentals<-parentals11[c(23:31,41:55),]
parentals<-na.omit(parentals)

#assign hybrids(hyacinthoides,ecristatum,lavendulare) to a R object
hybrids11<- alliumcanadense %>%
  select(Taxon,Latitude,Longitude)
hybrids<-hybrids11[c(22,32:40),]
hybrids<-na.omit(hybrids)

##prepare varieties,parentals,and hybrids for modeling
canadense <- canadense[,c(3,2)]
lavendulare <- lavendulare[,c(3,2)]
ecristatum<- ecristatum[,c(3,2)]
fraseri<- fraseri[,c(3,2)]
hyacinthoides<- hyacinthoides[,c(3,2)]
mobilense<- mobilense[,c(3,2)]
parentals<- parentals[,c(3,2)]
hybrids<- hybrids[,c(3,2)]

#making two objects for canadense that are permuted datasets: 
#x.permuted.object contains half of the canadense occurrence points and will be run in maxent with 1929 layers in for loop
#x.permuted.object2 contains half of the canadense occurrence points and will be run in maxent with 2011 layers in for loop
#assign 10 occurrence points from the canadense object to the x.permuted object and do not replace the values
x.permuted <- sample(1:nrow(canadense), size = 10, replace = FALSE)
#contains the row names of the canadense object in numerical order. The information in these rows will be put into x.permuted. 
x.permuted <- x.permuted[order(x.permuted)]
#display row names
x.permuted

#put the remaining row names of the canadense object into x.permuted2. 
x.permuted2 <- setdiff(1:nrow(canadense), x.permuted)
#contains the row names of the canadense object in numerical order. The information in these rows will be put into x.permuted2. 
x.permuted2 <- x.permuted2[order(x.permuted2)]
#display row names
x.permuted2

#import specific rows of canadense locality data into x.permuted.object and x.permuted.object2
#creates paired datasets
x.permuted.canadense1 <- canadense[x.permuted,]
x.permuted.canadense2 <- canadense[x.permuted2,]

##For loop 
#one dataset will run 100 times with 1929 layers in maxent, and an I statistic will be calculated for each run
#the other dataset will run 100 times with 2011 layers in maxent, and an I statistic will be calculated for each run
#The critical value (the fifth lowest I statistic out of 100) will be used to conclude whether the niches are significantly different for 1929 and 2011
permut.vals <-NULL
for (i in 1:100){
  

fred<-
permut.vals <- rbind(permut.vals,fred)
}