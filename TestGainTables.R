###Constructing figures for Thesis
#load libraries
library(plyr)
library(gridExtra)

##make test gain table for Allium
#for canadense in 1929
can9<- read.csv("models/canadense1929Maxent/maxentResults.csv")
can9<- can9[ 11,c(9,8,27:29)]

#for lavendulare in 1929
lav9<- read.csv("models/lavendulare1929Maxent/maxentResults.csv")
lav9<- lav9[ 7,c(9,8,27:29)]

#no maxent results for ecristatum in 1929 
#ecr9<- read.csv("models/ecristatum1929Maxent/maxentResults.csv")
#ecr9<- ecr9[ ,c(9,8,27:29)]

#not including hyacinthoides in 1929 since used only 2 occurrence points
#hya9<- read.csv("models/hyacinthoides1929Maxent/maxentResults.csv")
#hya9<- hya9[ 3,c(9,8,27:29)]

#for mobilense in 1929
mob9<- read.csv("models/mobilense1929Maxent/maxentResults.csv")
mob9<- mob9[ 12,c(9,8,27:29)]

#for fraseri in 1929
fra9<- read.csv("models/fraseri1929Maxent/maxentResults.csv")
fra9<- fra9[ 10,c(9,8,27:29)]

#for parentals in 1929
par9<- read.csv("models/parentals1929Maxent/maxentResults.csv")
par9<- par9[ ,c(9,8,27:29)]

#for hybrids in 1929
hyb9<- read.csv("models/hybrids1929Maxent/maxentResults.csv")
hyb9<- hyb9[ ,c(9,8,27:29)]

#combine rows from all result tables into 1929 test gain table
testGainTable1929 <- rbind(can9,lav9,mob9,fra9,par9,hyb9)

#for canadense in 2011
can11<- read.csv("models/canadense2011Maxent/maxentResults.csv")
can11<- can11[ 11,c(9,8,32:35)]

#for lavendulare in 2011
lav11<- read.csv("models/lavendulare2011Maxent/maxentResults.csv")
lav11<- lav11[ 7,c(9,8,32:35)]

#no maxent results for ecristatum in 2011 
#ecr11<- read.csv("models/ecristatum2011Maxent/maxentResults.csv")
#ecr11<- ecr11[ ,c(9,8,32:35)]

#not including hyacinthoides in 1929 since used only 2 occurrence points
#hya11<- read.csv("models/hyacinthoides2011Maxent/maxentResults.csv")
#hya11<- hya11[ 3,c(9,8,32:35)]

#for mobilense in 2011
mob11<- read.csv("models/mobilense2011Maxent/maxentResults.csv")
mob11<- mob11[ 12,c(9,8,32:35)]

#for fraseri in 2011
fra11<- read.csv("models/fraseri2011Maxent/maxentResults.csv")
fra11<- fra11[ 10,c(9,8,32:35)]

#for parentals in 2011
par11<- read.csv("models/parentals2011Maxent/maxentResults.csv")
par11<- par11[ ,c(9,8,32:35)]

#for hybrids in 2011
hyb11<- read.csv("models/hybrids2011Maxent/maxentResults.csv")
hyb11<- hyb11[ ,c(9,8,32:35)]

#combine rows from all result tables into 2011 test gain table
testGainTable2011 <- rbind(can11,lav11,mob11,fra11,par11,hyb11)

##making publicatoin ready test gain table
#create Taxon column for testGainTable1929 & testGainTable2011
Taxon<-data.frame(Taxon = c("canadense","lavendulare","mobilense","fraseri","parentals","hybrids"))
#add column to testGainTable1929 & testGainTable2011
FinalTestGainTable1929<-cbind(Taxon,testGainTable1929)
FinalTestGainTable2011<-cbind(Taxon,testGainTable2011)

#using plyr,rename columns for testGainTable1929 & testGainTable2011
FinalTestGainTable1929<-rename(FinalTestGainTable1929,c("Test.AUC"="Test AUC","Test.gain"="Full model","Test.gain.with.only.ppt9"="Only precipitation","Test.gain.with.only.tmean9"="Only mean temperature","Test.gain.with.only.vpdmax9"="Only maximum vapor pressure deficit"))
FinalTestGainTable2011<-rename(FinalTestGainTable2011,c("Test.AUC"="Test AUC","Test.gain"="Full model","Test.gain.with.only.ppt11"="Only precipitation","Test.gain.with.only.tmean11"="Only mean temperature","Test.gain.with.only.vpdmin11"="Only minimum vapor pressure deficit","Test.gain.with.only.tdmean11"="Only mean dewpoint temperature"))

#export tables
#1929
pdf(file = "figures/TestGainTable1929.pdf",height = 2, width = 10)
grid.table(FinalTestGainTable1929)
dev.off()

#2011
pdf(file = "figures/TestGainTable2011.pdf",height = 2, width = 13)
grid.table(FinalTestGainTable2011)
dev.off()
