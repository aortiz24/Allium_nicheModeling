###Response Curve Graphs for Allium canadense
##Canadense 1929
#Giving number of occurrence points
replicates.number <- nrow(read.csv("models/canadense1929Maxent/maxentResults.csv"))-1

#Gives number of clmatic variables
predictor.number <- length(names(predictors9))

#Gives you the names of your climatic variables
predictor.names <- names(predictors9)

#makes averages (across each replicate) of your response curve data for each variable

#the outer loop makes the csv file final outputs for each variable where the y column has been replaced with averages across the replicates
for(i in 1:predictor.number){
  
  #this inner loop makes the averages for each environmental variable based on the response curve data of each replicate for that variable
  y.averages <- NULL
  for(j in 1:nrow(read.csv(paste("models/canadense1929Maxent/plots/","species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = "")))){
    
    #this loop makes a list of the y columns from the response curve data of each replicate of a given environmental variable	
    y.matrix <- NULL
    for(k in (1:replicates.number)){
      
      y.matrix <- c(y.matrix, read.csv(paste("models/canadense1929Maxent/plots/","species", "_", k-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[j,3])
      
    }
    
    #this command makes the full list of y averages for a given variable at each value of x
    y.averages <- c(y.averages, mean(y.matrix))
    
  }
  
  #this command actually writes the csv file (in main directory of project not the plots folder) for each variable, where the column of y's has been replaced with the average y's across replicates
  write.csv(cbind(read.csv(paste("models/canadense1929Maxent/","species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[1:2], y.averages), paste(predictor.names[i], ".csv", sep = ""), row.names = FALSE)
  
}

#precipitation 1929 layer
Can9.ppt <- read.csv("models/canadense1929Maxent/ppt9.csv")
pCan9X <- Can9.ppt[,2]
pCan9Y <- Can9.ppt[,3]

#mean temperature layer
Can9.tmean <- read.csv("models/canadense1929Maxent/tmean9.csv")
tCan9X <- Can9.tmean[,2]
tCan9Y <- Can9.tmean[,3]

#maximum vapor pressure deficit layer
Can9.vpdmax <- read.csv("models/canadense1929Maxent/vpdmax9.csv")
vCan9X <- Can9.vpdmax[,2]
vCan9Y <- Can9.vpdmax[,3]

#open png file
png(filename="figures/canadense_response_curves_1929.png")
#plot multiple response curve in png file in figures directory
par(mfrow=c(1,3))
#plot precipitation 1929 layer
plot(pCan9Y ~ pCan9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(a)")
#plot mean temperature 1929 layer
plot(tCan9Y ~ tCan9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(b)")
#plot maxiumum vapor pressure deficit 1929 layer
plot(vCan9Y ~ vCan9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Maximum Vapor Pressure Deficit", main = "(c)")
dev.off()

##Lavendulare 1929
#Giving number of occurrence points
replicates.number <- nrow(read.csv("models/lavendulare1929Maxent/maxentResults.csv"))-1

#Gives number of clmatic variables
predictor.number <- length(names(predictors9))

#Gives you the names of your climatic variables
predictor.names <- names(predictors9)

#makes averages (across each replicate) of your response curve data for each variable

#the outer loop makes the csv file final outputs for each variable where the y column has been replaced with averages across the replicates
for(i in 1:predictor.number){
  
  #this inner loop makes the averages for each environmental variable based on the response curve data of each replicate for that variable
  y.averages <- NULL
  for(j in 1:nrow(read.csv(paste("models/lavendulare1929Maxent/plots/","species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = "")))){
    
    #this loop makes a list of the y columns from the response curve data of each replicate of a given environmental variable	
    y.matrix <- NULL
    for(k in (1:replicates.number)){
      
      y.matrix <- c(y.matrix, read.csv(paste("models/lavendulare1929Maxent/plots/","species", "_", k-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[j,3])
      
    }
    
    #this command makes the full list of y averages for a given variable at each value of x
    y.averages <- c(y.averages, mean(y.matrix))
    
  }
  
  #this command actually writes the csv file (in main directory of project not the plots folder) for each variable, where the column of y's has been replaced with the average y's across replicates
  write.csv(cbind(read.csv(paste("models/lavendulare1929Maxent/","species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[1:2], y.averages), paste(predictor.names[i], ".csv", sep = ""), row.names = FALSE)
  
}

#precipitation 1929 layer
Lav9.ppt <- read.csv("models/lavendulare1929Maxent/ppt9.csv")
pLav9X <- Lav9.ppt[,2]
pLav9Y <- Lav9.ppt[,3]

#mean temperature layer
Lav9.tmean <- read.csv("models/lavendulare1929Maxent/tmean9.csv")
tLav9X <- Lav9.tmean[,2]
tLav9Y <- Lav9.tmean[,3]

#maximum vapor pressure deficit layer
Lav9.vpdmax <- read.csv("models/lavendulare1929Maxent/vpdmax9.csv")
vLav9X <- Lav9.vpdmax[,2]
vLav9Y <- Lav9.vpdmax[,3]

#open png file
png(filename="figures/lavendulare_response_curves_1929.png")
#plot multiple response curve in png file in figures directory
par(mfrow=c(1,3))
#plot precipitation 1929 layer
plot(pLav9Y ~ pLav9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(a)")
#plot mean temperature 1929 layer
plot(tLav9Y ~ tLav9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(b)")
#plot maxiumum vapor pressure deficit 1929 layer
plot(vLav9Y ~ vLav9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Maximum Vapor Pressure Deficit", main = "(c)")
dev.off()

##Mobilense 1929
#Giving number of occurrence points
replicates.number <- nrow(read.csv("models/mobilense1929Maxent/maxentResults.csv"))-1

#Gives number of clmatic variables
predictor.number <- length(names(predictors9))

#Gives you the names of your climatic variables
predictor.names <- names(predictors9)

#makes averages (across each replicate) of your response curve data for each variable

#the outer loop makes the csv file final outputs for each variable where the y column has been replaced with averages across the replicates
for(i in 1:predictor.number){
  
  #this inner loop makes the averages for each environmental variable based on the response curve data of each replicate for that variable
  y.averages <- NULL
  for(j in 1:nrow(read.csv(paste("models/mobilense1929Maxent/plots/","species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = "")))){
    
    #this loop makes a list of the y columns from the response curve data of each replicate of a given environmental variable	
    y.matrix <- NULL
    for(k in (1:replicates.number)){
      
      y.matrix <- c(y.matrix, read.csv(paste("models/mobilense1929Maxent/plots/","species", "_", k-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[j,3])
      
    }
    
    #this command makes the full list of y averages for a given variable at each value of x
    y.averages <- c(y.averages, mean(y.matrix))
    
  }
  
  #this command actually writes the csv file (in main directory of project not the plots folder) for each variable, where the column of y's has been replaced with the average y's across replicates
  write.csv(cbind(read.csv(paste("models/mobilense1929Maxent/","species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[1:2], y.averages), paste(predictor.names[i], ".csv", sep = ""), row.names = FALSE)
  
}

#precipitation 1929 layer
Mob9.ppt <- read.csv("models/mobilense1929Maxent/ppt9.csv")
pMob9X <- Mob9.ppt[,2]
pMob9Y <- Mob9.ppt[,3]

#mean temperature layer
Mob9.tmean <- read.csv("models/mobilense1929Maxent/tmean9.csv")
tMob9X <- Mob9.tmean[,2]
tMob9Y <- Mob9.tmean[,3]

#maximum vapor pressure deficit layer
Mob9.vpdmax <- read.csv("models/mobilense1929Maxent/vpdmax9.csv")
vMob9X <- Mob9.vpdmax[,2]
vMob9Y <- Mob9.vpdmax[,3]

#open png file
png(filename="figures/mobilense_response_curves_1929.png")
#plot multiple response curve in png file in figures directory
par(mfrow=c(1,3))
#plot precipitation 1929 layer
plot(pMob9Y ~ pMob9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(a)")
#plot mean temperature 1929 layer
plot(tMob9Y ~ tMob9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(b)")
#plot maxiumum vapor pressure deficit 1929 layer
plot(vMob9Y ~ vMob9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Maximum Vapor Pressure Deficit", main = "(c)")
dev.off()

##Fraseri 1929
#Giving number of occurrence points
replicates.number <- nrow(read.csv("models/fraseri1929Maxent/maxentResults.csv"))-1

#Gives number of clmatic variables
predictor.number <- length(names(predictors9))

#Gives you the names of your climatic variables
predictor.names <- names(predictors9)

#makes averages (across each replicate) of your response curve data for each variable

#the outer loop makes the csv file final outputs for each variable where the y column has been replaced with averages across the replicates
for(i in 1:predictor.number){
  
  #this inner loop makes the averages for each environmental variable based on the response curve data of each replicate for that variable
  y.averages <- NULL
  for(j in 1:nrow(read.csv(paste("models/fraseri1929Maxent/plots/","species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = "")))){
    
    #this loop makes a list of the y columns from the response curve data of each replicate of a given environmental variable	
    y.matrix <- NULL
    for(k in (1:replicates.number)){
      
      y.matrix <- c(y.matrix, read.csv(paste("models/fraseri1929Maxent/plots/","species", "_", k-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[j,3])
      
    }
    
    #this command makes the full list of y averages for a given variable at each value of x
    y.averages <- c(y.averages, mean(y.matrix))
    
  }
  
  #this command actually writes the csv file (in main directory of project not the plots folder) for each variable, where the column of y's has been replaced with the average y's across replicates
  write.csv(cbind(read.csv(paste("models/fraseri1929Maxent/","species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[1:2], y.averages), paste(predictor.names[i], ".csv", sep = ""), row.names = FALSE)
  
}

#precipitation 1929 layer
Fra9.ppt <- read.csv("models/fraseri1929Maxent/ppt9.csv")
pFra9X <- Fra9.ppt[,2]
pFra9Y <- Fra9.ppt[,3]

#mean temperature layer
Fra9.tmean <- read.csv("models/fraseri1929Maxent/tmean9.csv")
tFra9X <- Fra9.tmean[,2]
tFra9Y <- Fra9.tmean[,3]

#maximum vapor pressure deficit layer
Fra9.vpdmax <- read.csv("models/fraseri1929Maxent/vpdmax9.csv")
vFra9X <- Fra9.vpdmax[,2]
vFra9Y <- Fra9.vpdmax[,3]

#open png file
png(filename="figures/fraseri_response_curves_1929.png")
#plot multiple response curve in png file in figures directory
par(mfrow=c(1,3))
#plot precipitation 1929 layer
plot(pFra9Y ~ pFra9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(a)")
#plot mean temperature 1929 layer
plot(tFra9Y ~ tFra9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(b)")
#plot maxiumum vapor pressure deficit 1929 layer
plot(vFra9Y ~ vFra9X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Maximum Vapor Pressure Deficit", main = "(c)")
dev.off()

##Parentals 1929 - model must be loaded
# create objects for each variable in model
tPar1 <- response(maxParAdv9, var = 1) #mean temperature
pPar2 <- response(maxParAdv9, var = 2) #precipitation
vPar3 <- response(maxParAdv9, var = 3) #maximum vapor pressure deficit

#open png file
png(filename="figures/parentals_response_curves_1929.png")
# combined figure
par(mfrow=c(1,3))
#plot mean temperature 1929 layer
plot(tPar1, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(a)")
#plot precipitation 1929 layer
plot(pPar2, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(b)")
#plot maximum vapor pressure deficit layer
plot(vPar3, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Maximum Vapor Pressure Deficit", main = "(c)")
dev.off()

##Hybrids 1929 - model must be loaded
# create objects for each variable in model
tHyb1 <- response(maxHybAdv9, var = 1) #mean temperature
pHyb2 <- response(maxHybAdv9, var = 2) #precipitation
vHyb3 <- response(maxHybAdv9, var = 3) #maximum vapor pressure deficit

#open png file
png(filename="figures/hybrids_response_curves_1929.png")
# combined figure
par(mfrow=c(1,3))
#plot mean temperature 1929 layer
plot(tHyb1, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(a)")
#plot precipitation 1929 layer
plot(pHyb2, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(b)")
#plot maximum vapor pressure deficit layer
plot(vHyb3, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Maximum Vapor Pressure Deficit", main = "(c)")
dev.off()

##Canadense 2011
#Giving number of occurrence points
replicates.number <- nrow(read.csv("models/canadense2011Maxent/maxentResults.csv"))-1

#Gives number of clmatic variables
predictor.number <- length(names(predictors11))

#Gives you the names of your climatic variables
predictor.names <- names(predictors11)

#makes averages (across each replicate) of your response curve data for each variable

#the outer loop makes the csv file final outputs for each variable where the y column has been replaced with averages across the replicates
for(i in 1:predictor.number){
  
  #this inner loop makes the averages for each environmental variable based on the response curve data of each replicate for that variable
  y.averages <- NULL
  for(j in 1:nrow(read.csv(paste("models/canadense2011Maxent/plots/species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = "")))){
    
    #this loop makes a list of the y columns from the response curve data of each replicate of a given environmental variable	
    y.matrix <- NULL
    for(k in (1:replicates.number)){
      
      y.matrix <- c(y.matrix, read.csv(paste("models/canadense2011Maxent/plots/species", "_", k-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[j,3])
      
    }
    
    #this command makes the full list of y averages for a given variable at each value of x
    y.averages <- c(y.averages, mean(y.matrix))
    
  }
  
  #this command actually writes the csv file (in main directory of project not the plots folder) for each variable, where the column of y's has been replaced with the average y's across replicates
  write.csv(cbind(read.csv(paste("models/canadense2011Maxent/plots/species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[1:2], y.averages), paste(predictor.names[i], ".csv", sep = ""), row.names = FALSE)
  
}

#precipitation 2011 layer
Can11.ppt <- read.csv("models/canadense2011Maxent/ppt11.csv")
pCan11X <- Can11.ppt[,2]
pCan11Y <- Can11.ppt[,3]

#mean temperature layer
Can11.tmean <- read.csv("models/canadense2011Maxent/tmean11.csv")
tCan11X <- Can11.tmean[,2]
tCan11Y <- Can11.tmean[,3]

#maximum vapor pressure deficit layer
Can11.vpdmin <- read.csv("models/canadense2011Maxent/vpdmin11.csv")
vCan11X <- Can11.vpdmin[,2]
vCan11Y <- Can11.vpdmin[,3]

#mean dewpoint temperature 2011 layer
Can11.tdmean <- read.csv("models/canadense2011Maxent/tdmean11.csv")
dCan11X <- Can11.tdmean[,2]
dCan11Y <- Can11.tdmean[,3]

#open png file
png(filename="figures/canadense_response_curves_2011.png")
#plot multiple response curve in png file in figures directory
par(mfrow=c(2,2))
#plot precipitation 2011 layer
plot(pCan11Y ~ pCan11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(a)")
#plot mean temperature 2011 layer
plot(tCan11Y ~ tCan11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(b)")
#plot miniumum vapor pressure deficit 2011 layer
plot(vCan11Y ~ vCan11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Minimum Vapor Pressure Deficit", main = "(c)")
#plot mean dewpoint temperature 2011 layer
plot(dCan11Y ~ dCan11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Dewpoint Temperature (Celsius)", main = "(d)")
dev.off()

##Lavendulare 2011
#Giving number of occurrence points
replicates.number <- nrow(read.csv("models/lavendulare2011Maxent/maxentResults.csv"))-1

#Gives number of clmatic variables
predictor.number <- length(names(predictors11))

#Gives you the names of your climatic variables
predictor.names <- names(predictors11)

#makes averages (across each replicate) of your response curve data for each variable

#the outer loop makes the csv file final outputs for each variable where the y column has been replaced with averages across the replicates
for(i in 1:predictor.number){
  
  #this inner loop makes the averages for each environmental variable based on the response curve data of each replicate for that variable
  y.averages <- NULL
  for(j in 1:nrow(read.csv(paste("models/lavendulare2011Maxent/plots/species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = "")))){
    
    #this loop makes a list of the y columns from the response curve data of each replicate of a given environmental variable	
    y.matrix <- NULL
    for(k in (1:replicates.number)){
      
      y.matrix <- c(y.matrix, read.csv(paste("models/lavendulare2011Maxent/plots/species", "_", k-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[j,3])
      
    }
    
    #this command makes the full list of y averages for a given variable at each value of x
    y.averages <- c(y.averages, mean(y.matrix))
    
  }
  
  #this command actually writes the csv file (in main directory of project not the plots folder) for each variable, where the column of y's has been replaced with the average y's across replicates
  write.csv(cbind(read.csv(paste("models/lavendulare2011Maxent/plots/species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[1:2], y.averages), paste(predictor.names[i], ".csv", sep = ""), row.names = FALSE)
  
}

#precipitation 2011 layer
Lav11.ppt <- read.csv("models/lavendulare2011Maxent/ppt11.csv")
pLav11X <- Lav11.ppt[,2]
pLav11Y <- Lav11.ppt[,3]

#mean temperature layer
Lav11.tmean <- read.csv("models/lavendulare2011Maxent/tmean11.csv")
tLav11X <- Lav11.tmean[,2]
tLav11Y <- Lav11.tmean[,3]

#maximum vapor pressure deficit layer
Lav11.vpdmin <- read.csv("models/lavendulare2011Maxent/vpdmin11.csv")
vLav11X <- Lav11.vpdmin[,2]
vLav11Y <- Lav11.vpdmin[,3]

#mean dewpoint temperature 2011 layer
Lav11.tdmean <- read.csv("models/lavendulare2011Maxent/tdmean11.csv")
dLav11X <- Lav11.tdmean[,2]
dLav11Y <- Lav11.tdmean[,3]

#open png file
png(filename="figures/lavendulare_response_curves_2011.png")
#plot multiple response curve in png file in figures directory
par(mfrow=c(2,2))
#plot precipitation 2011 layer
plot(pLav11Y ~ pLav11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(a)")
#plot mean temperature 2011 layer
plot(tLav11Y ~ tLav11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(b)")
#plot miniumum vapor pressure deficit 2011 layer
plot(vLav11Y ~ vLav11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Minimum Vapor Pressure Deficit", main = "(c)")
#plot mean dewpoint temperature 2011 layer
plot(dLav11Y ~ dLav11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Dewpoint Temperature (Celsius)", main = "(d)")
dev.off()

##Mobilense 2011
#Giving number of occurrence points
replicates.number <- nrow(read.csv("models/mobilense2011Maxent/maxentResults.csv"))-1

#Gives number of clmatic variables
predictor.number <- length(names(predictors11))

#Gives you the names of your climatic variables
predictor.names <- names(predictors11)

#makes averages (across each replicate) of your response curve data for each variable

#the outer loop makes the csv file final outputs for each variable where the y column has been replaced with averages across the replicates
for(i in 1:predictor.number){
  
  #this inner loop makes the averages for each environmental variable based on the response curve data of each replicate for that variable
  y.averages <- NULL
  for(j in 1:nrow(read.csv(paste("models/mobilense2011Maxent/plots/species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = "")))){
    
    #this loop makes a list of the y columns from the response curve data of each replicate of a given environmental variable	
    y.matrix <- NULL
    for(k in (1:replicates.number)){
      
      y.matrix <- c(y.matrix, read.csv(paste("models/mobilense2011Maxent/plots/species", "_", k-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[j,3])
      
    }
    
    #this command makes the full list of y averages for a given variable at each value of x
    y.averages <- c(y.averages, mean(y.matrix))
    
  }
  
  #this command actually writes the csv file (in main directory of project not the plots folder) for each variable, where the column of y's has been replaced with the average y's across replicates
  write.csv(cbind(read.csv(paste("models/mobilense2011Maxent/plots/species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[1:2], y.averages), paste(predictor.names[i], ".csv", sep = ""), row.names = FALSE)
  
}

#precipitation 2011 layer
Mob11.ppt <- read.csv("models/mobilense2011Maxent/ppt11.csv")
pMob11X <- Mob11.ppt[,2]
pMob11Y <- Mob11.ppt[,3]

#mean temperature layer
Mob11.tmean <- read.csv("models/mobilense2011Maxent/tmean11.csv")
tMob11X <- Mob11.tmean[,2]
tMob11Y <- Mob11.tmean[,3]

#maximum vapor pressure deficit layer
Mob11.vpdmin <- read.csv("models/mobilense2011Maxent/vpdmin11.csv")
vMob11X <- Mob11.vpdmin[,2]
vMob11Y <- Mob11.vpdmin[,3]

#mean dewpoint temperature 2011 layer
Mob11.tdmean <- read.csv("models/mobilense2011Maxent/tdmean11.csv")
dMob11X <- Mob11.tdmean[,2]
dMob11Y <- Mob11.tdmean[,3]

#open png file
png(filename="figures/mobilense_response_curves_2011.png")
#plot multiple response curve in png file in figures directory
par(mfrow=c(2,2))
#plot precipitation 2011 layer
plot(pMob11Y ~ pMob11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(a)")
#plot mean temperature 2011 layer
plot(tMob11Y ~ tMob11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(b)")
#plot miniumum vapor pressure deficit 2011 layer
plot(vMob11Y ~ vMob11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Minimum Vapor Pressure Deficit", main = "(c)")
#plot mean dewpoint temperature 2011 layer
plot(dMob11Y ~ dMob11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Dewpoint Temperature (Celsius)", main = "(d)")
dev.off()

##Fraseri 2011
#Giving number of occurrence points
replicates.number <- nrow(read.csv("models/fraseri2011Maxent/maxentResults.csv"))-1

#Gives number of clmatic variables
predictor.number <- length(names(predictors11))

#Gives you the names of your climatic variables
predictor.names <- names(predictors11)

#makes averages (across each replicate) of your response curve data for each variable

#the outer loop makes the csv file final outputs for each variable where the y column has been replaced with averages across the replicates
for(i in 1:predictor.number){
  
  #this inner loop makes the averages for each environmental variable based on the response curve data of each replicate for that variable
  y.averages <- NULL
  for(j in 1:nrow(read.csv(paste("models/fraseri2011Maxent/plots/species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = "")))){
    
    #this loop makes a list of the y columns from the response curve data of each replicate of a given environmental variable	
    y.matrix <- NULL
    for(k in (1:replicates.number)){
      
      y.matrix <- c(y.matrix, read.csv(paste("models/fraseri2011Maxent/plots/species", "_", k-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[j,3])
      
    }
    
    #this command makes the full list of y averages for a given variable at each value of x
    y.averages <- c(y.averages, mean(y.matrix))
    
  }
  
  #this command actually writes the csv file (in main directory of project not the plots folder) for each variable, where the column of y's has been replaced with the average y's across replicates
  write.csv(cbind(read.csv(paste("models/fraseri2011Maxent/plots/species", "_", i-1, "_", predictor.names[i], "_", "only.dat", sep = ""))[1:2], y.averages), paste(predictor.names[i], ".csv", sep = ""), row.names = FALSE)
  
}

#precipitation 2011 layer
Fra11.ppt <- read.csv("models/fraseri2011Maxent/ppt11.csv")
pFra11X <- Fra11.ppt[,2]
pFra11Y <- Fra11.ppt[,3]

#mean temperature layer
Fra11.tmean <- read.csv("models/fraseri2011Maxent/tmean11.csv")
tFra11X <- Fra11.tmean[,2]
tFra11Y <- Fra11.tmean[,3]

#maximum vapor pressure deficit layer
Fra11.vpdmin <- read.csv("models/fraseri2011Maxent/vpdmin11.csv")
vFra11X <- Fra11.vpdmin[,2]
vFra11Y <- Fra11.vpdmin[,3]

#mean dewpoint temperature 2011 layer
Fra11.tdmean <- read.csv("models/fraseri2011Maxent/tdmean11.csv")
dFra11X <- Fra11.tdmean[,2]
dFra11Y <- Fra11.tdmean[,3]

#open png file
png(filename="figures/fraseri_response_curves_2011.png")
#plot multiple response curve in png file in figures directory
par(mfrow=c(2,2))
#plot precipitation 2011 layer
plot(pFra11Y ~ pFra11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(a)")
#plot mean temperature 2011 layer
plot(tFra11Y ~ tFra11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(b)")
#plot miniumum vapor pressure deficit 2011 layer
plot(vFra11Y ~ vFra11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Minimum Vapor Pressure Deficit", main = "(c)")
#plot mean dewpoint temperature 2011 layer
plot(dFra11Y ~ dFra11X, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Dewpoint Temperature (Celsius)", main = "(d)")
dev.off()

##Parentals 2011 - model must be loaded
# create objects for each variable in model
tPar1 <- response(maxParAdv11, var = 1) #mean temperature
pPar2 <- response(maxParAdv11, var = 2) #precipitation
vPar3 <- response(maxParAdv11, var = 3) #minimum vapor pressure deficit
dPar4 <- response(maxParAdv11, var = 4) #mean dewpoint temperature

#open png file
png(filename="figures/parentals_response_curves_2011.png")
# combined figure
par(mfrow=c(2,2))
#plot mean temperature 2011 layer
plot(tPar1, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(a)")
#plot precipitation 2011 layer
plot(pPar2, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(b)")
#plot minimum vapor pressure deficit layer
plot(vPar3, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Minimum Vapor Pressure Deficit", main = "(c)")
#plot mean dewpoint temperature layer
plot(dPar4, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Dewpoint Temperature (Celsius)", main = "(d)")
dev.off()

##Hybrids 2011 - model must be loaded
# create objects for each variable in model
tHyb1 <- response(maxHybAdv11, var = 1) #mean temperature
pHyb2 <- response(maxHybAdv11, var = 2) #precipitation
vHyb3 <- response(maxHybAdv11, var = 3) #minimum vapor pressure deficit
dHyb4 <- response(maxHybAdv11, var = 4) #mean dewpoint temperature

#open png file
png(filename="figures/hybrids_response_curves_2011.png")
# combined figure
par(mfrow=c(2,2))
#plot mean temperature 2011 layer
plot(tHyb1, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Temperature (Celsius)", main = "(a)")
#plot precipitation 2011 layer
plot(pHyb2, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Precipitation (mm)", main = "(b)")
#plot minimum vapor pressure deficit layer
plot(vHyb3, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Minimum Vapor Pressure Deficit", main = "(c)")
#plot mean dewpoint temperature layer
plot(dHyb4, type = "l", col="red", lwd=5, ylim=c(0,1), ylab = "Habitat Suitability", xlab = "Mean Dewpoint Temperature (Celsius)", main = "(d)")
dev.off()