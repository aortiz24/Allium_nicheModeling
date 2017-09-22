# Allium Niche Modeling

Scripts for creating maps and performing niche modeling in RStudio.

The general workflow is:

•	`mapping.R`: importing Allium canadense occurrence data into R, creating simple maps, creating customized shapefiles

•	`layerPrep.R`: masking/clipping PRISM layers, looking for correlations between layers

•	`maxent.R`: creating niche models using occurrence data and climate layers

•	`nicheOverlap.R`: assessing whether niche models for different varieties are distinct from each other

• `creatingPermutedAlliumDatasets.R`: assessing the accuracy of identification of climatic determinants of a variaties' range

• `ResponseCurves.R`: creating figures that each have multiple response curve graphs representing each environmental layers included in each model constructed

There are a number of files and directories that are downloaded or created during this analysis. To perform all commands in `layerPrep.R`, you should download the PRISM 1929 layers from [Recent Years](http://prism.oregonstate.edu/recent/) and the PRISM 2011 layers from [Historical Past](http://prism.oregonstate.edu/historical/). Store them in a convenient place (note the paths for using these layers may need to be changed). Clipped layers are included in `layers` for your convenience. A shapefile created in `mapping.R` is included in`shapefiles` for convenience. Models are included for all varieties, hybrids, and parentals for Allium canadense in `models` for observation. ANOVA and Tukey's test results are included in `anova_results` for observation.
