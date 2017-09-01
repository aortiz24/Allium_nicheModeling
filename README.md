# Allium Niche Modeling

Scripts for creating maps and performing niche modeling in RStudio.

The general workflow is:

•	`mapping.R`: importing Allium canadense occurrence data into R, creating simple maps, creating customized shapefiles

•	`layerPrep.R`: masking/clipping PRISM layers, looking for correlations between layers

•	`maxent.R`: creating niche models using occurrence data and climate layers

•	`nicheOverlap.R`: assessing whether niche models for different varieties are distinct from each other

• `creatingPermutedAlliumDatasets.R`: assessing the accuracy of identification of climatic determinants of a variaties' range

There are a number of files and directories that are downloaded or created during this analysis. To perform all commands in `layerPrep.R`, you should download the PRISM layers and store them in a convenient place (note the paths for using these layers may need to be changed). Clipped layers are included in `layers` for your convenience. A shapefile created in `mapping.R` is included in`shapefiles` for convenience. Models are included for all varieties, hybrids, and parentals for Allium canadense in `models` for observation. ANOVA and Tukey's test results are included in `anova_results` for observation.
