##################################################################################
### blockCV (http://htmlpreview.github.io/?https://github.com/rvalavi/blockCV/blob/master/vignettes/BlockCV_for_SDM.html)
##################################################################################

library(raster)
library(blockCV)

## Import data
# import explanatory varibales
awt <- raster::brick(system.file("extdata", "awt.grd", package = "blockCV"))

# import species presence-absence data
PA <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
# make a SpatialPointsDataFrame object from data.frame
pa_data <- SpatialPointsDataFrame(PA[,c("x", "y")], PA, proj4string=crs(awt))

# plot data
plot(awt[[1]]) 
points(pa_data[which(pa_data$Species==1), ], col="red") # add presence points
points(pa_data[which(pa_data$Species==0), ], col="blue") # add absence points
legend(x=500000, y=8250000, legend=c("Presence","Absence"), col=c(2, 4), pch=c(1,1), bty="n")

##################################################################################
## Visualize spatial autocorrelation
##################################################################################

# Visualize spatial dependency of explanatory variables
sac <- spatialAutoRange(rasterLayer = awt,
                        sampleNumber = 5000,
                        border = NULL,
                        showPlots = TRUE,
                        plotVariograms = FALSE,
                        doParallel = FALSE)
plot(sac$variograms[[1]])

# spatial blocking by specified range with random assignment
sb <- spatialBlock(speciesData = pa_data,
                   species = "Species",
                   rasterLayer = awt,
                   theRange = 68000, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 250, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)


# adding points on saptialBlock plot
sb$plots + geom_point(data = as.data.frame(coordinates(pa_data)), aes(x=x, y=y), alpha=0.6)

##################################################################################
## Find the best number of folds and block size
##################################################################################
# What's the best?

# explore generated folds
foldExplorer(blocks = sb, 
             rasterLayer = awt, 
             speciesData = pa_data)
# explore the block size
rangeExplorer(rasterLayer = awt) # the only mandatory input

# add species data to add them on the map
rangeExplorer(rasterLayer = awt,
              speciesData = pa_data,
              species = "Species",
              rangeTable = NULL,
              minRange = 30000, # limit the search domain
              maxRange = 100000)

##################################################################################
# Use blockCV in biomod2
##################################################################################

library(biomod2)
# species occurrences
DataSpecies <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
# the name of studied species
myRespName <- "Species"
# the presence/absences data for our species
myResp <- as.numeric(DataSpecies[,myRespName])
# the XY coordinates of species data
myRespXY <- DataSpecies[,c("x","y")]
# change the RasterBrick to RasterStack
awt <- stack(awt)

# 1. Formatting Data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = awt, # explanatory raster data
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     na.rm = TRUE)

# 2. Defining the folds for DataSplitTable
# note that biomodTable should be used here not folds
DataSplitTable <- sb$biomodTable # use generated folds from spatialBlock in previous section

# 3. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()

# 4. Model fitting
myBiomodModelOut <- BIOMOD_Modeling( myBiomodData,
                                     models = c('GLM','MARS','GBM'),
                                     models.options = myBiomodOption,
                                     DataSplitTable = DataSplitTable, # blocking folds
                                     VarImport = 0,
                                     models.eval.meth = c('ROC'),
                                     do.full.models=FALSE,
                                     modeling.id="test")

# 5. Model evaluation
# get all models evaluation
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
myBiomodModelEval["ROC","Testing.data",,,]


### Variable importance
myBiomodModelOut@variables.importances@val

