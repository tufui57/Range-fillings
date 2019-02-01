############################################################################################################
############################                    BIOMOD                     #################################
############################################################################################################

x <- c("rgdal", "raster", "dplyr", "biomod2")
lapply(x, require, character.only = TRUE)

######################################################################################################
## NOTE! MAKE SURE package "biomod2" is the latest version! IF NOT, INSTALL AGAIN!
## Otherwise, you get errors on model developing!
######################################################################################################

# Load functions to develop BIOMOD
source(".\\Range fillings\\F_Create_Package_BIOMOD.r")

############ NOTE ########################################################################
# BIOMOD_Projection() & BIOMOD_Modeling() write raster files in tempdir()
# MAKE SURE TEMPORARY RASTER FOLDER IS NOT FULL! 
##########################################################################################

# clear temporary raster folder 
tempFilePath <- list.files(rasterOptions()$tmpdir, full.names = T)
file.remove(tempFilePath)

##################################################################################################################
########################        1km grid data import
##################################################################################################################
# character string of target genus name
genus_name <- "Acaena"

### Set arguments
# data frame of occurrence data and climate data
if (genus_name == "Acaena"){
  datapath <- "Y://1st chapter_Acaena project//Acaena manuscript//meta data//Acaena_bioclim_landcover_history_worldclim1_1km.csv"
}else{
  datapath <- "Y://Chionochloa_bioclim_landcover_history_worldclim1_1km.csv"
}


### Import raster of climate data
# Reference raster for coordinate system. This raster must have the same dimentions as the raster of occurrence data
ref.raster <- raster(
  paste("Y://GIS map and Climate data//newzealandpotentialvegetatio1.bil", sep="")
)
proj4stringNZTM <- proj4string(ref.raster)
# Worldclim ver.1.4
path <-"Y:\\GIS map and Climate data\\worldclim\\bio_411"
source(".\\functions\\F01_project_resample_WORLDCLIM.R")


# Import data frame of bioclim and occurrence records
climate.occ <- read.csv(datapath)
# Change colmun name of coordinates 
names(climate.occ)[names(climate.occ) %in% c("x","y")] <- c("NZTMlon", "NZTMlat")

# Change object name of bioclim rasters
data.ras <- bioNZ


# Get species name
spname <- colnames(climate.occ)[grepl(genus_name, colnames(climate.occ))]

# Remove NA from BIOCLIM
climate.occ2 <- (is.na(climate.occ$bioclim1) == F) %>% climate.occ[.,]
# Remove rows which have no occurrence records of any species
climate.occ3 <- (rowSums(climate.occ2[, spname], na.rm = T) > 0) %>% climate.occ2[.,]
# Remove species with < 10 presence records out of species name vector.
spname <- spname[(climate.occ3[, spname] %>% colSums(., na.rm = TRUE)) > 9]
# Remove wrong species names
if (genus_name == "Acaena"){
  spname <-  spname[spname!="Acaena_microphylla"] 
}

### Generate pseudo-absence
### I assume cells with any other species than the target species as the cells where the target species was absent. 
for(i in spname){climate.occ3[is.na(climate.occ3[, i]), i] <- 0}

# Environmental variables extracted from BIOCLIM and converted into NZTM.
myExpl <- stack(data.ras[c("bioclim1", "bioclim6", "bioclim12", "bioclim15")])

#########################################################
## Takes a while to run... Don't run on laptop! Slow!
#########################################################

setwd("Y://BIOMOD for Grid2")

## test
i = spname[10]
data = climate.occ3
folder.name = "blockCV_01Feb19"

## presence/absence vector
spData <- data[, grepl(i, colnames(data))]

## SET THE NAME - THIS IS A CHARACTER STRING THAT JUST HAS THE NAME OF THE SPECIES
##  USED FOR NAMING FILES AND FIGS
myRespName <- i

## SET THE RESPONSE VARIABLE
myResp <- as.numeric(spData)

## SET THE COORDINATES
myRespXY <- data[, c("NZTMlon", "NZTMlat")]


## FORMAT THE DATA FOR BIOMOD
## USING THE DEFAULTNAMES FOR VARIABLES THAT WE HAVE SPECIFIED ABOVE
myBiomodData <- BIOMOD_FormatingData(
  resp.var = myResp,
  resp.xy = myRespXY,
  expl.var = myExpl,
  resp.name = myRespName
)

## DEFINE THE MODEL OPTIONS: BELOW SETS THEM TO ALLTHE DEFAULTS
myBiomodOption <- BIOMOD_ModelingOptions()

bioclim.ras <- raster::brick(data.ras[c("bioclim1","bioclim6","bioclim12","bioclim15")])

sac <- spatialAutoRange(rasterLayer = bioclim.ras, # raster file
                 sampleNumber = 50000, # number of cells to be used
                 doParallel = TRUE,
                 showPlots = TRUE)
plot(sac$variograms[[1]])

# spatial blocking by specified range with random assignment
sb.sp <- spatialBlock(speciesData = spData,
                   species = "Species",
                   rasterLayer = bioclim.ras,
                   theRange = , # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 250, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)


# adding points on saptialBlock plot
sb$plots + geom_point(data = as.data.frame(coordinates(pa_data)), aes(x=x, y=y), alpha=0.6)

# 2. Defining the folds for DataSplitTable
# note that biomodTable should be used here not folds
DataSplitTable <- sb$biomodTable # use generated folds from spatialBlock in previous section

# # 4. Model fitting
# myBiomodModelOut.block <- BIOMOD_Modeling( myBiomodData,
#                                      models = c('GLM','MARS','GBM'),
#                                      models.options = myBiomodOption,
#                                      DataSplitTable = DataSplitTable, # blocking folds
#                                      VarImport = 100,
#                                      models.eval.meth = c('ROC'),
#                                      do.full.models=FALSE,
#                                      modeling.id="test")
## RUN THE MODELS
myBiomodModelOut <- BIOMOD_Modeling(myBiomodData,
                                    models = c('MARS', 'RF', 'ANN', 'GBM', 'GAM', 'GLM', 'SRE'
                                    ),
                                    models.options = myBiomodOption,
                                    NbRunEval = 5, # How many times evaluation is implemented.
                                    DataSplitTable = DataSplitTable, # blocking folds
                                    VarImport = 100,
                                    
                                    # For the difference between all the evaluation methods. See https://rdrr.io/cran/biomod2/man/BIOMOD_Modeling.html
                                    models.eval.meth = c('TSS'),
                                    rescal.all.models = FALSE,
                                    do.full.models = TRUE,
                                    modeling.id = folder.name
)

##########################################################
## NOTE! MAKE SURE TEMPORARY RASTER FOLDER IS NOT FULL! ##
##########################################################

# clear the folder 
tempFilePath <- list.files(rasterOptions()$tmpdir, full.names = T)
file.remove(tempFilePath)


for(i in spname){
  tryCatch(
    runBiomod(i, data = climate.occ3, myExpl = myExpl, folder.name = "blockCV_01Feb19"),
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
}