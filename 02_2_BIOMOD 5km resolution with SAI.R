############################################################################################################
############################                    BIOMOD                     #################################
############################################################################################################
## This BIOMOD for 5km resolution and considering LGM climate
############################################################################################################


##################################################################################################################
########################        Load libraries
##################################################################################################################

library(rgdal)
library(raster)
library(dplyr)
library(biomod2)

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
########################        5km grid data import
##################################################################################################################
# character string of target genus name
genus_name <- "Chionochloa"

### Set arguments
# data frame of occurrence data and climate data
if (genus_name == "Acaena"){
  datapath <- "Y://Acaena_bioclim_landcover_history_worldclim1_5km.csv"
}else{
  datapath <- "Y://Chionochloa_bioclim_landcover_history_worldclim1_5km.csv"
}


### Import raster of climate data
# Reference raster for coordinate system. This raster must have the same dimentions as the raster of occurrence data
ref.raster <- raster(
  paste("Y://GIS map and Climate data//newzealandpotentialvegetatio5.bil", sep="")
)
proj4stringNZTM <- proj4string(ref.raster)
# Worldclim ver.1.4
path <-"Y:\\GIS map and Climate data\\worldclim\\bio_411"
source(".\\functions\\F01_project_resample_WORLDCLIM.R")


# Import data frame of bioclim and occurrence records
if(file.exists(datapath)){
  climate.occ <- read.csv(datapath)
}else{
  print("Adjust arguments in the following R file to generate the data")
  source(".//Chionochloa niche evolution//scripts//01_createData18Sep.R")
}

# Change colmun name of coordinates 
names(climate.occ)[names(climate.occ) %in% c("x","y")] <- c("NZTMlon", "NZTMlat")

##############################################################################
### Add SAIcl to bioclim rasters
##############################################################################
nam <- load("Y:\\5th chapter SAI chapter\\meta data\\SAI_5km_currentInLGM_5000kmWindow_4var.data")
sai <- get(nam)
load("Y:\\5th chapter SAI chapter\\raw data\\Scores_Acaena_landcover5km.data")

source(".//functions//F02_create_raster_from_dataframe.R")

scores.sai <- cbind(scores, unlist(sai))

# Convert dataframe to raster
sai.raster <- convert_dataframe_to_raster(ref.raster, scores.sai, c("x","y"), "unlist(sai)")
names(sai.raster) <- "sai_cl"

##############################################################################
### Add SAIcc to bioclim rasters
##############################################################################

nam <- load("Y:\\5th chapter SAI chapter\\meta data\\SAI_5km_currentInCurrent_5000kmWindow_4var.data")
sai <- get(nam)

scores.sai1500 <- cbind(scores, unlist(sai))

# Convert dataframe to raster
sai.raster1500 <- convert_dataframe_to_raster(ref.raster, scores.sai1500, c("x","y"), "unlist(sai)")
names(sai.raster1500) <- "sai_cc"

# Create raster stack of bioclim and SAI rasters
bio.ras <- stack(bioNZ)
data.ras <- addLayer(bio.ras, c(sai.raster, sai.raster1500))

##############################################################################
### Add the difference between SAIcc - SAIcl to rasters
##############################################################################

load("diff_SAI_5km_wholeNZ27Feb.data")

scores.sai.diff <- merge(scores.sai, sai.diff, by=c("x","y"))

# Convert dataframe to raster
sai.diff.ras <- convert_dataframe_to_raster(ref.raster, scores.sai.diff, c("x","y"), "diff")
names(sai.diff.ras) <- "sai_diff"

# Create raster stack of bioclim and SAI rasters
bio.ras <- stack(bioNZ)
data.ras <- addLayer(bio.ras, c(sai.raster, sai.raster1500, sai.diff.ras))

##############################################################################
### Collate data
##############################################################################
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
myExpl <- stack(data.ras[[c("bioclim1", "bioclim6", "bioclim12", "bioclim15", "sai_cl", "sai_cc" , "sai_diff"
                            )]])

## Stay in a specific working directory, because all data needed for restoration is saved there.
if(dir.exists("Y://BIOMOD for Grid2") == FALSE){
  dir.create("Y://BIOMOD for Grid2")
}

setwd("Y://BIOMOD for Grid2")

#########################################################
## Takes a while to run... Don't run on laptop! Slow!
#########################################################

# NOTE; default setting has 5 fold cross validation with 75% training data.
# You can change the arguments accessing .\\Range fillings\\F_Create_Package_BIOMOD.r"

for(i in spname){
  tryCatch(
    runBiomod(i, data = climate.occ3, myExpl = myExpl, folder.name = "SAIdiff_4Mar19"),
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
}


#################################################################################################################
########################        PROJECT THE MODELS ONTO CURRENT CLIMATE SPACE
#################################################################################################################

# get folder names
folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]

#########################################################
## Takes a while to run... slow but wait!
#########################################################

for(i in folders){
  tryCatch(
    biomodProjection_fromSavedBiomodModels(i,
                                           modelname = "SAIdiff_4Mar19",
                                           proj.name = "SAIdiff_4Mar19"),
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
}
#################################################################################################################
########################        Plot BIOMOD projections
#################################################################################################################

projectionPlot <- function(spname, # species name
                           proj.name
){
  # load projection data
  modelname <- load(paste(".\\", spname, "\\proj_", proj.name, "\\", spname, ".", proj.name,  ".projection.out",
                          sep=""))
  model <- get(modelname)
  
  ## plot each projection separately 
  proj_val <- get_predictions(model)
  
  for (i in 1:length(proj_val@layers)) {
    
    png(filename = paste(".\\", spname, "\\proj_", proj.name, "\\", names(subset(proj_val, i)), ".png", sep=""), 
        height = 900, width = 750, units = "px")
    plot(proj_val[[i]])
    title(names(subset(proj_val, i)))
    dev.off()
  }
  
}


for(i in folders){
  tryCatch(
    projectionPlot(i, proj.name = "SAIdiff_4Mar19"),
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
}

#################################################################################################################
########################        BIOMOD ensamble models
#################################################################################################################

# Get folder names
folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]

ensembleModelling_projection <- function(spname, # species name
                                         folder.name,
                                         BIOMODproj.name, ensambleProj.name
) {
  # load projection data
  files <- list.files(paste(".//", spname, "//proj_", ensambleProj.name, sep = ""), full.names = T)
  ensamblefile <- (files  %>% grepl("out$", .) %>% files[.])
  
  if(length(ensamblefile) > 0){
    
    print("The ensamble prediction result file already exists.")
    
  }else{
    
    # Load BIOMOD.model.out
    mod <- load(paste(".\\", spname, "\\", spname, ".", folder.name, ".models.out",
                      sep = "")
    )
    model <- get(mod)
    
    ## Ensemble modelling
    myBiomodEM <- BIOMOD_EnsembleModeling(modeling.output = model,
                                          chosen.models = 'all',
                                          em.by = 'all',
                                          eval.metric = c('TSS'),
                                          eval.metric.quality.threshold = c(0.7),
                                          # Models.eval.meth must be one or two, because temporary raster folder can't store data of models for more than 3 evaluation metrics.
                                          # If you run this on computer with bigger storage for the folder,  it may run without the error. (In writeBin(as.vector(v[start:end, ]), x@file@con, size = x@file@dsize) :problem writing to connection)
                                          models.eval.meth = c('TSS'), #, 'ROC', 'ACCURACY'
                                          prob.mean = TRUE,
                                          prob.cv = FALSE,
                                          prob.ci = FALSE,
                                          prob.ci.alpha = 0.05,
                                          prob.median = FALSE,
                                          committee.averaging = FALSE,
                                          prob.mean.weight = TRUE,
                                          prob.mean.weight.decay = 'proportional'
    )
    
    # Load BIOMOD.projection.output
    modelname <- 
      load(paste(".\\", spname, "\\proj_", BIOMODproj.name, "\\", spname, ".", BIOMODproj.name, ".projection.out",
                 sep = "")
      )
    projModel <- get(modelname)
    
    # Creating the ensemble projections
    # Nothing is returned by this function, but specific projection files () are saved on the hard drive projection folder. 
    BIOMOD_EnsembleForecasting(projection.output = projModel,
                               EM.output = myBiomodEM,
                               proj.name = ensambleProj.name,
                               # Threshold to return presence/absence. 
                               # binary.meth = NULL returns only probability, but if binary.meth is specified both presence/absence and probability are returned.
                               binary.meth ="TSS"
    )
    
    
  }
}

# If there are species whose BIOMOD failed and no grd file was generated, you don't get the prediction.
for(i in folders){
  tryCatch(
    ensembleModelling_projection(i, 
                                 folder.name = "SAIdiff_4Mar19", 
                                 BIOMODproj.name = "SAIdiff_4Mar19", 
                                 ensambleProj.name = "SAIdiff_4Mar19_ensamble"),
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
}


## Plot ensemble projection

EMprojectionPlot <- function(spname, # species name
                             proj.name # file location of ensamble model projection.out
) {
  # load projection data
  files <- list.files(paste(".//", spname, "//proj_", proj.name, sep = ""), full.names = T)
  
  # If binary.meth in BIOMOD_EnsembleForecasting() was specified, you got two grd files.
  proj <- lapply((files  %>% grepl("grd$", .) %>% files[.]), raster)
  
  if(length(proj) > 1){
    proj <- proj[[2]]
  }
  
  png(filename = paste(".//ensemble_projection//", names(proj), "_", proj.name, ".png", sep = ""),
      height = 900, width = 750, units = "px"
  )
  plot(proj)
  title(names(proj))
  dev.off()
  
}

# If there are species whose BIOMOD failed and no grd file was generated, you don't get the plot.
for(i in folders){
  tryCatch(
    EMprojectionPlot(i, proj.name = "SAIdiff_4Mar19"),
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
}
