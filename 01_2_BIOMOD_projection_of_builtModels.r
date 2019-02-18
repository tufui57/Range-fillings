#################################################################################################################
###   Model projection for new data, LGM climate    
#################################################################################################################
### Use this script, if you want to change output style of projection from the one in "BIOMOD 5km resolution.R".
### This script is based on "BIOMOD 5km resolution.R".

library(dplyr)
library(raster)
library(biomod2)
# Load functions to develop BIOMOD
source(".\\Range fillings\\F_Create_Package_BIOMOD.r")
# Create LGM raster stuck
source(".\\Range fillings\\01_4_create_LGMclimate_raster.r")

biomodProjection_forNewData <- function(foldername, # Folder names containing BIOMOD models
                                        newData, # input environmental data for projection
                                        modelname, # Same name used as folder.name in runBiomod function
                                        proj.name # Name of projection
) {
  
  # Path of .models.out file
  biomodModelPath <- paste(foldername, "/", foldername, ".", modelname, ".models.out", sep = "")
  
  
  # Load the BIOMOD.models.out object, and store the loaded object in a specific variable (myBiomodModelOut)
  x = load(biomodModelPath)
  myBiomodModelOut <- get(x)
  # Remove the old object since you've stored it in y 
  rm(x)
  
  myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                          new.env = newData,
                                          proj.name = proj.name,
                                          selected.models = 'all',
                                          # models.eval.meth should be less than 3,  because of temporary raster data folder size.
                                          binary.meth = c('TSS', 'ROC', 'ACCURACY'),
                                          compress = FALSE,
                                          build.clamping.mask = FALSE)
  
  ##########################################################
  ## NOTE! MAKE SURE TEMPORARY RASTER FOLDER IS NOT FULL! ##
  ##########################################################
  
  # clear the folder 
  tempFilePath <- list.files(rasterOptions()$tmpdir, full.names = T)
  file.remove(tempFilePath)
  
}



#########################################################
## RUN projection
#########################################################

genus_name = "Acaena"

## keep working directory same, because all data needed for restoration is saved there.
setwd("Y://BIOMOD for Grid2")

# get folder names of target genus
folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]

#########################################################
## Takes a while to run... slow but wait!
#########################################################

for(i in folders){
  tryCatch(
    biomodProjection_forNewData(i, LGMdata,
                                modelname = "5km_15Jan19",
                                proj.name = "5kmLGM_15Jan19"),
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
}


