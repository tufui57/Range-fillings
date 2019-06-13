#################################################################################################################
###   Ensemble model projection BINARY or PROBABILITY    
#################################################################################################################
### Use this script, if you want to change output style of projection from the one in "BIOMOD 5km resolution.R".
### This script is based on "BIOMOD 5km resolution.R".

library(dplyr)

genus_name = "Acaena"

## PLOTS THE PROJECTIONS
setwd("Y:\\BIOMOD for Grid2")
# Get folder names
folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]

ensembleProjection <- function(spname, # species name
                               BIOMODproj.name,
                               ensambleModel.name
) {

  # Load BIOMOD.projection.output
  modelname <- 
    load(paste(".\\", spname, "\\proj_", BIOMODproj.name, "\\", spname, ".", BIOMODproj.name, ".projection.out",
               sep = "")
    )
  projModel <- get(modelname)
  
  # Load BIOMOD.ensemble.model.out
  EMmod <- load(paste(".\\", spname, "\\", spname, ".", ensambleModel.name, "ensemble.models.out",
                    sep = "")
  )
  myBiomodEM <- get(EMmod)
  
  
  # Creating the ensemble projections
  # Nothing is returned by this function, but specific projection files () are saved on the hard drive projection folder. 
  BIOMOD_EnsembleForecasting(projection.output = projModel,
                             EM.output = myBiomodEM,
                             # Threshold to return presence/absence. 
                             # binary.meth = NULL returns only probability, but if binary.meth is specified both presence/absence and probability are returned.
                             binary.meth = NULL
  )
  
  

}

## Plot ensemble projection

EMprojectionPlot <- function(spname, # species name
                             BIOMODproj.name, # file location of ensamble model projection.out
                             binary # logical;TRUE = output binary projection, FALSE = output probability projection
) {
  # load projection data
  files <- list.files(paste(".//", spname, "//proj_", BIOMODproj.name, sep = ""), full.names = T)
  
  # If binary.meth in BIOMOD_EnsembleForecasting() was specified, you got two grd files.
  proj <- lapply((files  %>% grepl("grd$", .) %>% files[.]), raster)
  
  if(binary){
    proj <- proj[[2]]
  }else{
    proj <- proj[[1]]
  }
  
  png(filename = paste(".//ensemble_projection//", names(proj), "_", BIOMODproj.name, "_",
                       ifelse(binary, "TSS", "prob"),
                       ".png", sep = ""),
      height = 900, width = 750, units = "px"
  )
  plot(proj)
  title(names(proj))
  dev.off()
  
}


# If there are species whose BIOMOD failed and no grd file was generated, you don't get the prediction.
for(i in folders){
  tryCatch(
    {ensembleProjection(i,
                       BIOMODproj.name = "5kmLGM_15Jan19", # "SAIdiff_4Mar19"
                       ensambleModel.name="5km_15Jan19" #  "SAIdiff_4Mar19_ensamble"
                       )
    
    # If there are species whose BIOMOD failed and no grd file was generated, you don't get the plot.
    EMprojectionPlot(i, BIOMODproj.name = "5kmLGM_15Jan19", binary = FALSE)
      },
    
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
}
