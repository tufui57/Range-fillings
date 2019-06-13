############################################################################################################
############################   Get probability from BIOMOD ensemble models
############################################################################################################

setwd("Y://BIOMOD for Grid2")
library(dplyr)
library(raster)

genus_name <- "Acaena"

## Plot ensemble projection
folders <- list.dirs("Y://BIOMOD for Grid2//", full.names = FALSE, recursive = F)
folders <- (grepl(genus_name, folders) %>% folders[.])


get_EMprojection <- function(spname, # species name
                             binary = TRUE, # FALSE; return probabiliry, TRUE; return binary prediction
                             ensambleProj.name # file location of ensamble model projection.out
) {
  
  files <- list.files(paste(".//", spname, "//proj_", ensambleProj.name, sep = ""), full.names = T)
  
  if(length(files) == 0){
    print("No model built")
    proj <- "NA"
  }else{
      # Probability prediction
  if(binary == FALSE){
    if(sum(grepl("gri$", files)) == 1){
    proj <- (files %>% grepl("grd$", .) %>% files[.] %>% raster)
    }else{
      # If the ensemble model prediction has given both of probability and binary, there are two .grd files
      proj <- (files %>% grepl("ensemble.grd$", .) %>% files[.] %>% raster)
  }
  
  }
  
  
  # Binary prediction
  if(binary == TRUE){
    if(sum(grepl("gri$", files)) == 2){
    proj <- (files  %>% grepl("bin.grd$", .) %>% files[.] %>% raster)
    }else{
  # Omit species whose ENM failed.
    proj <- "NA"
    }
  }
  }
  
  return(proj)
}

# A. rorida has no model due to the small sample size

### Binary prediction needed for analyses
pred <- lapply(folders, get_EMprojection, binary = TRUE, ensambleProj.name = "SAIdiff_4Mar19_ensamble")
names(pred) <- folders
ensambleProj.name = "SAIdiff_4Mar19_ensamble"
binary = "binary"

save(pred, file = paste("Y://ensemblePredictionBinary_", genus_name, ensambleProj.name, binary,".data", sep = ""))

############################################################################################################
### Prediction maps 
############################################################################################################

### Prediction based on current climate
pred <- lapply(folders, get_EMprojection, binary = FALSE, ensambleProj.name = "SAIdiff_4Mar19_ensamble")
names(pred) <- folders
ensambleProj.name = "SAIdiff_4Mar19_ensamble"
binary = "prob"

save(pred, file = paste("Y://ensemblePredictionBinary_", genus_name, ensambleProj.name, binary,".data", sep = ""))

### Prediction based on LGM climate
predLGM <- lapply(folders, get_EMprojection, binary = FALSE, ensambleProj.name = "5kmLGM_15Jan19")
names(pred) <- folders
binary = "prob"

save(predLGM, file = paste("Y://ensemblePredictionBinary_", genus_name, "5kmLGM_15Jan19", binary,".data", sep = ""))

