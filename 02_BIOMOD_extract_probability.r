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
                             proj.name # file location of ensamble model projection.out
) {
  
  files <- list.files(paste(".//", spname, "//proj_", proj.name, sep = ""), full.names = T)
  
  # Probability prediction
  if(sum(grepl("gri$", files)) == 1){
    proj <- (files  %>% grepl("grd$", .) %>% files[.] %>% raster)
  }
  
  # Binary prediction
  if(sum(grepl("gri$", files)) == 2){
    proj <- (files  %>% grepl("bin.grd$", .) %>% files[.] %>% raster)
    }else{
  # Omit species whose ENM failed.
    proj <- "NA"
  }
  
  return(proj)
}

pred <- lapply(folders, get_EMprojection, proj.name = "6Nov18_ensamble")
names(pred) <- folders

save(pred, file = paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = ""))