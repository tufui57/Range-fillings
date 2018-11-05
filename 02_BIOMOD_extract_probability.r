############################################################################################################
############################   Get probability from BIOMOD ensemble models
############################################################################################################

setwd("Y://BIOMOD for Grid2")
library(dplyr)
library(raster)

genus_name <- "Chionochloa"

## Plot ensemble projection
folders <- list.dirs("Y://BIOMOD for Grid2//", full.names = FALSE, recursive = F)
folders <- (grepl(genus_name, folders) %>% folders[.])


get_EMprojection <- function(spname, # species name
                             proj.name # file location of ensamble model projection.out
) {
  # Omit species whose ENM failed.
  files <- list.files(paste(".//", spname, "//proj_", proj.name, sep = ""), full.names = T)
  
  if(sum(grepl("gri$", files)) == 1){
    proj <- (files  %>% grepl("grd$", .) %>% files[.] %>% raster)
  }else{
    proj <- "NA"
  }
  
  return(proj)
}

pred <- lapply(folders, get_EMprojection, proj.name = "18Oct18_ensamble")
names(pred) <- folders

save(pred, file = paste("Y://ensemblePrediction_", genus_name, ".data", sep = ""))
