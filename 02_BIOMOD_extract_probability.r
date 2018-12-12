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
                             proj.name # file location of ensamble model projection.out
) {
  
  files <- list.files(paste(".//", spname, "//proj_", proj.name, sep = ""), full.names = T)
  
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
  
  return(proj)
}

# A. rorida has no model due to the small sample size
pred <- lapply(folders[-17], get_EMprojection, binary = FALSE, proj.name = "7Nov18_ensamble")
names(pred) <- folders[-17]

save(pred, file = paste("Y://ensemblePrediction_", genus_name, "12Dec.data", sep = ""))
