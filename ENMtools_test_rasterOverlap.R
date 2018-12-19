library(ENMTools)
source(".//functions//F02_create_raster_from_dataframe.R")

###################################################################################################
### Range similarity
###################################################################################################
genus_name <-  "Acaena"

datname <- load(paste("Y://ensemblePrediction_", genus_name, "12Dec.data", sep = ""))
prob <- get(datname)
# Species name list
spname <- names(prob)

# datname <- load(paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = ""))
# bin <- get(datname)

ref <- raster(
  # This raster was converted from pre-human raster with ArcGIS using "majority" method for raster value assignment
  paste("Y://GIS map and Climate data//newzealandpotentialvegetatio1.bil", sep="")
)

genus_name = "Acaena"
dat <- read.csv("Y://Acaena_bioclim_landcover_history_worldclim1_1km.csv")
# Change name to be consistent with prediction data
colnames(dat)[grepl(paste("^", genus_name, sep=""), colnames(dat))] <- colnames(dat)[grepl(paste("^", genus_name, sep=""), colnames(dat))] %>% gsub("_", ".", .)

### Calculate range overlap
res <- list()
for(i in spname){
  ras <- convert_dataframe_to_raster(ref, dat, c("x","y"), i)
  
  # Arguments are required as raster whose values are probability
  res[[i]] <- raster.overlap(prob[[i]], ras)
  
}

D <- data.frame(sapply(res, "[[", 1))

write.csv(D, "Y://enmtools_rangeOverlap.csv")

###################################################################################################
### Environmental similarity
###################################################################################################

# Arguments are required as raster whose values are probability
env.overlap(prob[[1]], ras, env, tolerance = .001)

