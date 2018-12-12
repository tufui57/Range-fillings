library(ENMTools)
source(".//functions//F02_create_raster_from_dataframe.R")

###################################################################################################
### Range similarity
###################################################################################################
datname <- load(paste("Y://ensemblePrediction_", genus_name, "12Dec.data", sep = ""))
prob <- get(datname)

# datname <- load(paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = ""))
# bin <- get(datname)

ref <- raster(
  # This raster was converted from pre-human raster with ArcGIS using "majority" method for raster value assignment
  paste("Y://GIS map and Climate data//newzealandpotentialvegetatio1.bil", sep="")
)

genus_name = "Acaena"
dat <- read.csv("Y://Acaena_bioclim_landcover_history_worldclim1_1km.csv")
spname <- colnames(dat)[grepl(paste("^", genus_name, sep=""), colnames(dat))]

ras <- convert_dataframe_to_raster(ref, dat, c("x","y"), spname[1])

# Arguments are required as raster whose values are probability
raster.overlap(prob[[1]], ras)


