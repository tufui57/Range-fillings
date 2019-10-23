
#################################################################################################################
### Generate virtual species distribution
#################################################################################################################

library(rgdal)
library(raster)
library(dplyr)

##################################################################################################################
########################        Generate climate raster stack
##################################################################################################################


##################################################################################################################
########################        5km grid data import
##################################################################################################################

datapath <- "Y://Acaena_bioclim_landcover_history_worldclim1_5km.csv"


### Import raster of climate data
# Reference raster for coordinate system. This raster must have the same dimentions as the raster of occurrence data
ref.raster <- raster(
  paste("Y://GIS map and Climate data//newzealandpotentialvegetatio5.bil", sep="")
)
proj4stringNZTM <- proj4string(ref.raster)
# Worldclim ver.1.4
path <-"Y:\\GIS map and Climate data\\worldclim\\bio_411"
source(".\\GitHub\\functions\\F01_project_resample_WORLDCLIM.R")



# Load EPcc
load(".//EPcc_NZ_4var_test.data")
epcc <- load(".//EPcc_NZ_4var_test.data")
epcc <- get(epcc)
colnames(epcc)[ncol(epcc)] <- "EPcc"

# Load EPcl
load(".//EPcl_NZ_4var.data")
epcl <- load(".//EPcl_NZ_4var.data")
epcl <- get(epcl)
colnames(epcl)[ncol(epcl)] <- "EPcl"

scores.ep <- merge(scores, epcc[, c("x", "y", "EPcc")], by = c("x","y")) %>% 
  merge(., epcl[, c("x","y","EPcl")], by = c("x","y"))

#################################################################################################################
### Generate virtual species distribution
#################################################################################################################

library(virtualspecies)

test <- generateRandomSp(stack(bioNZ))

test2 <- generateRandomSp(stack(bioNZ))

test3 <- generateRandomSp(stack(bioNZ), niche.breadth = "narrow")
test4 <- generateRandomSp(stack(bioNZ), niche.breadth = "narrow")


test.s <-sampleOccurrences(test$pa.raster, 1000)
test2.s <-sampleOccurrences(test2$pa.raster, 1000)
test3.s <- sampleOccurrences(test3$pa.raster, 100)
test4.s <- sampleOccurrences(test4$pa.raster, 100)



test.s.ep <- merge(test.s$sample.points, scores.ep, by = c("x", "y"))



### Repeat randomization

### the following doesn't work when random suitatbility has too low values. they can't have >500 occurrences.
test.s.ep  <- list()
for(i in 1:100){
  
  test <- generateRandomSp(stack(bioNZ))
  test.s <-sampleOccurrences(test$pa.raster, 500)
  test.s.ep[[i]] <- merge(test.s$sample.points, scores.ep, by = c("x", "y"))

}

### Try generating distributoions with niche volumes on PCA
generateSpFromPCA()

