############################################################################################################
# Get raster data for Nothofagus menziesii
############################################################################################################

library(raster)
library(rgdal)
library(SDMTools)
library(dplyr)


setwd("C:\\Users\\nomur\\Documents")

notho <- read.csv("Y:\\Global phylogentic niche conservation\\meta data\\sp occurrence data\\Nothofagus_gbif.csv")

# Extract Nothofagus in NZ
nothofagus_nz <- notho[notho$species %in%  
                  c("Nothofagus menziesii","Nothofagus truncata", "Nothofagus fusca",
                    "Nothofagus solandri" # Black beech or montain beech
                    ),]

for(i in c("Nothofagus menziesii","Nothofagus truncata", "Nothofagus fusca", "Nothofagus solandri")
    ){
  
  nothofagus_nz[, i] <- ifelse(nothofagus_nz$species == i, 1, 0)
  
}
  
############################################################################################################
# Resample species occurrence raster on 1km to 5km resolution 
############################################################################################################

pre <- raster("Y://GIS map and Climate data//newzealandpotentialvegetatio5.bil")

# Import reference raster of WGS
refWGS <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411\\bio1_411.bil")

nothofagus_nz <- nothofagus_nz[!is.na(nothofagus_nz$decimallatitude),]

coordinateNames = c("decimallongitude", "decimallatitude")

# Set coordinates
points <- nothofagus_nz[, coordinateNames]
coordinates(points) <- nothofagus_nz[, coordinateNames]

ras2 <- list()
for(i in c("Nothofagus menziesii","Nothofagus truncata", "Nothofagus fusca", "Nothofagus solandri")
){
  
  # Put values in point object
  points$sp <-  nothofagus_nz[, i]
  proj4string(points) <- proj4string(refWGS)
  
  points_nz <- crop(points, extent(refWGS))
  
  # Rasterize points of WGS84 and 1 km grid resolution
  sp_raster <- rasterize(points_nz, refWGS, fun = mean, field = "sp")
  
  # Project and resample raster to NZTM and 5 km grid resolution
  ras2[[i]] <- projectRaster(sp_raster, pre)
  
  
}

############################################################################################################
# Convert raster to dataframe
############################################################################################################
lapply(ras2, values)

d5km <- cbind(coordinates(ras2[[1]]), sapply(ras2, values)) %>% as.data.frame


#############################################################################################################
### Prepare EP
#############################################################################################################

genus_name <- "Chionochloa"
# Import species occurrence data
load(paste(".//Scores_", genus_name, "_landcover_worldclim1_5km.data", sep = ""))

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

# Merge nothofagus data to EP data
notho5km <- merge(d5km, scores.ep, by=c("x","y"))

write.csv(notho5km, file = "Y://Nothofagus_in_nz.csv")

##################################
## Map
##################################
ref.extent <- raster("Y:\\GIS map and Climate data\\worldclim\\bio_411nztm\\bio1_411.bil")

# Import current outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of current polygon to decrease data size
nzland2 <- crop(nzland, ref.extent)

plot(nzland2)
plot(ras2[[3]], add = TRUE)


spname<-c("Nothofagus menziesii","Nothofagus truncata", "Nothofagus fusca", "Nothofagus solandri")

##################################
## Calculate EP meansures
##################################
ep.sp <- list()

for(i in spname){
  notho2 <- notho5km[!is.na(notho5km[,i]), ]
  ep.sp[[i]] <- notho2[notho2[, i] == 1, "EPcc"]
}
names(ep.sp)<- spname

## EPcc range over species occurrence records
ep.range <- lapply(spname, function(x){
  max(ep.sp[[x]]) - min(ep.sp[[x]])
})


epcccl.sp <- list()

for(i in spname){
  notho2 <- notho5km[!is.na(notho5km[,i]), ]
  epcccl.sp[[i]] <- notho2[notho2[, i] == 1, c("EPcc", "EPcl")]
}
names(epcccl.sp) <- spname

# Proportions of areas with species occurrence records and positive EPcc-cl 
epcccl.prop <- list()
for(i in 1:length(epcccl.sp)){
  res <- epcccl.sp[[i]][, "EPcc"] - epcccl.sp[[i]][, "EPcl"]
  epcccl.prop[[i]] <- sum(res > 0) / nrow(epcccl.sp[[i]])
  
}

sp.occ <- list()
for(i in spname){
  notho2 <- notho5km[!is.na(notho5km[,i]), ]
  sp.occ[[i]] <- sum(notho2[, i] > 0)
}
names(sp.occ)<- spname


ep.range2 <- cbind(unlist(ep.range), sapply(ep.sp, median), sapply(ep.sp, mean), unlist(epcccl.prop), unlist(sp.occ)) %>% as.data.frame
colnames(ep.range2) <- c("ep.range", "ep.median","ep.mean", "epcccl.prop", "range size")
ep.range2$spname <- rownames(ep.range2)

write.csv(ep.range2, "Y://Nothofagus5km_EP.csv")
