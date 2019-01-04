####################################################
### Niche filling calculation
####################################################
library(raster)
library(dplyr)

# Import function
source(".//functions//F04_convert_occurrencePoints_to_raster.R")

genus_name <- "Chionochloa"

# Reference raster
ref.raster <- raster("Y://GIS map and Climate data//newzealandpotentialvegetatio1.bil")
# Import PCA scores
load(paste(".\\Scores_", genus_name, "_landcover.data", sep = ""))

####################################################
### Create climate space raster 
####################################################

# Create raster whose resolution is the same as reference raster but coordinate system
# So that I can rasterize climate points which has no geographycal coordinate system
xy <- matrix(rep(1, nrow(ref.raster)*ncol(ref.raster)), nrow = nrow(ref.raster), ncol = ncol(ref.raster))
climate.ras <- raster(xy)
# Extent
extent(climate.ras) <- extent(
  min(scores$PC1),
  max(scores$PC1),
  min(scores$PC2),
  max(scores$PC2)
)
  

# Set coordinates
points <- scores[, c("PC1", "PC2")]
coordinates(points) <- scores[, c("PC1", "PC2")]

# Put values in point object
points$occ <- rep(1, nrow(scores))

# Project points. Original coordinate system of BIOCLIM variables is WGS84
climate.ref.ras <- rasterize(points, climate.ras, field = 1)

# Species name
spname <- grepl(genus_name, colnames(scores)) %>% colnames(scores)[.]

sp <- list()
for(i in spname){
  sp[[i]] <- scores[scores[, i] == 1,]
}

# Function to rasterize cliamte niche
rasterize_niche <- function(data, # data frame of occurrence data
         ref, # reference raster for coordinate system
         occ # TRUE; If "data" is the data frame whose nrow = the number of occurrence records
             # FALSE; If "data" is the data frame with predicted probability
){
  
  # Set coordinates
  points <- data[, c("PC1", "PC2")]
  coordinates(points) <- data[, c("PC1", "PC2")]
  
  # If "data" is the data frame whose nrow = the number of occurrence records
  if(occ == TRUE){
    # Put values in point object
    points$occ <- rep(1, nrow(data))
    
  }else{
    # If "data" is the data frame with predicted probability
    points$occ <- data[, "prob"]
  }

  # Rasterize point object
  sp_raster <- rasterize(points, ref, field = points@data[,"occ"], fun = mean)
 
  return(sp_raster)
}

####################################################
# Rasterize realized cliamte niche
####################################################

niche.ras <- lapply(sp, rasterize_niche, ref = climate.ras, occ = T)

# The number of occurrence cells on climate space
sapply(niche.ras, function(d){
  sum(values(d), na.rm = T)
  })
# The number of occurrence cells on geographycal space
sapply(sp, nrow)

####################################################
# Rasterize predicted cliamte niche
####################################################
# Load ensamble projection data
load(paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = ""))

# Change name to be consistent with prediction data
colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))] <- 
  colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))] %>% gsub("_", ".", .)
# Get species name list
spname <- names(pred)

# Get rid of 
pred2 <- pred[lapply(pred, class) == "RasterLayer"]

ras <- list()

for(i in spname){
  
  if(i %in% names(pred2)){
    
    # Prepare prediction data
    data2 = cbind(coordinates(pred2[[i]]), values(pred2[[i]]))
    colnames(data2)[3] <- "prob"
    
    # If you use probability data for overlap calculation, 
    if(any(data2[!is.na(data2[, "prob"]), "prob"] > 2)){
      # Probability should be dicimal
      data2[,3] <- data2[,3] / 1000
    }
    
    # Bind PC scores to prediction data by coodinates
    data3 <- merge(scores, data2, by = c("x", "y")) 
    
    data3 <- data3[,c("PC1","PC2","prob")]
    
    # Calculate niche overlap
    ras[[i]] <- rasterize_niche(data3, ref = climate.ras, occ = F)
  }else{
    
    print("No data")
    ras[[i]] <- "NA"
  }
  
}

####################################################
# Calculate niche filling
####################################################

# Change species name separator
names(niche.ras) <- names(niche.ras) %>% gsub("_", ".", .)

# Niche filling
nichefilling <- sapply(niche.ras[names(pred2)], function(d){
  sum(values(d), na.rm = T)
}) / sapply(ras[names(pred2)], function(d){
  sum(values(d), na.rm = T)
})

# Save the niche filling
write.csv(nichefilling, file = paste("NicheFilling_", genus_name, ".csv", sep=""))
