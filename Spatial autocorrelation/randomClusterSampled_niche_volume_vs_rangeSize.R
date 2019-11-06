
########################################################################################################
# Random cluster sampling for testing range size vs. niche volume (calcualted by ranges of annual temperature and precipitaion)
######################################################################################################
source(".//GitHub//Range-fillings//Spatial autocorrelation//F_randomClusterSampling.R")
library(dplyr)
library(raster)

### Random sampling from the same sized square as the observed sepcies occurrences.

clustersampling <- function(numberOfSamples,
                            clusterSize, # give range of coordinates (long and lat), e.g., c(1000, 1000) 
                            region, # data of whole region
                            coordinateNames,
                            allowMultipleSampling = FALSE
                            
){
  region$id <- 1:nrow(region)
  
  # The size of square of the observed sepcies occurrences
  range.x <- clusterSize[1]
  range.y <- clusterSize[2]
  
  # Choose the first sample
  first.sample <- region[sample(1:nrow(region), 1),]
  
  # The neighbourhood square of the first sample
  dat2 <- region[(region[, coordinateNames[1]] <= (first.sample[, coordinateNames[1]] + range.x)), ]
  neighbour.x <- dat2[(dat2[, coordinateNames[1]] >= (first.sample[, coordinateNames[1]] - range.x)), ]
  dat3 <- neighbour.x[(neighbour.x[, coordinateNames[2]] <= (first.sample[, coordinateNames[2]] + range.y)), ]
  neighbour.xy <- dat3[(dat3[, coordinateNames[2]] >= (first.sample[, coordinateNames[2]] - range.y)), ]
  
  # If the number of grid cells within the neighbourhood is not enough for the sampling, sample the first cell again.
  if(allowMultipleSampling == FALSE && nrow(neighbour.xy) < numberOfSamples){
    
    print("Sampling restarts, because the number of grid cells within the neighbourhood is not enough.")
    
    # Choose the first sample
    first.sample <- region[sample(1:nrow(region), 1, replace = allowMultipleSampling),]
    
    # The neighbourhood square of the first sample
    dat2 <- region[(region[, coordinateNames[1]] <= (first.sample[, coordinateNames[1]] + range.x)), ]
    neighbour.x <- dat2[(dat2[, coordinateNames[1]] >= (first.sample[, coordinateNames[1]] - range.x)), ]
    dat3 <- neighbour.x[(neighbour.x[, coordinateNames[2]] <= (first.sample[, coordinateNames[2]] + range.y)), ]
    neighbour.xy <- dat3[(dat3[, coordinateNames[2]] >= (first.sample[, coordinateNames[2]] - range.y)), ]
    
  }
  # Sample the rest of points from the neighbourhood square of the first sample
  samples <- region[sample(neighbour.xy$id, (numberOfSamples - 1), replace = allowMultipleSampling), ]
  return(samples)
}

# Load climate data
load("Y://Scores_Chionochloa_landcover5km.data")

## Repeat random cluster sampling
samples <- list()

for(j in c(100000, 300000)){
  cluster.samples <- list()
  
  for(i in seq(10, 1500, 10)){
    samples[[i]] <- clustersampling(numberOfSamples = i, clusterSize = c(j,j), 
                                  region = scores, coordinateNames = c("x","y"),
                                  allowMultipleSampling = T)
  }
  cluster.samples[[j]] <- samples
  
}

random.niche.volume <- 
  lapply(cluster.samples[[1]], nichePlot::SchoenerD_ecospat, scores.ep, "PC1", "PC2", scores.ep)

### Plot
png("Y://Climate envelope volumes of Randomly sampled areas.png")
plot(seq(10, 1500, 10), 
     sapply(random.niche.volume, function(x){
       x[[1]][1]
     }
     ),
     main = "Climate envelope volumes of Randomly sampled areas",
     xlab = "number of samples",
     ylab = "Climate envelope volume"
)
dev.off()



