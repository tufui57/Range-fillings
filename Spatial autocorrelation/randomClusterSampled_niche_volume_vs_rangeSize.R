
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
  neighbour.x <- dat2[(dat2[, coordinateNames[1]] >= (first.sample[, coordinateNames[1]])), ]
  dat3 <- neighbour.x[(neighbour.x[, coordinateNames[2]] <= (first.sample[, coordinateNames[2]] + range.y)), ]
  neighbour.xy <- dat3[(dat3[, coordinateNames[2]] >= (first.sample[, coordinateNames[2]])), ]
  
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

# 100km x 100km cluster
random.niche.volume <- list()

for(i in seq(10, 1700, 10)){
  samples <- clustersampling(numberOfSamples = i, clusterSize = c(100000, 100000), 
                             region = scores, coordinateNames = c("x","y"),
                             allowMultipleSampling = T)
  random.niche.volume[[i]] <- nichePlot::SchoenerD_ecospat(scores, "PC1", "PC2", scores, samples)
  
}

# 300km x 300km cluster
random.niche.volume300 <- list()

for(i in seq(10, 1700, 10)){
  samples <- clustersampling(numberOfSamples = i, clusterSize = c(300000, 300000), 
                             region = scores, coordinateNames = c("x","y"),
                             allowMultipleSampling = T)
  random.niche.volume300[[i]] <- nichePlot::SchoenerD_ecospat(scores, "PC1", "PC2", scores, samples)
  
}

# no limited cluster
random.niche.volumeNZ <- list()

for(i in seq(10, 1700, 10)){
  
  ran.row <- sample(1:nrow(scores), i)
  ran.sam <- scores[ran.row, ]
  random.niche.volumeNZ[[i]] <- nichePlot::SchoenerD_ecospat(scores, "PC1", "PC2", scores, ran.sam) 
  
}

randoms <- c(random.niche.volume, random.niche.volume300, random.niche.volumeNZ)
save(randoms, file = "randomClusterSamples.data")


load("randomClusterSamples.data")
random.niche.volume <- randoms[1:1700]
random.niche.volume300 <- randoms[1701:3400]
random.niche.volumeNZ <- randoms[3401:5100]

### Climatic niche volume of species

aca <- read.csv("Y://AcaenaEPclimatedata.csv")
chi <- read.csv("Y://ChionochloaEPclimatedata.csv")
notho <- read.csv("Y://NothofagusEPclimatedata.csv")

### Plot
# png("Y://Climate envelope volumes of Random cluster samples.png", width = 700, height = 450)
# 
# plot(seq(10, 1700, 10), 
#      sapply(seq(10, 1700, 10), function(x){
#        random.niche.volumeNZ[[x]][[1]][1]
#      }
#      ),
#      ylim = c(0,1),
#      main = "Climate envelope volumes of random cluster samples",
#      xlab = "Number of samples",
#      ylab = "Climate envelope volume", cex=0.5
# )
# points(seq(10, 1700, 10), 
#        sapply(seq(10, 1700, 10), function(x){
#          random.niche.volume300[[x]][[1]][1]
#        }
#        ), col = "red", pch = 5, cex=0.5
#        )
# points(seq(10, 1700, 10), 
#        sapply(seq(10, 1700, 10), function(x){
#          random.niche.volume[[x]][[1]][1]
#        }
#        ), col = "blue", pch = 2, cex=0.5
# )
# 
# # Add sepcies niche volume
# points(aca$sp.occ, aca$niche_volume, col = "green", pch = 16
# )
# 
# points(chi$sp.occ, chi$niche_volume, col = "brown", pch = 16
# )
# 
# points(notho$sp.occ, notho$niche_volume, col = "purple", pch = 16
# )
# 
# # Add a legend
# legend(1, 1, legend=c("NZ", "300km", "100km"),
#        col = c("black", "red", "blue"), pch = c(1,5,2),  bty="n")
# legend(170, 1, legend=c("Acaena", "Chionohcloa", "Nothofagus"),
#        col = c("green", "brown", "purple"), pch = c(16,16,16),  bty="n")
# 
# 
# dev.off()



### Plot
png("Y://Climate envelope volumes of Random cluster samples2.png", width = 400, height = 500)

par(mfrow=c(3,1), mar=c(3.1, 4.1, 0.6, 2.1), cex=0.9)

plot(seq(10, 1700, 10), 
     sapply(seq(10, 1700, 10), function(x){
       random.niche.volumeNZ[[x]][[1]][1]
     }
     ),
     ylim = c(0,1),
     xlab = "Number of samples",
     ylab = "Climate envelope volume", cex=0.5
)

# Add sepcies niche volume
points(aca$sp.occ, aca$niche_volume, col = "green", pch = 16
)

points(chi$sp.occ, chi$niche_volume, col = "brown", pch = 16
)

points(notho$sp.occ, notho$niche_volume, col = "purple", pch = 16
)

# Add a legend
legend(1, 1, legend="NZ",
       col = "black", pch = 1,  bty="n")


### Plot 300km
plot(seq(10, 1700, 10), 
     sapply(seq(10, 1700, 10), function(x){
       random.niche.volume300[[x]][[1]][1]
     }
     ),
     ylim = c(0,1),
     xlab = "Number of samples",
     ylab = "Climate envelope volume", cex=0.5
)


# Add sepcies niche volume
points(aca$sp.occ, aca$niche_volume, col = "green", pch = 16
)

points(chi$sp.occ, chi$niche_volume, col = "brown", pch = 16
)

points(notho$sp.occ, notho$niche_volume, col = "purple", pch = 16
)

# Add a legend
legend(1, 1, legend="300km",
col = "black", pch = 1,  bty="n")

### Plot 100km
plot(seq(10, 1700, 10), 
     sapply(seq(10, 1700, 10), function(x){
       random.niche.volume[[x]][[1]][1]
     }
     ),
     ylim = c(0,1),
     xlab = "Number of samples",
     ylab = "Climate envelope volume", cex=0.5
)

# Add sepcies niche volume
points(aca$sp.occ, aca$niche_volume, col = "green", pch = 16
)

points(chi$sp.occ, chi$niche_volume, col = "brown", pch = 16
)

points(notho$sp.occ, notho$niche_volume, col = "purple", pch = 16
)

# Add a legend
legend(1, 1, legend="100km",
       col = "black", pch = 1,  bty="n")

dev.off()