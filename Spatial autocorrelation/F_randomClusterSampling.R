#############################################################################################################
### Cluster sampling
#############################################################################################################
### Random sampling from the same sized square as the observed sepcies occurrences.

clustersampling <- function(dat, # an observed species distribution
                            region, # data of whole region
                            coordinateNames
  
){
  region$id <- 1:nrow(region)
  
  # Measure the size of square of the observed sepcies occurrences
  range.x <- max(dat[, coordinateNames[1]]) - min(dat[, coordinateNames[1]])
  range.y <- max(dat[, coordinateNames[2]]) - min(dat[, coordinateNames[2]])
  
  # Choose the first sample
  first.sample <- region[sample(1:nrow(region), 1),]
  
  # The neighbourhood square of the first sample
  dat2 <- region[(region[, coordinateNames[1]] <= (first.sample[, coordinateNames[1]] + range.x)), ]
  neighbour.x <- dat2[(dat2[, coordinateNames[1]] >= (first.sample[, coordinateNames[1]] - range.x)), ]
  dat3 <- neighbour.x[(neighbour.x[, coordinateNames[2]] <= (first.sample[, coordinateNames[2]] + range.y)), ]
  neighbour.xy <- dat3[(dat3[, coordinateNames[2]] >= (first.sample[, coordinateNames[2]] - range.y)), ]
  
  
  # Sample the rest of points from the neighbourhood square of the first sample
  samples <- region[sample(neighbour.xy$id, (nrow(dat) - 1)),]
  return(samples)
}


repeat_ClusterSampling <- function(spname,
                           data1, # data with climate values 
                           data2 = NULL, # predicted presence data. Get samples from this data
                           iteration,
                           coordinateNames
){
  ran.ep <- list()
  
  for(i in 1:length(spname)){
    
    ### If data2 isn't given, just sample randomly from data1
    if (is.null(data2) == FALSE){
      sample.area <- merge(data1, data2[[i]], by = coordinateNames)
    }else{
      sample.area <- data1
    }
    
    # Remove NA values from species occurrence records
    data1 <- data1[!is.na(data1[, spname[i]]), ]
    
    samples <- list()
    
    for(j in 1:iteration){
      
      samples[[j]] <- clustersampling(dat = data1[data1[, spname[i]] == 1, ], 
                                      region = sample.area, coordinateNames = coordinateNames)
      
    }
    ran.ep[[i]] <- samples
  }
  
  return(ran.ep)
  
}
