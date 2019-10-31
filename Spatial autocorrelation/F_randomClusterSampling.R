#############################################################################################################
### Ckuster sampling
#############################################################################################################
### Random sampling from the same sized square as the observed sepcies occurrences.

clustersampling <- function(data1, # an observed species distribution
                            region, # data of whole region
                            coordinateNames
  
){
  region$id <- 1:nrow(region)
  
  # Measure the size of square of the observed sepcies occurrences
  range.x <- max(data1[, coordinateNames[1]]) - min(data1[, coordinateNames[1]])
  range.y <- max(data1[, coordinateNames[2]]) - min(data1[, coordinateNames[2]])
  
  # Choose the first sample
  first.sample <- region[sample(1:nrow(region), 1),]
  
  # The neighbourhood square of the first sample
  dat2 <- region[(region[, coordinateNames[1]] <= (first.sample[, coordinateNames[1]] + range.x)), ]
  neighbour.x <- dat2[(dat2[, coordinateNames[1]] >= (first.sample[, coordinateNames[1]] - range.x)), ]
  dat3 <- neighbour.x[(neighbour.x[, coordinateNames[2]] <= (first.sample[, coordinateNames[2]] + range.y)), ]
  neighbour.xy <- dat3[(dat3[, coordinateNames[2]] >= (first.sample[, coordinateNames[2]] - range.y)), ]
  
  
  # Sample the rest of points from the neighbourhood square of the first sample
  samples <- region[sample(neighbour.xy$id, (nrow(data1) - 1)),]
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
    
    samples <- list()
    
    for(j in 1:iteration){
      
      samples[[j]] <- clustersampling(data1[data1[, spname[i]] == 1, ], sample.area, coordinateNames)
      
    }
    ran.ep[[i]] <- samples
  }
  
  return(ran.ep)
  
}



genus_name <- "Chionochloa"
# Import species occurrence data
load(paste("Y://Scores_", genus_name, "_landcover5km.data", sep = ""))

# Load EPcc
load("Y://EPcc_NZ_4var_test.data")
epcc <- load("Y://EPcc_NZ_4var_test.data")
epcc <- get(epcc)
colnames(epcc)[ncol(epcc)] <- "EPcc"

# Load EPcl
load("Y://EPcl_NZ_4var.data")
epcl <- load("Y://EPcl_NZ_4var.data")
epcl <- get(epcl)
colnames(epcl)[ncol(epcl)] <- "EPcl"

scores.ep <- merge(scores, epcc[, c("x", "y", "EPcc")], by = c("x","y")) %>% 
  merge(., epcl[, c("x","y","EPcl")], by = c("x","y"))
spname <- colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))]


test <- repeat_ClusterSampling(spname, scores.ep, 
                       valueName = "EPcc", iteration = 10, coordinateNames = c("x","y"))

lapply(ran.ep, lapply, mean)