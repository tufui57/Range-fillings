#############################################################################################################
### Cluster sampling
#############################################################################################################
source(".//Range-fillings//Spatial autocorrelation//F_randomClusterSampling.R")
library(dplyr)
library(raster)

genus_name <- "Nothofagus"
if(genus_name == "Nothofagus"){
  # Import species occurrence data
  scores.ep <- read.csv("Y://Nothofagus_in_nz.csv")
  spname <- colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))]
  
  scores.ep <- scores.ep[!is.na(scores.ep[, spname[1]]), ]
  
  load(paste("Y://ensemblePredictionBinary_", genus_name, "31Oct19_ensamblebinary.data", sep = ""))
  
}else{
  
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
  colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))] <- gsub("_",".",spname)
  
  load(paste("Y://ensemblePredictionBinary_", genus_name, "5km_15Jan19_ensamblebinary.data", sep = ""))
}

ran.ep <- repeat_ClusterSampling(spname, data1 = scores.ep, 
                                 iteration = 1000, coordinateNames = c("x","y"))
names(ran.ep) <- spname

save(ran.ep, file = paste("Y://", genus_name, "_randomClusterSamples.data", sep = ""))


#############################################################################################################
### Random sampling from climatically suitable areas
#############################################################################################################

a <- sapply(pred, function(x){
  class(x) == "RasterLayer"
} )

pred2 <- pred[a]

spname <- names(pred2)

a <- list()
for(i in 1:length(pred2)){
  
  test <- 
    tryCatch({
      cbind(coordinates(pred2[[i]]), values(pred2[[i]]))
    }, erorr = function(e) e
    )
  
  test2 <- as.data.frame(test)
  a[[i]] <- test2[!is.na(test2$V3),] %>% .[.$V3 == 1,]
}

spname <-  names(pred2)

### Sample EPcc-cl from predicted presence
random.ep <- repeat_randomSampling(spname, data1 = scores.ep, data2 = a, 
                                   iteration = 1000, coordinateNames = c("x","y")
)
names(random.ep) <- names(pred2)

save(random.ep, file = paste("Y://", genus_name, "_randomSamples_suitableArea.data", sep = ""))

