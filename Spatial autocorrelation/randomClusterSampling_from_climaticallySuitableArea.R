#############################################################################################################
### Cluster sampling
#############################################################################################################
source(".//Range-fillings//Spatial autocorrelation//F_randomClusterSampling.R")
library(dplyr)
library(biomod2)

genus_name <- "Chionochloa"
if(genus_name == "Nothofagus"){
  # Import species occurrence data
  scores.ep <- read.csv("Y://Nothofagus_in_nz.csv")
  spname <- colnames(scores)[grepl(paste("^", genus_name, sep = ""), colnames(scores))]
  
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
  
  colnames(scores.ep) <- gsub("_",".", colnames(scores.ep))
  
  load(paste("Y://ensemblePredictionBinary_", genus_name, "5km_15Jan19_ensamblebinary.data", sep = ""))
  
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
}

ran.ep <- repeat_ClusterSampling(spname, data1 = scores.ep, data2 = a,
                                 iteration = 10, coordinateNames = c("x","y"))
# EP mean
mean.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y) mean(y$EPcc))
}
)

hist(mean.ep[,10])
colnames(mean.ep) <- spname


# EP range
range.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y){
    max(y$EPcc) - min(y$EPcc)
    })
}
)

colnames(range.ep) <- spname

# Proportion of positive EPcc-cl
prop.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y){
   epcccl <- (y$EPcc - y$EPcl)
   epcccl.prop <- sum(epcccl > 0) / nrow(y)
   return(epcccl.prop)
  })
}
)

colnames(prop.ep) <- spname

sp <- read.csv(paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))

spname <- sp$spname
  
### Significance test of EP valuse of species habitats based on the distribution of randomly sampled points

significance.test <- function(ep.data){
  sig <- list()
for(i in spname){
  dat <- sp[sp$spname == i,]
  percentile <- quantile(ep.data[,i], probs = c(2.5/100, 1 - 2.5/100))
  
  sig[[i]] <- "NA"
  if(dat$epcccl.prop <= percentile[1]){
    sig[[i]] <- "lower"
  } 
   
  if(dat$epcccl.prop >= percentile[2]){
    sig[[i]] <- "higher"
  } 
  
}
  return(sig)
}

sig <- cbind(unlist(significance.test(prop.ep)), unlist(significance.test(range.ep)),
             unlist(significance.test(mean.ep))
)
colnames(sig) <- c("EPcccl.prop","EPcc.range","EPcc.mean")

write.csv(sig, file = paste("Y://",genus_name,"_significant_EP.csv", sep=""))

