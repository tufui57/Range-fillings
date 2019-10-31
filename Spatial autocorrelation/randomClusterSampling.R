#############################################################################################################
### Cluster sampling
#############################################################################################################
source(".//Range-fillings//Spatial autocorrelation//F_randomClusterSampling.R")
library(dplyr)

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

ran.ep <- repeat_ClusterSampling(spname, data1 = scores.ep, 
                                 iteration = 1000, coordinateNames = c("x","y"))
# EP mean
mean.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y) mean(y$EPcc))
}
)

hist(mean.ep[,10])

# EP range
range.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y){
    max(y$EPcc) - min(y$EPcc)
    })
}
)

# Proportion of positive EPcc-cl
prop.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y){
   epcccl <- (y$EPcc - y$EPcl)
   epcccl.prop <- sum(epcccl > 0) / nrow(y)
   return(epcccl.prop)
  })
}
)


