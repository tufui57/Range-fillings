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
                                 iteration = 500, coordinateNames = c("x","y"))
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

