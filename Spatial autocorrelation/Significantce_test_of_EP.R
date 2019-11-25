
#############################################################################################################  
### Significance test of EP valuse of species habitats based on the distribution of randomly sampled points
#############################################################################################################
library(dplyr)


genus_name <- "Nothofagus"

if(genus_name == "Nothofagus"){
  # Import species occurrence data
  scores <- read.csv("Y://Nothofagus_in_nz.csv")
}else{
  load(paste("Y://Scores_", genus_name, "_landcover5km.data", sep = ""))
}

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

colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))] <-
  gsub("_", ".", colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))])


x <- load(paste("Y://", genus_name, "_randomClusterSamples.data", sep = ""))
ran.ep <- get(x)

names(ran.ep) <- gsub("_", ".", names(ran.ep))

#############################################################################################################
## Calculate mean and range of randomly sampled EP
#############################################################################################################

# EP mean
mean.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y) mean(y$EPcc))
}
)
colnames(mean.ep) <- names(ran.ep)

# EP range
range.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y){
    max(y$EPcc) - min(y$EPcc)
  })
}
)

colnames(range.ep) <- names(ran.ep)

# Proportion of positive EPcc-cl
prop.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y){
    epcccl <- (y$EPcc - y$EPcl)
    epcccl.prop <- sum(epcccl > 0) / nrow(y)
    return(epcccl.prop)
  })
}
)

colnames(prop.ep) <- names(ran.ep)


sp <- read.csv(paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))

sp$spname <- gsub("_", ".", sp$spname)

library(magrittr)

findIn = function(u, v)
{
  strsplit(u,' ') %>%
    unlist %>%
    sapply(grep, value=T, x=v) %>%
    unlist %>%
    unique
}

spname <- unique(c(findIn(sp$spname, names(ran.ep)), findIn(names(ran.ep), sp$spname)))

#############################################################################################################
## Significance test of EP valuse of species habitats based on the distribution of randomly sampled points
#############################################################################################################

significance.test <- function(ep.data, measure, # colname of the EP data
                              threshold = 5 # significance threshold. default is upper and lower 5%. 
                              ){
  sig <- list()
  for(i in spname){
    dat <- sp[sp$spname == i,]
    percentile <- quantile(ep.data[,i], probs = c(threshold/100, 1 - threshold/100))
    
    meanEP <- mean(ep.data)
    
    sig[[i]] <- ifelse(dat[, measure] < meanEP, "lower", "higher")
    
    if(dat[, measure] <= percentile[1]){
      sig[[i]] <- "lower*"
    } 
    
    if(dat[, measure] >= percentile[2]){
      sig[[i]] <- "higher*"
    } 
    
  }
  return(sig)
}

sig <- cbind(sp$sp.occ[sp$spname %in% spname], 
             unlist(significance.test(prop.ep, "epcccl.prop")), 
             unlist(significance.test(range.ep, "ep.range")), 
             unlist(significance.test(mean.ep, "ep.mean"))
) %>% as.data.frame
colnames(sig) <- c("Range.size", "EPcccl.prop", "EPcc.range", "EPcc.mean")

write.csv(sig, file = paste("Y://", genus_name, "_significant_EP_cluster5%.csv", sep=""))

### Proportion
for (i in c("EPcccl.prop", "EPcc.range", "EPcc.mean")) {
  print(i)
  print(summary(sig[, i]))
  print(summary(sig[, i]) / nrow(sig))
}

res <- lapply(c("EPcccl.prop", "EPcc.range", "EPcc.mean"), function(i){
  (summary(sig[, i]) / nrow(sig))
}) 
names(res) <- c("EPcccl.prop", "EPcc.range", "EPcc.mean")

write.csv(unlist(res), file = paste("Y://", genus_name, "_EPsummary_cluster5%.csv", sep=""))


rm(list = ls(all.names = TRUE))

