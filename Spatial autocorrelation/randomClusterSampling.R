#############################################################################################################
### Cluster sampling
#############################################################################################################
source(".//Range-fillings//Spatial autocorrelation//F_randomClusterSampling.R")
library(dplyr)
library(ggplot2)

genus_name <- "Acaena"
if(genus_name == "Nothofagus"){
  # Import species occurrence data
  scores.ep <- read.csv("Y://Nothofagus_in_nz.csv")
  spname <- colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))]
  
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
  
}

ran.ep <- repeat_ClusterSampling(spname, data1 = scores.ep, 
                                 iteration = 1000, coordinateNames = c("x","y"))
names(ran.ep) <- spname

#############################################################################################################  
### Significance test of EP valuse of species habitats based on the distribution of randomly sampled points
#############################################################################################################

# EP mean
mean.ep <- sapply(ran.ep, function(x){
  sapply(x, function(y) mean(y$EPcc))
}
)

hist(mean.ep[,4])
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


sp.occ <- list()
for(i in spname){
  sp.occ[[i]] <- sum(scores.ep[, i] == 1, na.rm = T)
}
names(sp.occ) <- spname


sp <- read.csv(paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))

spname <- sp$spname

## Significance test of EP valuse of species habitats based on the distribution of randomly sampled points

significance.test <- function(ep.data, measure){
  sig <- list()
for(i in spname){
  dat <- sp[sp$spname == i,]
  percentile <- quantile(ep.data[,i], probs = c(2.5/100, 1 - 2.5/100))
  
  sig[[i]] <- "NA"
  if(dat[, measure] <= percentile[1]){
    sig[[i]] <- "lower"
  } 
   
  if(dat[, measure] >= percentile[2]){
    sig[[i]] <- "higher"
  } 
  
}
  return(sig)
}

sig <- cbind(unlist(sp.occ), 
             unlist(significance.test(prop.ep, "epcccl.prop")), 
             unlist(significance.test(range.ep, "ep.range")), 
             unlist(significance.test(mean.ep, "ep.mean"))
) %>% as.data.frame
colnames(sig) <- c("Range.size", "EPcccl.prop", "EPcc.range", "EPcc.mean")

write.csv(sig, file = paste("Y://", genus_name, "_significant_EP.csv", sep=""))

#################################################################################################################
### Draw plots; EP vs. species range
#################################################################################################################
data2 <- create_dataframe_EP_error(ran.ep)

dat <- read.csv(paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))

dat$spname <- gsub("_", ".", dat$spname)

data2$spname <- gsub("_", ".", data2$spname)

dat2 <- merge(dat, data2, by = "spname")

if(genus_name=="Nothofagus"){
  dat2 <- cbind(unlist(sp.occ), dat2)
  colnames(dat2)[1] <- "sp.occ"
}else{
  colnames(dat2)[colnames(dat2)=="c.unlist.sp.occ.."] <- "sp.occ"
}


# Default line plot
p <- ggplot(dat2, aes(x = sp.occ, y = ep.range)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.range, ymax = error.max.range), width = .2, col = "red",
                position = position_dodge(0.05)
                )+
  geom_errorbar(aes(ymin = error.2.5.range, ymax = error.97.5.range), width = .2, col = "yellow",
                position = position_dodge(0.05)
  )

png(paste("Y://", genus_name, "_eprange_cluster.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP range", x = "Species range") +
  theme_classic()
dev.off()

# Default line plot
p <- ggplot(dat2, aes(x = sp.occ, y = ep.mean)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = error.min.mean, ymax = error.max.mean), width = .2, col = "red",
                position=position_dodge(0.05)
                ) +
  geom_errorbar(aes(ymin = error.2.5.mean, ymax = error.97.5.mean), width = .2, col = "yellow",
                position = position_dodge(0.05)
  )

png(paste("Y://", genus_name, "_epmean_cluster.png", sep = ""))
# Finished line plot
p + labs(title = genus_name, y = "EP mean", x = "Species range") +
  theme_classic()
dev.off()


# Default line plot
p <- ggplot(dat2, aes(x = sp.occ, y = epcccl.prop)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.prop, ymax = error.max.prop), col = "red") +
  geom_errorbar(aes(ymin = error.2.5.prop, ymax = error.97.5.prop), width = .2, col = "yellow",
                position = position_dodge(0.05)
  )


png(paste("Y://", genus_name, "speciesRange_Prop_potiveEPcc_cl_cluster.png", sep = ""))
# Finished line plot
p + labs(title = genus_name, y = "Proportion of positive EPcc-cl", x = "Species range") +
  theme_classic()
dev.off()
