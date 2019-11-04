#############################################################################################################
### Random sampling from climatically suitable areas
#############################################################################################################
library(raster)
library(dplyr)
library(ggplot2)
source(".\\Range-fillings\\Spatial autocorrelation\\F_randomClusterSampling.R")

############################################################
# Import predicted presence
############################################################

genus_name <- "Chionochloa"
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

#############################################################################################################  
### Significance test of EP valuse of species habitats based on the distribution of randomly sampled points
#############################################################################################################

# EP mean
mean.ep <- sapply(random.ep, function(x){
  sapply(x, function(y) mean(y$EPcc))
}
)

colnames(mean.ep) <- names(pred2)

# EP range
range.ep <- sapply(random.ep, function(x){
  sapply(x, function(y){
    max(y$EPcc) - min(y$EPcc)
  })
}
)

colnames(range.ep) <- names(pred2)

# Proportion of positive EPcc-cl
prop.ep <- sapply(random.ep, function(x){
  sapply(x, function(y){
    epcccl <- (y$EPcc - y$EPcl)
    epcccl.prop <- sum(epcccl > 0) / nrow(y)
    return(epcccl.prop)
  })
}
)

colnames(prop.ep) <- names(pred2)


sp.occ <- list()
for(i in spname){
  sp.occ[[i]] <- sum(scores.ep[, i] == 1, na.rm = T)
}
names(sp.occ) <- names(pred2)


sp <- read.csv(paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))

sp$spname <- gsub("_",".", sp$spname)

## Significance test of EP valuse of species habitats based on the distribution of randomly sampled points

spnames <- names(pred2)[names(pred2) %in% sp$spname]

significance.test <- function(ep.data, measure){
  sig <- list()
  
  for(i in spnames){
    dat <- sp[sp$spname == i,]
    percentile <- quantile(ep.data[,i], probs = c(2.5/100, 1 - 2.5/100))
    
    sig[[i]] <- "NonSig"
    if(dat[, measure] <= percentile[1]){
      sig[[i]] <- "lower"
    } 
    
    if(dat[, measure] >= percentile[2]){
      sig[[i]] <- "higher"
    } 
    
  }
  return(sig)
}

sig <- cbind(unlist(sp.occ)[names(pred2) %in% sp$spname], 
             unlist(significance.test(prop.ep, "epcccl.prop")), 
             unlist(significance.test(range.ep, "ep.range")), 
             unlist(significance.test(mean.ep, "ep.mean"))
) %>% as.data.frame
colnames(sig) <- c("Range.size", "EPcccl.prop", "EPcc.range", "EPcc.mean")

write.csv(sig, file = paste("Y://", genus_name, "_significant_EP_suitableArea.csv", sep=""))

#################################################################################################################
### Draw plots; EP vs. species range
#################################################################################################################
data2 <- create_dataframe_EP_error(random.ep)

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
png(paste("Y://", genus_name, "_eprange.png", sep=""))
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


png(paste("Y://", genus_name, "_epmean.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP mean", x = "Species range") +
  theme_classic()
dev.off()

###############################################################################
### Climatic niche volume vs. EP
################################################################################

#### Add error bars on line plots

# Default line plot
p <- ggplot(dat2, aes_string(x = "niche_volume", y = "ep.range")) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.range, ymax = error.max.range), col = "red")

png(paste("Y://", genus_name, "_eprange_nicheVolme.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP range", x = "Species niche volume") +
  theme_classic()
dev.off()


# Default line plot
p <- ggplot(dat2, aes(x = niche_volume, y = ep.mean)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.mean, ymax = error.max.mean), col = "red")

png(paste("Y://", genus_name, "_epmean_nicheVolme.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP mean", x = "Species niche volume") +
  theme_classic()
dev.off()


# Default line plot
p <- ggplot(dat2, aes(x = niche_volume, y = ep.median)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.median, ymax = error.max.median), col = "red",
                position = position_dodge(0.05))

png(paste("Y://", genus_name, "_epmedian_nicheVolme.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP median", x = "Species niche volume") +
  theme_classic()
dev.off()



#################################################################################################################
### Draw plots; proportion of positive EPcc-cl vs. species range or niche volume
#################################################################################################################

# Default line plot
p <- ggplot(dat2, aes(x = sp.occ, y = epcccl.prop)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.prop, ymax = error.max.prop), col = "red") +
  geom_errorbar(aes(ymin = error.2.5.prop, ymax = error.97.5.prop), width = .2, col = "yellow",
                position = position_dodge(0.05)
  )

png(paste("Y://", genus_name, "speciesRange_Prop_potiveEPcc_cl.png", sep = ""))
# Finished line plot
p + labs(title = genus_name, y = "Proportion of positive EPcc-cl", x = "Species range") +
  theme_classic()
dev.off()


# Default line plot
p <- ggplot(dat2, aes(x = niche_volume, y = epcccl.prop)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.prop, ymax = error.max.prop), col = "red")

png(paste("Y://", genus_name, "speciesNiche_Prop_potiveEPcc_cl.png", sep = ""))
# Finished line plot
p + labs(title = genus_name, y = "Proportion of positive EPcc-cl", x = "Species niche volume") +
  theme_classic()
dev.off()
