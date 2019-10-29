#############################################################################################################
### Random sampling from climatically suitable areas
#############################################################################################################
library(raster)
library(dplyr)
library(ggplot2)
source(".\\GitHub\\Range-fillings\\Spatial autocorrelation\\F_randomSampling_EP_from_climatically_suitable_areas.R")

genus_name <- "Chionochloa"
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

##############################################################################
### Data preparation
##############################################################################

# Import species occurrence data
load(paste(".//Scores_", genus_name, "_landcover_worldclim1_5km.data", sep = ""))

# Load EPcc
load(".//EPcc_NZ_4var_test.data")
epcc <- load(".//EPcc_NZ_4var_test.data")
epcc <- get(epcc)
colnames(epcc)[ncol(epcc)] <- "EPcc"

# Load EPcl
load(".//EPcl_NZ_4var.data")
epcl <- load(".//EPcl_NZ_4var.data")
epcl <- get(epcl)
colnames(epcl)[ncol(epcl)] <- "EPcl"

scores.ep <- merge(scores, epcc[, c("x", "y", "EPcc")], by = c("x","y")) %>% 
  merge(., epcl[, c("x","y","EPcl")], by = c("x","y"))

colnames(scores.ep) <- gsub("_", ".", colnames(scores.ep))

##############################################################
# Random sampling
############################################################

scores.ep$EPcccl <- (scores.ep$EPcc - scores.ep$EPcl)

### Sample from predicted presence
random.ep <- randomsampling(spname, scores.ep, a, "EPcccl", 1000, c("x","y"))

# random.epcc <- randomsampling(spname, scores.ep, a, "EPcc", 1000, c("x","y"))

random.ep.prop.positive <- lapply(random.ep, function(x){
  res<-list()
  for(i in 1:nrow(x)){
    res[[i]] <- sum(x[i,] > 0) / length(x[i,]) 
  }
  return(
    c(
      max(unlist(res)), min(unlist(res))
    )
  )
}
)

errors <- do.call(cbind, random.ep.prop.positive) %>% t

colnames(errors) <- c("error.max.prop", "error.min.prop")
errors2 <- as.data.frame(errors)

errors2$spname <- spname

dat <- read.csv(paste("Y://randomsampledEP", genus_name,".csv"))
dat$spname <- gsub("_", ".", dat$spname)
dat2 <- merge(dat, errors2, by = "spname")

#################################################################################################################
### Get Epcc-cl of species occurrence records
#################################################################################################################

epcccl.sp <- list()

for(i in spname){
  epcccl.sp[[i]] <- scores.ep[scores.ep[, i] == 1, c("EPcc", "EPcl")]
}
names(epcccl.sp)<- spname

# Proportions of areas with species occurrence records and positive EPcc-cl 
epcccl.prop <- list()
for(i in 1:length(epcccl.sp)){
  res <- epcccl.sp[[i]][, "EPcc"] - epcccl.sp[[i]][, "EPcl"]
  epcccl.prop[[i]] <- sum(res > 0) / nrow(epcccl.sp[[i]])
  
}

dat3 <- merge(dat2, data.frame(spname, unlist(epcccl.prop)), by = "spname")

#################################################################################################################
### Draw plots
#################################################################################################################

# Default line plot
p <- ggplot(dat3, aes(x = c.unlist.sp.occ.., y = unlist.epcccl.prop.)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = error.min.prop, ymax = error.max.prop), col = "red")

png(paste("Y://", genus_name, "speciesRange_Prop_potiveEPcc_cl.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "Proportion of positive EPcc-cl", x = "Species range") +
  theme_classic()
dev.off()


# Default line plot
p <- ggplot(dat3, aes(x = niche.volume, y = unlist.epcccl.prop.)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = error.min.prop, ymax = error.max.prop), col = "red")

png(paste("Y://", genus_name, "speciesNiche_Prop_potiveEPcc_cl.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "Proportion of positive EPcc-cl", x = "Species niche volume") +
  theme_classic()
dev.off()
