##############################################################################
### Data preparation
##############################################################################

library(dplyr)
library(ggplot2)
source(".\\GitHub\\Range-fillings\\Spatial autocorrelation\\F_randomSampling_EP_from_climatically_suitable_areas.R")

genus_name <- "Acaena"
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

spname <- colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))]

sai.dat <- mutate(scores.ep, sumsp = rowSums(scores.ep[,spname]))
sai.dat <- mutate(sai.dat, spocc = ifelse(sai.dat$sumsp > 0, 1, 0))

#############################################################################################################
### Range of EPcc for species habitats
#############################################################################################################
ep.sp <- list()

for(i in spname){
  ep.sp[[i]] <- scores.ep[scores.ep[, i] == 1, "EPcc"]
}
names(ep.sp)<- spname

## EPcc range over species occurrence records
ep.range <- lapply(spname, function(x){
  max(ep.sp[[x]]) - min(ep.sp[[x]])
})


sp.occ <- list()
for(i in spname){
  sp.occ[[i]] <- sum(scores[, i] == 1)
}
names(sp.occ)<- spname


#################################################################################################################
### Do species with smaller niche volume occur in rare climate areas? 
#################################################################################################################
if(genus_name=="Acaena"){
  sp <-   read.csv("Y:\\1st chapter_Acaena project\\Acaena manuscript\\meta data\\Acaena_data_analyses18sep.csv")
  colnames(sp)
  
}else{
  sp <- read.csv("Y://NicheVolume_age_chion.csv")
  colnames(sp)[4] <- "niche_volume"
}

names(ep.range) <- spname

ep.range <- cbind(unlist(ep.range), sapply(ep.sp, median), sapply(ep.sp, mean)) %>% as.data.frame
colnames(ep.range) <- c("ep.range", "ep.median","ep.mean")
ep.range$spname <- rownames(ep.range)
dat <- merge(ep.range, sp, by = "spname")
dat2 <- data.frame(c(unlist(sp.occ)), names(sp.occ))
dat <- merge(dat, dat2, by.x = "spname", by.y = "names.sp.occ.")

#################################################################################################################
### Generate virtual species distribution
#################################################################################################################

##############################################################
# Random sampling of EP
############################################################

### Sample randomly from any grid cells in NZ
random.ep <- randomsampling(spname, scores.ep, valueName = "EPcc", iteration =  1000, coordinateNames = c("x","y"))

random.ep.mean <- lapply(random.ep, function(x){
  res<-list()
  for(i in 1:nrow(x)){
    res[[i]] <- mean(x[i,])
  }
  return(
    c(
      max(unlist(res)), min(unlist(res))
    )
  )
}
)
random.ep.median <- lapply(random.ep, function(x){
  res<- list()
  for(i in 1:nrow(x)){
    res[[i]] <- median(x[i,])
  }

  return(
    c(
      max(unlist(res)), min(unlist(res))
      )
  )
}
)
random.ep.range <- lapply(random.ep, function(x){
  res<- list()
  for(i in 1:nrow(x)){
    res[[i]] <- (max(x[i,]) - min(x[i,]))
  }
  return(
    c(
      max(unlist(res)), min(unlist(res))
    )
  )
}
)

errors <- cbind(
  do.call(cbind, random.ep.range) %>% t,
  do.call(cbind, random.ep.mean) %>% t,
  do.call(cbind, random.ep.median) %>% t
)
colnames(errors) <- c("error.max.range", "error.min.range", "error.max.mean", "error.min.mean", "error.max.median", "error.min.median")
errors2 <- as.data.frame(errors)

errors2$spname <- spname

dat3 <- merge(dat, errors2, by = "spname")

write.csv(dat3, paste("Y://randomsampledEP", genus_name,".csv"))

################################################################################
### Species range vs. EP
################################################################################

#### Add error bars on line plots

# Default line plot
p <- ggplot(dat3, aes(x = c.unlist.sp.occ.., y = ep.range)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = error.min.range, ymax = error.max.range), width=.2, col = "red",
                position=position_dodge(0.05))

png(paste("Y://", genus_name, "_eprange.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP range", x = "Species range") +
  theme_classic()
dev.off()

# Default line plot
p <- ggplot(dat3, aes(x = c.unlist.sp.occ.., y = ep.mean)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = error.min.mean, ymax = error.max.mean), width=.2, col = "red",
                position=position_dodge(0.05))

png(paste("Y://", genus_name, "_epmean.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP mean", x = "Species range") +
  theme_classic()
dev.off()


# Default line plot
p <- ggplot(dat3, aes(x = c.unlist.sp.occ.., y = ep.median)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = error.min.median, ymax = error.max.median), width=.2, col = "red",
                position=position_dodge(0.05))

png(paste("Y://", genus_name, "_epmedian.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP median", x = "Species range") +
  theme_classic()
dev.off()


################################################################################
### Climatic niche volume vs. EP
################################################################################

#### Add error bars on line plots

# Default line plot
p <- ggplot(dat3, aes_string(x = "niche_volume", y = "ep.range")) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.range, ymax = error.max.range), col = "red")

png(paste("Y://", genus_name, "_eprange.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP range", x = "Species niche volume") +
  theme_classic()
dev.off()


# Default line plot
p <- ggplot(dat3, aes(x = niche_volume, y = ep.mean)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.mean, ymax = error.max.mean), col = "red")

png(paste("Y://", genus_name, "_epmean.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP mean", x = "Species niche volume") +
  theme_classic()
dev.off()


# Default line plot
p <- ggplot(dat3, aes(x = niche_volume, y = ep.median)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.median, ymax = error.max.median), col = "red",
                position = position_dodge(0.05))

png(paste("Y://", genus_name, "_epmedian.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP median", x = "Species niche volume") +
  theme_classic()
dev.off()



########################################################################################################
# Random sampling for testing range size vs. niche volume (calcualted by ranges of annual temperature and precipitaion)
######################################################################################################

random.niche.volume <- list()

for(i in seq(100, 5000, 100)){
  
  test.row <- sample(1:nrow(scores.ep), i)
  test.s <- scores.ep[test.row, ]
  random.niche.volume[[i]] <- nichePlot::SchoenerD_ecospat(scores.ep, "bioclim1", "bioclim12", scores.ep, test.s) 
  
}

plot(seq(100, 5000, 100), 
  sapply(random.niche.volume[seq(100, 5000, 100)], function(x){
  x[[1]][1]
    }
  ),
  main = "Climate envelope volumes of Randomly sampled areas",
  xlab = "number of samples",
  ylab = "Climate envelope volume"
)

random.niche.volume10 <- list()

for(i in seq(10, 1500, 10)){
  
  test.row <- sample(1:nrow(scores.ep), i)
  test.s <- scores.ep[test.row, ]
  random.niche.volume10[[i]] <- nichePlot::SchoenerD_ecospat(scores.ep, "bioclim1", "bioclim12", scores.ep, test.s) 
  
}

png("Y://Climate envelope volumes of Randomly sampled areas.png")
plot(seq(10, 1500, 10), 
     sapply(random.niche.volume10[seq(10, 1500, 10)], function(x){
       x[[1]][1]
     }
     ),
     main = "Climate envelope volumes of Randomly sampled areas",
     xlab = "number of samples",
     ylab = "Climate envelope volume"
)
dev.off()

#######################################################################################
### Are Acaena species distributed randomly within suitable cliamte range?
#######################################################################################


##################################################################################################################
########################        Generate climate raster stack
##################################################################################################################


###        5km grid data import
datapath <- "Y://Acaena_bioclim_landcover_history_worldclim1_5km.csv"

### Import raster of climate data
# Reference raster for coordinate system. This raster must have the same dimentions as the raster of occurrence data
ref.raster <- raster(
  paste("Y://GIS map and Climate data//newzealandpotentialvegetatio5.bil", sep="")
)
proj4stringNZTM <- proj4string(ref.raster)
# Worldclim ver.1.4
path <-"Y:\\GIS map and Climate data\\worldclim\\bio_411"
source(".\\GitHub\\functions\\F01_project_resample_WORLDCLIM.R")



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

#################################################################################################################
### Generate virtual species distribution
#################################################################################################################

library(virtualspecies)

test <- generateRandomSp(stack(bioNZ))

test2 <- generateRandomSp(stack(bioNZ))

test3 <- generateRandomSp(stack(bioNZ), niche.breadth = "narrow")
test4 <- generateRandomSp(stack(bioNZ), niche.breadth = "narrow")


test.s <-sampleOccurrences(test$pa.raster, 1000)
test2.s <-sampleOccurrences(test2$pa.raster, 1000)
test3.s <- sampleOccurrences(test3$pa.raster, 100)
test4.s <- sampleOccurrences(test4$pa.raster, 100)



test.s.ep <- merge(test.s$sample.points, scores.ep, by = c("x", "y"))



### Repeat randomization

### the following doesn't work when random suitatbility has too low values. they can't have >500 occurrences.
test.s.ep  <- list()
for(i in 1:100){
  
  test <- generateRandomSp(stack(bioNZ))
  test.s <-sampleOccurrences(test$pa.raster, 500)
  test.s.ep[[i]] <- merge(test.s$sample.points, scores.ep, by = c("x", "y"))
  
}

### Try generating distributoions with niche volumes on PCA

generateSpFromPCA()
