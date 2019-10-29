##############################################################################
### Data preparation
##############################################################################

library(dplyr)
library(ggplot2)
source(".\\GitHub\\Range-fillings\\Spatial autocorrelation\\F_randomSampling_EP_from_climatically_suitable_areas.R")

genus_name <- "Acaena"
dat <- read.csv(paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))

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



