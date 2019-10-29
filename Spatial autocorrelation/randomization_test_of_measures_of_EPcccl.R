#############################################################################################################
### Random EPcc-cl
#############################################################################################################

##############################################################################
### Data preparation
##############################################################################

library(dplyr)
library(ggplot2)

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

##############################################################
# Random sampling of EPcc-cl
############################################################

scores.ep$EPcccl <- (scores.ep$EPcc - scores.ep$EPcl)

ran.ep <- list()

for(i in 1:length(spname)){
  
  test  <- list()
  
  for(j in 1:1000){
    
    test.row <- sample(1:nrow(scores.ep), sum(scores.ep[, spname[i]]))
    test[[j]] <- scores.ep[test.row, "EPcccl"]
    
  }
  ran.ep[[i]] <- test
}

random.ep <- lapply(ran.ep, function(x){
  do.call(rbind, x)
}
)

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
p <- ggplot(dat3, aes(x = niche_volume, y = unlist.epcccl.prop.)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = error.min.prop, ymax = error.max.prop), col = "red")

png(paste("Y://", genus_name, "speciesNiche_Prop_potiveEPcc_cl.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "Proportion of positive EPcc-cl", x = "Species niche volume") +
  theme_classic()
dev.off()
