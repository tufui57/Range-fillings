##############################################################################
### Data preparation
##############################################################################

library(dplyr)

genus_name <- "Chionochloa"
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

### Histograms of occurrence records and EPcc
# par(mfrow = c(4,4))
# lapply(spname, function(x){hist(ep.sp[[x]], main = x)})

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
### The followings must be tested by spatial autocorrelation model!
#################################################################################################################
### Do species occurring in areas with wider range of EP have larger range?
# par(mfrow = c(1,1))
png(paste("Y://", genus_name, "EPrange_speciesRange.png", sep=""))
plot(unlist(ep.range), unlist(sp.occ), main = genus_name, xlab = "EP range over species habitat", ylab = "Species range size")
dev.off()

## EPcc average over species occurrence records
png(paste("Y://", genus_name, "EPmean_speciesRange.png", sep=""))
plot(sapply(ep.sp, mean), unlist(sp.occ), main = genus_name, xlab = "EP mean over species habitat", ylab = "Species range size")
dev.off()

## EPcc median over species occurrence records
png(paste("Y://", genus_name, "EPmedian_speciesRange.png", sep=""))
plot(sapply(ep.sp, median), unlist(sp.occ), main = genus_name, xlab = "EP median over species habitat", ylab = "Species range size")
dev.off()

#################################################################################################################
### Do areas with higher EP have more species?
#################################################################################################################
library(ggplot2)
library(gridExtra)

png(paste("Y://", genus_name, "EPmedian_speciesRange.png", sep=""))
p1 <- ggplot() +
  geom_point(aes(scores.ep$EPcc, sai.dat$sumsp)) +
  ggtitle(genus_name) +
  xlab("EPcc") +
  ylab("Species richness")

p2 <- ggplot() +
  geom_histogram(aes(scores.ep$EPcc)) + 
  xlab("EPcc")

grid.arrange(p1, p2,
  heights = c(2, 1)
)
dev.off()

#################################################################################################################
### Do species with smaller niche volume occur in rare climate areas? 
#################################################################################################################
if(genus_name=="Acaena"){
  sp <-   read.csv("Y:\\1st chapter_Acaena project\\Acaena manuscript\\meta data\\Acaena_data_analyses18sep.csv")
  colnames(sp)

}else{
  sp <- read.csv("Y://NicheVolume_age_chion.csv")
}

names(ep.range) <- spname

ep.range <- cbind(unlist(ep.range), sapply(ep.sp, median), sapply(ep.sp, mean)) %>% as.data.frame
colnames(ep.range) <- c("ep.range", "ep.median","ep.mean")
ep.range$spname <- rownames(ep.range)
dat <- merge(ep.range, sp, by = "spname")
dat2 <- data.frame(c(unlist(sp.occ)), names(sp.occ))
dat <- merge(dat, dat2, by.x = "spname", by.y = "names.sp.occ.")

png(paste("Y://", genus_name, "EPrange_speciesNicheVolume.png", sep=""))
plot(dat$ep.range, dat$niche_volume, main = genus_name, xlab = "EP range", ylab = "Climatic niche volume")
dev.off()

png(paste("Y://", genus_name, "EPmean_speciesNicheVolume.png", sep=""))
plot(dat$ep.mean, dat$niche_volume, main = genus_name, xlab = "EP mean", ylab = "Climatic niche volume")
dev.off()

png(paste("Y://", genus_name, "EPmedian_speciesNicheVolume.png", sep=""))
plot(dat$ep.median, dat$niche_volume, main = genus_name, xlab = "EP median", ylab = "Climatic niche volume")
dev.off()

#################################################################################################################
### Climatic niche volume vs. range size
#################################################################################################################

png(paste("Y://", genus_name, "speciesRange_speciesNicheVolume.png", sep=""))
plot(dat$c.unlist.sp.occ.., dat$niche_volume, main = genus_name, xlab = "Speices range", ylab = "Climatic niche volume")
dev.off()


#################################################################################################################
### Epcc-cl vs. species occurrences
#################################################################################################################

epcccl.sp <- list()

for(i in spname){
  epcccl.sp[[i]] <- scores.ep[scores.ep[, i] == 1, c("EPcc", "EPcl")]
}
names(epcccl.sp)<- spname

# Draw EPcc-cl histgrams over areas with species occurrence records
for(i in 1:length(epcccl.sp)){
  hist(epcccl.sp[[i]][, "EPcc"] - epcccl.sp[[i]][, "EPcl"], 
       main = names(epcccl.sp)[i], xlab = "EPcc-cl",
       xlim = c(-0.15, 0.15)
       )
}

# Proportions of areas with species occurrence records and positive EPcc-cl 
epcccl.prop <- list()
for(i in 1:length(epcccl.sp)){
  res <- epcccl.sp[[i]][, "EPcc"] - epcccl.sp[[i]][, "EPcl"]
  epcccl.prop[[i]] <- sum(res > 0) / nrow(epcccl.sp[[i]])
  
}

dat3 <- merge(dat, data.frame(spname, unlist(epcccl.prop)), by = "spname")

png(paste("Y://", genus_name, "speciesRange_Prop_potiveEPcc_cl.png", sep=""))
plot(dat3$c.unlist.sp.occ.., dat3$unlist.epcccl.prop.,
     main=genus_name, 
     xlab = "Species range", ylab = "Proportion of positive EPcc-cl over species range size")
dev.off()
