##############################################################################
### Data preparation
##############################################################################

library(dplyr)

genus_name <- "Chionochloa" # "Acaena"
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

}else{
  sp <- read.csv("Y://NicheVolume_age_chion.csv")
}

names(ep.range) <- spname
ep.range <- unlist(ep.range) %>% as.data.frame
colnames(ep.range) <- "ep.range"
ep.range$spname <- rownames(ep.range)
dat <- merge(ep.range, sp, by = "spname")

png(paste("Y://", genus_name, "EPrange_speciesNicheVolume.png", sep=""))
plot(dat$ep.range, dat$niche_volume, main = genus_name, xlab = "EP range", ylab = "Climatic niche volume")
dev.off()
