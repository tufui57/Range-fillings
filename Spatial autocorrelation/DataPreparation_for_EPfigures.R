#############################################################################################################
### Prepare EP
#############################################################################################################
library(raster)
library(dplyr)
library(ggplot2)

##############################################################################
### Data preparation
##############################################################################
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

write.csv(dat, paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))

