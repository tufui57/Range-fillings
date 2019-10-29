#############################################################################################################
### Prepare EP
#############################################################################################################

library(dplyr)

##############################################################################
### Data preparation
##############################################################################
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

#################################################################################################################
### Get Epcc-cl of species occurrence records
#################################################################################################################

epcccl.sp <- list()

for(i in spname){
  epcccl.sp[[i]] <- scores.ep[scores.ep[, i] == 1, c("EPcc", "EPcl")]
}
names(epcccl.sp) <- spname

# Proportions of areas with species occurrence records and positive EPcc-cl 
epcccl.prop <- list()
for(i in 1:length(epcccl.sp)){
  res <- epcccl.sp[[i]][, "EPcc"] - epcccl.sp[[i]][, "EPcl"]
  epcccl.prop[[i]] <- sum(res > 0) / nrow(epcccl.sp[[i]])
  
}



ep.range <- cbind(unlist(ep.range), sapply(ep.sp, median), sapply(ep.sp, mean), unlist(epcccl.prop)) %>% as.data.frame
colnames(ep.range) <- c("ep.range", "ep.median","ep.mean", "epcccl.prop")
ep.range$spname <- rownames(ep.range)
dat <- merge(ep.range, sp, by = "spname")
dat2 <- data.frame(c(unlist(sp.occ)), names(sp.occ))
dat <- merge(dat, dat2, by.x = "spname", by.y = "names.sp.occ.")

write.csv(dat, paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))

