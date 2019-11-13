#############################################################################################################
### Prepare EP
#############################################################################################################

library(dplyr)

##############################################################################
### Data preparation
##############################################################################
genus_name <- "Chionochloa"
if(genus_name == "Nothofagus"){
  # Import species occurrence data
  scores <- read.csv("Y://Nothofagus_in_nz.csv")
  spname <- colnames(scores)[grepl(paste("^", genus_name, sep = ""), colnames(scores))]
  
  scores <- scores[!is.na(scores[, spname[1]]), ]

  }else{
  
  # Import species occurrence data
  load(paste(".//Scores_", genus_name, "_landcover_worldclim1_5km.data", sep = ""))
  }

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

colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))] <-
  colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))] %>% gsub("_", ".", .)

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
  }
  
if(genus_name=="Chionochloa"){
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

ep <- cbind(unlist(sp.occ), unlist(ep.range), sapply(ep.sp, median), sapply(ep.sp, mean), unlist(epcccl.prop)) %>% as.data.frame
colnames(ep) <- c("sp.occ", "ep.range", "ep.median","ep.mean", "epcccl.prop")
ep$spname <- rownames(ep)

if(genus_name == "Nothofagus"){
  
  niche <- list()
  for(i in spname){
    sp <- scores.ep[scores.ep[, i] == 1, ]
    niche.vol <- tryCatch(
      nichePlot::SchoenerD_ecospat(scores.ep, "PC1", "PC2", scores.ep, sp),
      error = function(err) NA
    )
    niche[[i]] <- tryCatch(niche.vol[[1]][1],
                      error = function(err) NA
    )
  }
  ep$niche_volume <- unlist(niche)

  write.csv(ep, paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))
  
  }else{
    dat <- merge(ep, sp, by = "spname")
    dat$spname <- gsub("_", ".", dat$spname)
    write.csv(dat, paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))
}
