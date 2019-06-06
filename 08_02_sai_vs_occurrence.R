##############################################################################
### Data preparation
##############################################################################

library(dplyr)

genus_name <- "Chionochloa"

# Import species occurrence data
load(paste("Y://5th chapter SAI chapter//raw data//Scores_", genus_name,"_landcover_worldclim1_5km.data", sep=""))

source(".//SAI//05_calculate_SAI_5km_differnce.R")

scores.sai <- merge(sai.diff, scores, by = c("x","y"))
spname <- colnames(scores.sai)[grepl(paste("^", genus_name, sep = ""), colnames(scores.sai))]

sai.dat <- mutate(scores.sai, sumsp = rowSums(scores.sai[,spname]))
sai.dat <- mutate(scores.sai, spocc = ifelse(sai.dat$sumsp > 0, 1,0))

#############################################################################################################
### Barplot the number of species occurrence grid cells grouped by the values of SAIcl (climate stability)
#############################################################################################################

res <- list()
for(i in seq(0,1,0.1)[-11]){
  sai.dat2 <- sai.dat[sai.dat$SAIcl >= i & sai.dat$SAIcl <= i + 0.1, ]
  res[as.character(i)] <- sum(sai.dat2$sumsp)
}

res.occ <- list()
for(i in seq(0,1,0.1)[-11]){
  sai.dat2 <- sai.dat[sai.dat$SAIcl >= i & sai.dat$SAIcl <= i + 0.1, ]
  res.occ[as.character(i)] <- sum(sai.dat2$spocc)
}

res.sai<-list()
for(i in seq(0,1,0.1)[-11]){
  sai.dat2 <- sai.dat[sai.dat$SAIcl >= i & sai.dat$SAIcl <= i + 0.1, ]
  res.sai[as.character(i)] <- nrow(sai.dat2)
}

barplot(unlist(res.sai))
barplot(unlist(res.occ), col = "red", add = T)
