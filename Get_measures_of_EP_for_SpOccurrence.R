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

scores.ep <- merge(scores, epcc[,c("x","y","EPcc")], by=c("x","y")) %>% 
  merge(., epcl[,c("x","y","EPcl")], by=c("x","y"))

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
par(mfrow = c(4,4))
lapply(spname, function(x){hist(ep.sp[[x]], main = x)})

## EPcc range over species occurrence records
ep.range <- lapply(spname, function(x){
  max(ep.sp[[x]]) - min(ep.sp[[x]])
  })


sp.occ <- list()
for(i in spname){
  sp.occ[[i]] <- sum(scores[, i] == 1)
}
names(ep.sp)<- spname

### The followings must be tested by spatial autocorrelation model!
#################################################################################################################
### Do species occurring in areas with wider range of EP have larger range?
plot(unlist(ep.range), unlist(sp.occ))

## EPcc average over species occurrence records
plot(sapply(ep.sp, mean), unlist(sp.occ))
## EPcc median over species occurrence records
plot(sapply(ep.sp, median), unlist(sp.occ))


#################################################################################################################
### Do areas with higher EP have more species?
plot(scores.ep$EPcc, sai.dat$sumsp)

#################################################################################################################
### Do species with smaller niche volume occur in rare climate areas? 
sp <- read.csv("Y:\\1st chapter_Acaena project\\Acaena manuscript\\meta data\\Acaena_data_analyses18sep.csv")
plot(unlist(ep.range), sp$niche_volume)

