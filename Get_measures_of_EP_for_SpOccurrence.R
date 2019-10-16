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
lapply(spname, function(x){range(ep.sp[[x]])})
lapply(spname, function(x){width(ep.sp[[x]])})

#############################################################################################################
### Barplot the number of species occurrence grid cells grouped by the values of EPcl
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
