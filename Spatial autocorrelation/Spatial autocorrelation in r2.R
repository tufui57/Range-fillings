################################################################################
### Models accounting for spatial autocorrelation
################################################################################


########################################
### Simultenious autoregressive model
########################################

library(spdep)
library(spatialreg)
library(dplyr)
library(raster)

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

# load climatic suitability of species
ensambleProj.name = "SAIdiff_4Mar19_ensamble"
binary = "prob"
load(paste("Y://ensemblePredictionBinary_", genus_name, "SAIdiff_4Mar19_ensambleprob.data", sep = ""))

spname <- colnames(scores)[grepl(paste("^", genus_name, sep = ""), colnames(scores))]

names(pred) == spname

### Convert raster of probability to dataframe
probability <- list()

for(i in 1:length(spname)){

  tryCatch(
    {
      d1 <- data.frame(coordinates(pred[[i]]),values(pred[[i]]))
      colnames(d1)[3] <- "pred"
      probability[[i]] <- d1[!is.na(d1[, "pred"]), ]
    }, error=function(e){}
  )
  
}

# test
i = 1

scores.ep <- merge(scores, epcc[,c("x","y","EPcc")], by=c("x","y")) %>% 
  merge(., epcl[,c("x","y","EPcl")], by=c("x","y")) %>% 
  merge(., probability[[i]],  by=c("x","y"))


### Neighbourhood structure

### Vector of lists of nearest neighbour grid cells
# Neighbourhood defined by distance
neighs <- dnearneigh(as.matrix(scores.ep[,c("x","y")]), 
                     d1 = 4900, d2 = 20100) 
# d2 = 5100 means 5km and it generates empty neighbours for some grid cells and link for each cell is just 1.

### Spatial autocorrelation index (Moran's I) of EPcc along distance
sp.correlogram(neighs, test$EPcc, order = 6, method = "I", zero.policy = TRUE)


### LM
m1 <- lm(pred ~ EPcc, data = scores.ep)
# LM residuals
scores.ep$residuals <- residuals(m1)
# Moran's I by MC
moran.res <- moran.mc(scores.ep$residuals, nb2listw(neighs, zero.policy=TRUE), 
                      nsim = 99, # iteration only 99 times
                      zero.policy = T)
moran.res$p.value

### Simultaneous autoregressive model

system.time(
  sac.model <- spatialreg::sacsarlm(pred ~ EPcc, data = scores.ep, 
                                    nb2listw(neighs, zero.policy = T),
                                    zero.policy = T)
)
# user   system  elapsed 
# 13077.63     8.02 13086.77 

summary(sac.model, correlation = TRUE)

scores.ep$residuals.sac <- residuals(sac.model)

### Draw correlograms for ordinary least swuares and simultaneous autoregressive models
system.time(
  sp.correlogram(neighs, scores.ep$residuals, order = 10, method = "I", zero.policy = TRUE) 
)
#    user  system elapsed 
# 3712.04    2.96 3720.02 

system.time(
  res <- sp.correlogram(neighs, scores.ep$residuals.sac, order = 10, method = "I", zero.policy = TRUE) 
)




### Neighbourhood structure
test <- scores.ep[1:8000,]
### Vector of lists of nearest neighbour grid cells
# Neighbourhood defined by distance
neighs <- dnearneigh(as.matrix(test[, c("x","y")]), 
                     d1 = 4900, d2 = 40800) # this generates empty neighbours for some grid cells
### LM
m1 <- lm(values.pred..1... ~ EPcc, data = test)
# LM residuals
test$residuals <- residuals(m1)
# Moran's I test by MC
moran.res <- moran.mc(test$residuals, nb2listw(neighs, zero.policy=TRUE), 999)
moran.res$p.value

### Simultaneous autoregressive model

sac.model <- sacsarlm(values.pred..1... ~ EPcc, data = test, nb2listw(neighs))

summary(sac.model, correlation = TRUE)

test$residuals.sac <- residuals(sac.model)

### Draw correlograms for ordinary least swuares and simultaneous autoregressive models
sp.correlogram(neighs, test$residuals, order = 6, method = "I", 
               zero.policy = TRUE # Include list of no-neighbour observations in output if TRUE
               ) 
sp.correlogram(neighs, test$residuals.sac, order = 6, method = "I", zero.policy = TRUE)

moran.test(test$residuals.sac, nb2listw(neighs))


