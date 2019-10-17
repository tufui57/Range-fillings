################################################################################
### Models accounting for spatial autocorrelation
################################################################################


########################################
### Simultenious autoregressive model
########################################

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

test <- data.frame(coordinates(pred[[1]]),values(pred[[1]]))
test2 <- test[!is.na(test$values.pred..1...), ]

scores.ep <- merge(scores, epcc[,c("x","y","EPcc")], by=c("x","y")) %>% 
  merge(., epcl[,c("x","y","EPcl")], by=c("x","y")) %>% 
  merge(., test2,  by=c("x","y"))

spname <- colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))]

### Neighbourhood structure

### Vector of lists of nearest neighbour grid cells
# Neighbourhood defined by distance
neighs <- dnearneigh(as.matrix(scores.ep[,c("x","y")]), 
                     d1 = 4900, d2 = 5200) # this generates empty neighbours for some grid cells

### Spatial autocorrelation index (Moran's I) of EPcc along distance
sp.correlogram(neighs, test$EPcc, order = 6, method = "I", zero.policy = TRUE)


### LM
m1 <- lm(values.pred..1... ~ EPcc, data=scores.ep)
# LM residuals
scores.ep$residuals <- residuals(m1)
# Moran's I by MC
moran.res <- moran.mc(scores.ep$residuals, neighs, 999)
moran.res$p.value

### Simultaneous autoregressive model

sac.model <- sacsarlm(values.pred..1... ~ EPcc, data=scores.ep, neighs)

summary(sac.model, correlation = TRUE)

scores.ep$residuals.sac <- residuals(sac.model)

### Draw correlograms for ordinary least swuares and simultaneous autoregressive models
sp.correlogram(neighs, scores.ep$residuals, order = 10, method = "I", zero.policy = TRUE) 
sp.correlogram(neighs, scores.ep$residuals.sac, order = 10, method = "I", zero.policy = TRUE)





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

################################################################################
### Generate virtual species distribution
################################################################################

library(virtualspecies)
# Referenc of this package; https://onlinelibrary.wiley.com/doi/10.1111/ecog.01388

generateRandomSp()
