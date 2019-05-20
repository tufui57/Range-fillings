library(raster)
library(spdep)

load(".//Scores_Acaena_landcover5km.data")

po <- SpatialPoints(scores[, c("x","y")])
# Create neighbour list object
test <- dnearneigh(po, d1=4900, d2=5200)
# Some cells have no neighbours. Need zero.policy = TRUE.
nblist <- nb2listw(test, style = "W", zero.policy = TRUE)
print(nblist, zero.policy = TRUE)

# Add SAI values
#load("Y://SAI_5km_currentInLGM_5000kmWindow_4var.data")
load("Y://SAI_5km_currentInCurrent_5000kmWindow_4var.data")

scores.sai <- cbind(scores, unlist(sai))
colnames(scores.sai)[length(scores.sai)] <- "sai"

# Spatial autocorrelation model
spname <- colnames(scores)[grep("^Acaena", colnames(scores))]
sar <- list()
for(i in spname){
  form <- formula(paste(i, "~ PC1 + PC2 + sai"))
   sar[[i]] <- errorsarlm(form, data = scores.sai, nblist, zero.policy=TRUE)
   summary(sar[i])
 }

# Save the results
save(sar, file = "SARmodels_Acaena.data")

### Residual plot
# Get residuals
lapply(sar, residuals.sarlm)

# If the model fitting is high, standerdized residuals must evenly distribute.
lapply(sar, function(x){
  plot(predict.sarlm(x), residuals.sarlm(x))
  })
