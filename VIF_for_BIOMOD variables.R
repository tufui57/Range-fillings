#########################################################
## VIF for climate varibles used for building BIOMOD
#########################################################

library(car)

# Environmental variables extracted from BIOCLIM and converted into NZTM.
myExpl <- stack(data.ras[[c("bioclim1", "bioclim6", "bioclim12", "bioclim15", "sai")]])

# Convert raster stack to data frame
climates <- as.data.frame(myExpl)



vif(lm(climates$sai ~ climates$bioclim1 + climates$bioclim6 + climates$bioclim12 + climates$bioclim15))
