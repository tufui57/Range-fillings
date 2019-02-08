#########################################################
## VIF for climate varibles used for building BIOMOD
#########################################################

library(car)

# Get raster stuck object of environmental variables from ".//Range fillings//BIOMOD 5km resolution with SAI.R"

# Convert raster stack to data frame
climates <- as.data.frame(myExpl)


# Calculate VIF
vif(lm(sai_cinl ~ ., data = climates))
