#########################################################
## VIF for climate varibles used for building BIOMOD
#########################################################

library(car)

# Get raster stuck object of environmental variables from ".//Range fillings//BIOMOD 5km resolution with SAI.R"

# Convert raster stack to data frame
climates <- as.data.frame(myExpl)


# Calculate VIF
vif(lm(sai_cc ~ ., data = climates))

###########################################################################
## Correlation coefficients between SAIcc, SAIcl and the difference
###########################################################################

climates2 <- climates[!is.na(climates$sai_diff), ]
cor(climates2$sai_cc, climates2$sai_cl)
plot(climates2$sai_cc, climates2$sai_cl)

cor(climates2$sai_cc, climates2$sai_diff)
plot(climates2$sai_cc, climates2$sai_diff)

cor(climates2$sai_cl, climates2$sai_diff)
plot(climates2$sai_cl, climates2$sai_diff)
