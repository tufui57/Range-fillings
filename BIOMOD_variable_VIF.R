#########################################################
## VIF for climate varibles used for building BIOMOD
#########################################################

library(car)

# Get raster stuck object of environmental variables from ".//Range fillings//BIOMOD 5km resolution with SAI.R"

# Convert raster stack to data frame
climates <- as.data.frame(myExpl)

# Calculate VIF
vif(lm(sai_cc ~ ., data = climates))
vif(lm(bioclim1 ~ bioclim6 + bioclim12 + bioclim15, data = climates))

###########################################################################
## Correlation coefficients between SAIcc, SAIcl and the difference
###########################################################################

library("PerformanceAnalytics")
# Correlation matrix
png("SAI_comparison.png")
chart.Correlation(climates, 
                  histogram=TRUE, pch=19)
dev.off()


climates2 <- climates[!is.na(climates$sai_diff), ]
cor(climates2$sai_cc, climates2$sai_cl)
plot(climates2$sai_cc, climates2$sai_cl)

cor(climates2$sai_cc, climates2$sai_diff)
plot(climates2$sai_cc, climates2$sai_diff)

cor(climates2$sai_cl, climates2$sai_diff)
plot(climates2$sai_cl, climates2$sai_diff)
