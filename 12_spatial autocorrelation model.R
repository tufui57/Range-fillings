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
load("SAI_5km_LGM_PC1_2.data")
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


# data(oldcol)
# lw <- nb2listw(COL.nb, style="W")
# ev <- eigenw(similar.listw(lw))
# COL.errW.eig <- errorsarlm(CRIME ~ INC + HOVAL, data=COL.OLD,
#                            lw, quiet=FALSE, control=list(pre_eig=ev))
# summary(COL.errW.eig)