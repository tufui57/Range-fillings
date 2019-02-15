#################################################################################
### Plot climate space on bioclim
#################################################################################

### Load coordinates of NZ at the current and the LGM
# Data of current NZ on 5km resolution
load(".\\Scores_Acaena_landcover5km.data")

# Data of the mainland at the LGM
load(".\\Scores_LGM_mainisland_worldclim1_5km.data")

### Plot 2D BIOCLIM

# bioclim 1 & 12
png("bio1_12.png", width = 900, height = 630)
par(cex=1.75)
plot(scores.lgm$bi1 / 10, scores.lgm$bi12, 
     col = "red",
     xlab = expression('AMT ('*~degree*C*')'), ylab = "AP (mm)"
     )
points(scores$bioclim1 / 10, scores$bioclim12)
legend(13, 5200, legend=c("current", "LGM"),
       col=c("black","red"), pch = 1)
dev.off()

# bioclim 6 & 15
png("bio6_15.png", width = 900, height = 630)
par(cex=1.75)
plot(scores.lgm$bi6 / 10, scores.lgm$bi15, 
     col = "red",
     xlab = expression('MTC ('*~degree*C*')'), ylab = "PS",
     xlim = c(min(scores$bioclim6)/10, max(scores$bioclim6)/10),
     ylim = c(min(scores$bioclim15), max(scores$bioclim15))
     )
points(scores$bioclim6 / 10, scores$bioclim15)
legend(6.5, 33, legend=c("current", "LGM"),
       col=c("black","red"), pch = 1)
dev.off()

