####################################################################
### Actual range overlap cannot be calculated.
####################################################################


genus_name <- "Chionochloa"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

# Import species occurrence data
source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//F02_clean_up_species_records.R")

### Get rid of species whose occurren resords < 5
dat2 <- dat[sapply(dat, nrow) >= 5]

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

# Create imaginary speices occurring at all cells in NZ
probAll <- pred[1]
values(probAll[[1]]) <- ifelse(is.na(values(probAll[[1]])), NA, 1)

sp.all <- data.frame(
  cbind(coordinates(probAll[[1]]), values(probAll[[1]]))
)
colnames(sp.all)[1:2] <- c("lon", "lat")
sp.all2 <- sp.all[!is.na(sp.all[,3]),]

i=spname[2]
spnode <- which(sispairs[,1] == get_nodeID_from_spname(i,tree))
sisname <- get_sisterSpNames(sispairs[spnode,1], tree)[[2]]

background = sp.all2
axis1 = "lon"
axis2 = "lat"
data1 = dat2[[i]]
data2 = dat2[[sisname]]

######################### NOTE ##################################################
# If species has occurrence point with the max values of background's axes,  
# ecospat.grid.clim.dyn() gives the following error;
# Error in quantile.default(spr, th.env) : 
# missing values and NaN's not allowed if 'na.rm' is FALSE

# Get rid of the points from species occurrences

removeThisRow <- which(data1$lon == max(data1$lon))
removeThisRow2 <- which(data1$lat == max(data1$lat))
removeThisRow3 <- which(data1$lon == min(data1$lon))
removeThisRow4 <- which(data1$lat == min(data1$lat))
data1.removed <- data1[-c(removeThisRow, removeThisRow2, removeThisRow3, removeThisRow4), ]


z1 <- ecospat::ecospat.grid.clim.dyn(background.clim, background.clim, 
                                     data1.removed[, c(axis1, axis2)], R = 100)
SchoenerD_ecospat
function (background, axis1, axis2, data1, data2, R = 100) 
{
  background.clim <- background[, c(axis1, axis2)]
  z1 <- ecospat::ecospat.grid.clim.dyn(background.clim, background.clim, 
                                       data1[, c(axis1, axis2)], R = 100)
  z2 <- ecospat::ecospat.grid.clim.dyn(background.clim, background.clim, 
                                       data2[, c(axis1, axis2)], R = 100)
  res <- list()
  res[[1]] <- unlist(ecospat::ecospat.niche.overlap(z1, z2, 
                                                    cor = T))
  res[[2]] <- unlist(ecospat::ecospat.niche.overlap(z1, z2, 
                                                    cor = F))
  return(res)
}
