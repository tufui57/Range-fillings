###############################################################
## Get available cliamte points within 95% convex hull polygon
###############################################################

library(rgeos)
library(rgdal)

source(".//functions//F_dataframe_to_points.R")

genus_name <- "Acaena"
# genus_name <- "Chionochloa"

load(paste(".\\Scores_", genus_name, "_landcover.data", sep = ""))

spname <- colnames(scores)[grepl(genus_name, colnames(scores))]

# Import convex hull polygons of species
path = paste(".\\chull\\chull_", spname, ".shp", sep="")
LAYERS <- sapply(path, ogrListLayers)
sp.conv <- mapply(readOGR, path, LAYERS)


###############################################################
## Points within polygons
###############################################################

# Convert data frame to points
point.sp <- points_from_dataframe(scores, spname[[1]], c("PC1", "PC2"))

# Plot the points and polygon
plot(point.sp)
plot(sp.conv[[1]], col = "red", add = T)


# Clip points by polygon
overlap <- over(point.sp, y = sp.conv[[1]])

points(overlap, col = "green")
