###############################################################
## Get available cliamte points within 95% convex hull polygon
###############################################################

library(rgeos)
library(rgdal)

genus_name <- "Acaena"
# genus_name <- "Chionochloa"

load(paste(".\\Scores_", genus_name, "_landcover.data", sep = ""))

spname <- colnames(scores)[grepl(genus_name, colnames(scores))]

# Import convex hull polygons of species
path = paste(".\\chull\\chull_", spname, ".shp", sep="")
LAYERS <- sapply(path, ogrListLayers)
sp.conv <- mapply(readOGR, path, LAYERS)


###############################################################
## Potential climate niche overlap
###############################################################

### Intersect of polygons

# combination of species 
com <- combn(length(spname), 2)

overlap <- list()
for(i in 1:ncol(com)){
  plot(sp.conv[[com[1,i]]])
  plot(sp.conv[[com[2,i]]], border = "red", add=T)
  
  overlap[[i]] <- gIntersection(sp.conv[[com[1,i]]], sp.conv[[com[2,i]]], byid = TRUE, drop_lower_td = TRUE) 
  
  plot(overlap[[i]], border = "green", add=T)

}



