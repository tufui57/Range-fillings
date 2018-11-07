###############################################################
## Get polygon of 95% convex hull
###############################################################
library(aplpack)
library("sp")
library("rgdal")

genus_name <- "Acaena"
# genus_name <- "Chionochloa"

load(paste(".\\Scores_", genus_name, "_landcover.data", sep = ""))

spname <- colnames(scores)[grepl(genus_name, colnames(scores))]

# Reference raster for project string
ref.raster <- raster(
  paste("Y://GIS map and Climate data//newzealandpotentialvegetatio1.bil", sep="")
)
proj4stringNZTM <- proj4string(ref.raster)

for(i in spname){
  # Get species
  sp.score <- scores[scores[, i] == 1,]
  # Get 95% convex hull
  coords <- plothulls(sp.score[,c("PC1", "PC2")], fraction=.95,  col.hull="red", lwd.hull=3)
  
  # Convert dataframe object of the convex hull into SpatialPolygon 
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  # set coordinate reference system with SpatialPolygons(..., proj4string=CRS(...))
  # e.g. CRS("+proj=longlat +datum=WGS84")
  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
  
  # Write the SpatialPolyon
  writeOGR(sp_poly_df, "chull", layer=paste("chull_", i, sep=""), driver="ESRI Shapefile")
  
  }

