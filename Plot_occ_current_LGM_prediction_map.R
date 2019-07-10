library(dplyr)
library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)

# character string of target genus name
genus_name <- "Chionochloa"

source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")

############################################################################################################
### Occurrence record maps 
############################################################################################################

### Load occurrence record data
# data frame of occurrence data and climate data
if (genus_name == "Acaena"){
  datapath <- "Y://1st chapter_Acaena project//Acaena manuscript//meta data//Acaena_bioclim_landcover_history_worldclim1_1km.csv"
}else{
  datapath <- "Y://Chionochloa_bioclim_landcover_history_worldclim1_1km.csv"
}

### Import raster of climate data
# Reference raster for coordinate system. This raster must have the same dimentions as the raster of occurrence data
ref.raster <- raster(
  paste("Y://GIS map and Climate data//newzealandpotentialvegetatio1.bil", sep="")
)
# Outline of NZ
path = "Y:\\GIS map and Climate data\\lds-nz-coastlines-and-islands-polygons-topo-150k-SHP\\nz-coastlines-and-islands-polygons-topo-150k.shp"
LAYERS <- ogrListLayers(path)
nzland <- readOGR(path, LAYERS)
# Crop extent of polygon
nzland <- crop(nzland, ref.raster)

# Import data frame of bioclim and occurrence records
climate.occ <- read.csv(datapath)

# Get species name
spname <- colnames(climate.occ)[grepl(genus_name, colnames(climate.occ))]

# Remove NA from BIOCLIM
climate.occ2 <- (is.na(climate.occ$bioclim1) == F) %>% climate.occ[.,]
# Remove rows which have no occurrence records of any species
climate.occ3 <- (rowSums(climate.occ2[, spname], na.rm = T) > 0) %>% climate.occ2[.,]

colnames(climate.occ3)[grepl(genus_name, colnames(climate.occ3))] <- colnames(climate.occ3)[grepl(genus_name, colnames(climate.occ3))] %>% gsub("_",".", .)

map_plot <- function(species, # character string of species name
         data # data for map
){
  # subset data for a species
  d.s <- data[which(data[, species] == 1), ]
  
  # create point
  pts <- d.s[, c("x", "y")]
  
  # point coordinate system setting
  coordinates(pts) <- d.s[, c("x", "y")]
  proj4pts <- proj4string(ref.raster)
  proj4string(pts) <- CRS(proj4pts)
  
  # Get species name tag 
  tag <- sptag2[which(spname == species)]
  
  # Plot map
  pMap <- ggplot() +
    geom_polygon(data = nzland, aes(x = long, y = lat, group = group), colour = "gray50", fill = 'gray90') +
    geom_point(data = d.s, aes(x = x, y = y), color = "blue", alpha = 0.1) +
    ggtitle(tag) +
    guides(colour = guide_legend(override.aes = list(size = 5, shape = 16, alpha = 0.7))) +
    labs(x = "", y = "") +
    # legend position inside plot at topleft
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          plot.title = element_text(family = "Times New Roman", size = 20),
          legend.justification = c(1, 1), legend.position = c(0.3, 1),
          panel.background =  element_blank(), #element_rect(fill = 'gray96'),
          #axis.text.y = element_text(angle = 90, hjust = 0.5)
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
  
  return(pMap)
  
}

############################################################################################################
### Prediction maps 
############################################################################################################

load(paste("Y://ensemblePredictionBinary_", genus_name, "SAIdiff_4Mar19_ensambleprob.data", sep = ""))
load(paste("Y://ensemblePredictionBinary_", genus_name, "5kmLGM_15Jan19prob.data", sep = ""))

# Get species name
spname <- colnames(climate.occ)[grepl(genus_name, colnames(climate.occ))] %>% gsub("_",".",.)
### Name tag
sptag <- makeTag_separate(spname, genus_name, ".")
sptag2 <- sptag$tag %>% as.character

pred2 <- pred[names(pred) %in% spname]
spname2 <- spname[spname %in% names(pred2)]
spname3 <- spname2[-c(which(pred2 == "NA"))]

occ <- list()
for(i in spname3){
  occ[[i]] <- map_plot(i, climate.occ3)
}

pred.cur <- list()
for(i in spname3){
  dat.cur <- data.frame(cbind(coordinates(pred[[i]]), values(pred[[i]])))
  colnames(dat.cur)[3] <- "Probability"
  
  dat.cur <- rbind(dat.cur, c(max(dat.cur$x), max(dat.cur$y), 1000))
  
  pred.cur[[i]] <- ggplot(dat.cur, aes(x = x, y = y)) +
    geom_raster(aes(fill = Probability)) +
    scale_fill_gradientn(colours = terrain.colors(1000), na.value = "white") +
    labs(x="", y="") +
    # legend position inside plot at topleft
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          legend.justification = c(1, 1), legend.position = c(0.3, 1),
          panel.background = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
}

names(predLGM) <- names(pred)
pred.lgm <- list()
for(i in spname3){
  dat.lgm <- data.frame(cbind(coordinates(predLGM[[i]]), values(predLGM[[i]])))
  colnames(dat.lgm)[3] <- "Probability"
  
  dat.lgm <- rbind(dat.lgm, c(max(dat.lgm$x), max(dat.lgm$y), 1000))
  
  pred.lgm[[i]] <- ggplot(dat.lgm, aes(x = x, y = y)) +
    geom_raster(aes(fill = Probability)) +
    scale_fill_gradientn(colours = terrain.colors(1000), na.value = "white") +
    labs(x="", y="") +
    # legend position inside plot at topleft
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          legend.justification = c(1, 1), legend.position = c(0.3, 1),
          panel.background = element_blank(), 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
}


# Plot in multiple panels
for(i in seq(1, length(pred.cur), 5)){
  
  png(paste("Y://Maps_",genus_name, i, ".png", sep=""), width = 800, height = 1300)

  grid.arrange(occ[[i]], pred.cur[[i]], pred.lgm[[i]], 
               occ[[i+1]], pred.cur[[i+1]], pred.lgm[[i+1]], 
               occ[[i+2]], pred.cur[[i+2]], pred.lgm[[i+2]], 
               occ[[i+3]], pred.cur[[i+3]], pred.lgm[[i+3]], 
               occ[[i+4]], pred.cur[[i+4]], pred.lgm[[i+4]], 
               nrow= 5, ncol = 3
               )
  
  dev.off()
}

if(genus_name=="Acaena"){
i=16
# Plot the rest of maps
png(paste("Y://Maps_",genus_name, i, ".png", sep=""), width = 800, height = 1300)

grid.arrange(occ[[i]], pred.cur[[i]], pred.lgm[[i]], 
             occ[[i+1]], pred.cur[[i+1]], pred.lgm[[i+1]], 
             nrow= 5, ncol = 3
)

dev.off()
}

if(genus_name=="Chionochloa"){
  i=31

  # Plot the rest of maps
  png(paste("Y://Maps_",genus_name, i, ".png", sep=""), width = 800, height = 1300)
  
  grid.arrange(occ[[i]], pred.cur[[i]], pred.lgm[[i]], 
               nrow= 5, ncol = 3
  )
  
  dev.off()
}

