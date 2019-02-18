#########################################################
## Prepare LGM data in BIOMOD format
#########################################################

### Import raster of climate data
# Reference for WGS
ref.raster <- raster("Y:\\GIS map and Climate data\\worldclim\\mrlgmbi_2-5m\\mrlgmbi1.tif")

# Reference for NZTM
ref <- raster("Y:\\GIS map and Climate data\\newzealandpotentialvegetatio5.bil")

### Import raster of climate data
# Reference for WGS
LGMworld<- lapply(paste("Y:\\GIS map and Climate data\\worldclim\\mrlgmbi_2-5m\\mrlgmbi", c(1,6,12,15), ".tif", sep=""),
                     raster)
# Change the reference's extent
e <- extent(165, 180, -49.5, -33)
LGMwgs_cropped <- lapply(LGMworld, crop, e)

### Project raster from WGS84 to NZTM
LGM_NZTM <- lapply(LGMwgs_cropped, projectRaster, crs = proj4string(ref))

names(LGM_NZTM) <- c("bioclim1", "bioclim6", "bioclim12", "bioclim15")

# Environmental variables extracted from BIOCLIM and converted into NZTM.
LGMdata <- stack(LGM_NZTM) %>% stack
