#################################################################################
### Plot map of LGM climate
#################################################################################

library(ggplot2)
library(gridExtra)
library(extrafont)
library(grid)
library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)

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

### Resample LGM rasters
# Prepare a raster which is resampled to
ref_lgm.ext <- extend(ref,extent(LGM_NZTM[[1]]))

LGM_NZTM2 <- lapply(LGM_NZTM, resample, ref_lgm.ext)

# Environmental variables extracted from BIOCLIM and converted into NZTM.
LGMdata <- stack(LGM_NZTM2) %>% stack


