#################################################################################################################
########################     Calculate and Plot Median of individual model projections 
#################################################################################################################

setwd("Y://")

### load libraries
library(rgdal)
library(raster)
library(biomod2)
library(png)
library(fields)

##### NOTE #####
# R can't process multiple big raster objects (bigger than 1.9GB).
# You can't run the following code by lapply().

folders <- list.dirs("BIOMOD", full.names = FALSE, recursive = F)
s=folders[15]
for (s in folders) {

    if (file.exists(paste("BIOMOD\\", gsub("\\.", "_", s), "_projection_median.tif", sep = ""))) {
        rast <- raster(paste("BIOMOD\\", gsub("\\.", "_", s), "_projection_median.tif", sep = ""))

    } else {

        # load projection data
        a <- load(paste("BIOMOD\\", s, "\\proj_current\\", s, ".current.projection.out",
                    sep = ""))
        p <- get(a)

        ## plot each projection separately 
        proj_val <- get_predictions(p)
        test <- lapply(proj_val@layers, function(x) {
            return(values(x))
        })

        test2 <- do.call(cbind, test)
        colnames(test2) <- names(proj_val)

        test3 <- apply(test2, 1, median)

        # set the same dimentions as target raster (ex. bioclim map)
        rast <- raster(ncol = 3600, nrow = 3600)
        extent(rast) <- extent(proj_val@layers[[1]])

        values(rast) <- test3

        # save raster
        writeRaster(rast, paste("BIOMOD\\", gsub("\\.", "_", s), "_projection_median.tif",
                            sep = ""), format = "GTiff")
    }

    # import occurrence maps
    ima <- readPNG(paste("Acaena project\\figures\\Occurrence maps\\Occurrence map based on observation\\",
                       ifelse(grepl("var", s), gsub(".var..", "_var._", s),
                           ifelse(grepl("novae.", s), gsub("novae.", "novae-", s), s)
                           ), "_Occurrence_transparent.png", sep = ""))

    # plot median probability on NZ map
    png(filename = paste("BIOMOD\\", s, "_probability_median2", ".png", sep = ""), height = 900, width = 750, units = "px")

    plot(rast, xlim = c(165, 180), ylim = c(-48, -34))

    # add occurrence map image
    rasterImage(ima, 174, -48, 180, -42)

    title(paste(s, "_probability_median", sep = ""))
    dev.off()
}
