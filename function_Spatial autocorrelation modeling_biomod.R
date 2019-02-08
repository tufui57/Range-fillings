############################################################################################################
############################                    BIOMOD                     #################################
############################################################################################################

x <- c("rgdal", "raster", "dplyr", "biomod2")
lapply(x, require, character.only = TRUE)

######################################################################################################
## NOTE! MAKE SURE package "biomod2" is the latest version! IF NOT, INSTALL AGAIN!
## Otherwise, you get errors on model developing!
######################################################################################################

# Load functions to develop BIOMOD
source(".\\Range fillings\\F_Create_Package_BIOMOD.r")

############ NOTE ########################################################################
# BIOMOD_Projection() & BIOMOD_Modeling() write raster files in tempdir()
# MAKE SURE TEMPORARY RASTER FOLDER IS NOT FULL! 
##########################################################################################

# clear temporary raster folder 
tempFilePath <- list.files(rasterOptions()$tmpdir, full.names = T)
file.remove(tempFilePath)

##################################################################################################################
########################        1km grid data import
##################################################################################################################
# character string of target genus name
genus_name <- "Acaena"

### Set arguments
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
proj4stringNZTM <- proj4string(ref.raster)
# Worldclim ver.1.4
path <-"Y:\\GIS map and Climate data\\worldclim\\bio_411"
source(".\\functions\\F01_project_resample_WORLDCLIM.R")


# Import data frame of bioclim and occurrence records
climate.occ <- read.csv(datapath)
# Change colmun name of coordinates 
names(climate.occ)[names(climate.occ) %in% c("x","y")] <- c("NZTMlon", "NZTMlat")

# Change object name of bioclim rasters
data.ras <- bioNZ


# Get species name
spname <- colnames(climate.occ)[grepl(genus_name, colnames(climate.occ))]

# Remove NA from BIOCLIM
climate.occ2 <- (is.na(climate.occ$bioclim1) == F) %>% climate.occ[.,]
# Remove rows which have no occurrence records of any species
climate.occ3 <- (rowSums(climate.occ2[, spname], na.rm = T) > 0) %>% climate.occ2[.,]
# Remove species with < 10 presence records out of species name vector.
spname <- spname[(climate.occ3[, spname] %>% colSums(., na.rm = TRUE)) > 9]
# Remove wrong species names
if (genus_name == "Acaena"){
  spname <-  spname[spname!="Acaena_microphylla"] 
}

### Generate pseudo-absence
### I assume cells with any other species than the target species as the cells where the target species was absent. 
for(i in spname){climate.occ3[is.na(climate.occ3[, i]), i] <- 0}

# Environmental variables extracted from BIOCLIM and converted into NZTM.
myExpl <- stack(data.ras[c("bioclim1", "bioclim6", "bioclim12", "bioclim15")])




##################################################################################
## Find the best number of folds and block size
##################################################################################

# Convert raster stuck to raster brick
bioclim.ras <- raster::brick(data.ras[c("bioclim1","bioclim6","bioclim12","bioclim15")])

# Get range size
sac.sp <- spatialAutoRange(rasterLayer = bioclim.ras, # raster file
                           sampleNumber = 50000, # number of cells to be used
                           doParallel = TRUE,
                           showPlots = TRUE)
plot(sac.sp$variograms[[1]])

#### How do you find the best?

# explore generated folds
foldExplorer(blocks = sb.sp, 
             rasterLayer = bioclim.ras, 
             speciesData = pa_data.sp)

# add species data to add them on the map
rangeExplorer(rasterLayer = bioclim.ras,
              speciesData = pa_data.sp,
              species = "spData",
              rangeTable = NULL,
              minRange = 100000, # limit the search domain
              maxRange = 1000000)



#########################################################
## Test
#########################################################
setwd("Y://BIOMOD for Grid2")

## test

blockCV_biomod <- function(i, # Species number
                           folder.name, # folder name for BIOMOD
                           range # block size for cross validation 
                           ){
  
  sp = spname[i]
  ## presence/absence vector
  spData <- climate.occ3[, grepl(sp, colnames(climate.occ3))]
  pa_data.sp <- cbind(spData, climate.occ3[, c("NZTMlon", "NZTMlat")])
  colnames(pa_data.sp)[-1] <- c("x","y")
  pa_data.sp <- SpatialPointsDataFrame(pa_data.sp[,c("x", "y")], pa_data.sp, proj4string=crs(bioclim.ras))
  
  ## response name = species name
  myRespName <- sp
  
  ## SET THE RESPONSE VARIABLE
  myResp <- as.numeric(spData)
  
  ## SET THE COORDINATES
  myRespXY <- climate.occ3[, c("NZTMlon", "NZTMlat")]
  
  ##################################################################################
  ## Visualize spatial autocorrelation
  ##################################################################################

  
  # spatial blocking by specified range with random assignment
  sb.sp <- spatialBlock(speciesData = pa_data.sp,
                        species = "spData",
                        rasterLayer = bioclim.ras,
                        theRange = sac.sp$range, # size of the blocks
                        k = 5,
                        selection = "random",
                        iteration = 250, # find evenly dispersed folds
                        biomod2Format = TRUE,
                        xOffset = 0, # shift the blocks horizontally
                        yOffset = 0)
  
  
  
  # adding points on saptialBlock plot
  sb.sp$plots + geom_point(data = as.data.frame(coordinates(pa_data.sp)), aes(x=x, y=y), alpha=0.6)
  
  #########################################################
  ## BIOMOD
  #########################################################
  
  ## FORMAT THE DATA FOR BIOMOD
  ## USING THE DEFAULTNAMES FOR VARIABLES THAT WE HAVE SPECIFIED ABOVE
  myBiomodData <- BIOMOD_FormatingData(
    resp.var = myResp,
    resp.xy = myRespXY,
    expl.var = myExpl,
    resp.name = myRespName
  )
  
  ## DEFINE THE MODEL OPTIONS: BELOW SETS THEM TO ALLTHE DEFAULTS
  myBiomodOption <- BIOMOD_ModelingOptions()
  
  # 2. Defining the folds for DataSplitTable
  # note that biomodTable should be used here not folds
  DataSplitTable <- sb.sp$biomodTable # use generated folds from spatialBlock in previous section
  
  
  ## RUN THE MODELS
  myBiomodModelOut <- BIOMOD_Modeling(myBiomodData,
                                      models = c('MARS', 'RF', 'ANN', 'GBM', 'GAM', 'GLM', 'SRE'
                                      ),
                                      models.options = myBiomodOption,
                                      # If DataSplitTable is filled, args NbRunEval, DataSplit and do.full.models will be ignored.
                                      DataSplitTable = DataSplitTable, # blocking folds
                                      VarImport = 100,
                                      # For the difference between all the evaluation methods. See https://rdrr.io/cran/biomod2/man/BIOMOD_Modeling.html
                                      models.eval.meth = c('TSS'),
                                      modeling.id = folder.name
  )
  
  # 5. Model evaluation
  # get all models evaluation
  myBiomodModelEval <- get_evaluations(myBiomodModelOut)
  myBiomodModelEval["TSS","Testing.data",,,]
  
  ########################################################
  # Comapre the importance with non spatial models
  ########################################################
  
  ## PLOTS THE PROJECTIONS
  setwd("Y:\\BIOMOD for Grid2")
  # Get folder names
  folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]
  
  # Names of BIOMOD models
  folder.name = "7Nov18"
  BIOMODproj.name = folder.name
  
  # Load BIOMOD.model.out
  mod <- load(paste(".\\", folders[i], "\\", folders[i], ".", folder.name, ".models.out",
                    sep = "")
  )
  model <- get(mod)
  
  ### Averaged ranks of Variable importance
  # non spatial BIOMOD
  imp <- as.data.frame(model@variables.importances@val)
  # Rank
  imp.rank <- apply(-imp, 2, rank, ties.method = "min")
  biomod <- apply(imp.rank, 1, mean)
  
  ### blockCV
  # Extract variable importances
  imp.bl <- as.data.frame(myBiomodModelOut@variables.importances@val)
  # Rank
  imp.rank.bl <- apply(-imp.bl, 2, rank, ties.method = "min")
  block <- apply(imp.rank.bl, 1, mean)
  
  print(spname[i])
  return(cbind(block, biomod))
  
}



test <- list()
for(i in 1:18){
  
  test[[i]] <- tryCatch(
    blockCV_biomod(i, folder.name = "blockCV_04Feb19"),
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
} 


########################################################
# Comapre the importance with non spatial models
########################################################
# character string of target genus name
genus_name <- "Acaena"

### Set arguments
# data frame of occurrence data and climate data
if (genus_name == "Acaena"){
  datapath <- "Y://1st chapter_Acaena project//Acaena manuscript//meta data//Acaena_bioclim_landcover_history_worldclim1_1km.csv"
}else{
  datapath <- "Y://Chionochloa_bioclim_landcover_history_worldclim1_1km.csv"
}

# Import data frame of bioclim and occurrence records
climate.occ <- read.csv(datapath)

# Get species name
spname <- colnames(climate.occ)[grepl(genus_name, colnames(climate.occ))]

## PLOTS THE PROJECTIONS
setwd("Y:\\BIOMOD for Grid2")
# Get folder names
folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]

# Names of BIOMOD models
folder.name = "7Nov18"
BIOMODproj.name = folder.name

compare.var.imp <- function(i){
  # Load BIOMOD.model.out
  mod <- load(paste(".\\", folders[i], "\\", folders[i], ".", folder.name, ".models.out",
                    sep = "")
  )
  model <- get(mod)
  
  ### Averaged ranks of Variable importance
  # non spatial BIOMOD
  imp <- as.data.frame(model@variables.importances@val)
  # Rank
  imp.rank <- apply(-imp, 2, rank, ties.method = "min")
  biomod <- apply(imp.rank, 1, mean)
  
  ### blockCV
  # Names of BIOMOD models
  folder.name = "blockCV_04Feb19"
  BIOMODproj.name = folder.name
  
  # Load BIOMOD.model.out
  mod <- load(paste(".\\", folders[i], "\\", folders[i], ".", folder.name, ".models.out",
                    sep = "")
  )
  model <- get(mod)
  # Extract variable importances
  imp.bl <- as.data.frame(model@variables.importances@val)
  # Rank
  imp.rank.bl <- apply(-imp.bl, 2, rank, ties.method = "min")
  block <- apply(imp.rank.bl, 1, mean)
  
  print(spname[i])
  return(cbind(block, biomod))
  
}

res <- list()
for(i in 1:length(folders)){
  res[[i]] <- tryCatch(
    compare.var.imp(i),
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    )
    }

names(res) <- folders[1:length(res)]

# Difference between blockCV and non spatial BIOMOD
diff.model <- sapply(res, function(x){
  x[,"block"] - x[,"biomod"]
})

res3 <- t(
  as.data.frame(do.call(cbind, diff.model))
     )

boxplot(res3, main = "Difference of variable importance between blockCV and non spatial BIOMOD")

### Merge the list components
res.mat<-list()
for(i in 1:length(res)){ 
  res.mat[[i]] <- cbind(res[[i]], diff.model[[i]])
  }

res2 <- as.data.frame(do.call(cbind, res.mat[sapply(res, is.null) == FALSE]))
# Colnames
sp.block <- names(res)[sapply(res, is.null) == FALSE]
colnames(res2) <- lapply(sp.block,rep,3) %>% lapply(., paste, c("block","biomod","diff"), sep="_") %>% unlist

write.csv(t(res2), file="Y://compare_biomod_blockcv.csv")
