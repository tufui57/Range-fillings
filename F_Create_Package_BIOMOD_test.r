#################################################################################################################
########################        Data formatting & Modelling
#################################################################################################################


runBiomod <- function(
  data, # List object of data frame containg presence/absence column
  i, # Species name
  folder.name,
  myExpl # Raster stack of environmental variables
 ) {
    ## presence/absence vector
    spData <- data[, grepl(i, colnames(data))]

    ## SET THE NAME - THIS IS A CHARACTER STRING THAT JUST HAS THE NAME OF THE SPECIES
    ##  USED FOR NAMING FILES AND FIGS
    myRespName <- i

    ## SET THE RESPONSE VARIABLE
    myResp <- as.numeric(spData)

    ## SET THE COORDINATES
    myRespXY <- data[, c("NZTMlon", "NZTMlat")]


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

    ## RUN THE MODELS
    myBiomodModelOut <- BIOMOD_Modeling(myBiomodData,
                                     models = c('MARS', 'RF', 'ANN', 'GBM', 'GAM', 'GLM', 'SRE' #,'CTA', 'FDA',
                                                ),
                                     models.options = myBiomodOption,
                                     NbRunEval = 5, # How many times evaluation is implemented.
                                     DataSplit = 75, # Split data for training models. The rest of data will be used for validation.
                                     VarImport = 100,
                                     
                                     # For the difference between all the evaluation methods. See https://rdrr.io/cran/biomod2/man/BIOMOD_Modeling.html
                                     models.eval.meth = c('TSS'),
                                     rescal.all.models = FALSE,
                                     do.full.models = TRUE,
                                     modeling.id = folder.name
                                     )

    ##########################################################
    ## NOTE! MAKE SURE TEMPORARY RASTER FOLDER IS NOT FULL! ##
    ##########################################################

    # clear the folder 
    tempFilePath <- list.files(rasterOptions()$tmpdir, full.names = T)
    file.remove(tempFilePath)


}



biomodProjection_fromSavedBiomodModels <- function(foldername,
                                                   modelname, # Same name used as folder.name in runBiomod function
                                                   proj.name # Name of projection
                                                   ) {

    # Path of .models.out file
    biomodModelPath <- paste(foldername, "/", foldername, ".", modelname, ".models.out", sep = "")


    # Load the BIOMOD.models.out object, and store the loaded object in a specific variable (myBiomodModelOut)
    x = load(biomodModelPath)
    myBiomodModelOut <- get(x)
    # Remove the old object since you've stored it in y 
    rm(x)

    myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
        new.env = myExpl,
        proj.name = proj.name,
        selected.models = 'all',
        # models.eval.meth should be less than 3,  because of temporary raster data folder size.
        binary.meth = c('TSS', 'ROC', 'ACCURACY'),
        compress = FALSE,
        build.clamping.mask = FALSE)

    ##########################################################
    ## NOTE! MAKE SURE TEMPORARY RASTER FOLDER IS NOT FULL! ##
    ##########################################################

    # clear the folder 
    tempFilePath <- list.files(rasterOptions()$tmpdir, full.names = T)
    file.remove(tempFilePath)

}
