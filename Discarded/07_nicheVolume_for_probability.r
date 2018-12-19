######################################################################################################
### Calculate geographical potential range overlap between sister sepcies
######################################################################################################

genus_name <- "Chionochloa"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")
load(".//Scores_landcover1km.data")

source(".//functions//F02_create_raster_from_dataframe.R")
source(".//Range fillings//F_get_probability.r")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, "12Dec.data", sep = ""))

####################################################################
### Potential niche volume
####################################################################

# Create imaginary speices occurring at all cells in NZ
prob1 <- get_occurrenceProbability_to_scores(spname[1], pred)
ras1 <- raster_from_dataframe(prob1)
rasall <- ras1
values(rasall) <- ifelse(is.na(values(ras1)), NA, 1000)

# Calculate niche volume
vold <- list()

ref <- raster(
  # This raster was converted from pre-human raster with ArcGIS using "majority" method for raster value assignment
  paste("Y://GIS map and Climate data//newzealandpotentialvegetatio1.bil", sep="")
)

for(i in gsub("_", ".", spname)){
  
  prob1 <- get_occurrenceProbability_to_scores(i, pred, scores)
    
  # dismo::nicheOverlap() requires raster of probability
  rassp <- convert_dataframe_to_raster(ref, # reference raster
                                       prob1, # dataframe
                                       c("x","y"), # colnames of coordinates in "dat"
                                       paste("prob_", i, sep="")
                                       )
  
  tryCatch(
  {
    vold[[i]] <- nicheOverlap(rassp, rasall, stat = 'D', mask = TRUE, checkNegatives = TRUE)
  }
  ,
  error = function(e) print(i)
  )
}


### Get vector of potential range values
volumed <- as.numeric(as.character(vold))

vols <- merge(actualvol, data.frame(cbind(spname, volumed)), by ="spname")

colnames(vols)[colnames(vols) == "volumed"] <- "potentialNicheVolume.D"
# Save the data
write.csv(vols, paste("NicheVolume_potential_actual_", genus_tag, ".csv", sep = ""))


