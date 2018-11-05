######################################################################################################
### Calculate geographical potential range overlap between sister sepcies
######################################################################################################

genus_name <- "Chionochloa"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")
source(".//Chionochloa niche evolution//Niche filling//F_get_probability.r")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

####################################################################
### Potential niche volume
####################################################################

# Import actual niche volume
actualvol <- read.csv(paste("NicheVolume_age_", genus_tag, ".csv", sep = ""))

# Create imaginary speices occurring at all cells in NZ
prob1 <- get_occurrenceProbability_to_scores(spname[1], pred)
ras1 <- raster_from_dataframe(prob1)
rasall <- ras1
values(rasall) <- ifelse(is.na(values(ras1)), NA, 1000)

# Calculate niche volume
vold <- list()

for(i in spname){
  
  prob1 <- get_occurrenceProbability_to_scores(i)
  
  # dismo::nicheOverlap() requires raster of probability
  rassp <- raster_from_dataframe(prob1)
  
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


