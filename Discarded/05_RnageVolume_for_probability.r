######################################################################################################
### Calculate geographical potential range overlap between sister sepcies
######################################################################################################

genus_name <- "Chionochloa" 
# genus_name <- "Acaena"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")
source(".//Chionochloa niche evolution//Niche filling//F_get_probability.r")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

####################################################################
### Actual range volume
####################################################################

# Import species occurrence data
source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//F02_clean_up_species_records.R")

### Get rid of species whose occurren resords < 5
dat2 <- dat[sapply(dat, nrow) >= 5]

# Create imaginary speices occurring at all cells in NZ
probAll <- pred[1]
values(probAll[[1]]) <- ifelse(is.na(values(probAll[[1]])), NA, 1)
plot(probAll[[1]])

# Get data frame of the imagenary species occcurrences
sp.all <- data.frame(
  cbind(coordinates(probAll[[1]]), values(probAll[[1]]))
)
colnames(sp.all)[1:2] <- c("lon", "lat")
# Get rid of cells which aren't on land
sp.all2 <- sp.all[!is.na(sp.all[,3]),]

# Calculate ratio of species habitat area; area of occurrence cells / NZ land area
D <- list()

for(i in spname[spname %in% sapply(tree$tip.label, clean_speciesname)]){
  D[[i]] <- nrow(dat2[[i]])/nrow(sp.all2)
  }

####################################################################
### Potential range volume
####################################################################

# Import actual niche volume
actualvol <- data.frame(cbind(names(unlist(D)), unlist(D)))
colnames(actualvol) <- c("spname", "rangeVolume")

# Calculate niche volume
vold <- list()

for(i in spname){
  
  # Get probability of the species
  prob1 <- pred[names(pred) == gsub("_",".", i)]
  
  ### Use dismo::nicheOverlap
  tryCatch(
  {
    vold[[i]] <- nicheOverlap(prob1[[1]], probAll[[1]], stat = 'D', mask = TRUE, checkNegatives = TRUE)
    }
  ,
  error = function(e) print(i)
  )
}


### Get vector of potential range values
volumed <- as.numeric(as.character(vold))

vols <- merge(actualvol, data.frame(cbind(spname, volumed)), by ="spname")

colnames(vols)[colnames(vols) == "volumed"] <- "potentialRangeVolume.D"

# Load species age
spage <- read.csv(paste("NicheVolume_age_", genus_tag,".csv", sep=""))
vols2 <- merge(vols, spage, by ="spname")

# Save the data
write.csv(vols2, paste("GeoVolume_potential_actual_", genus_tag, ".csv", sep = ""))

##############################################################################################
### Merge data of range and niche volume
##############################################################################################

# Save the data
vols <- read.csv(paste("GeoVolume_potential_actual_", genus_tag, ".csv", sep = ""))

# Load niche volume data
nichevol <- read.csv(paste("NicheVolume_potential_actual_", genus_tag, ".csv", sep = ""))
vols2 <- merge(vols, nichevol[, c("spname", "potentialNicheVolume.D")], by ="spname")

# Save the data
write.csv(vols2[, c("spname", "speciesAge", 
                    "rangeVolume", "potentialRangeVolume.D", 
                    "nicheVolume", "potentialNicheVolume.D")],
          paste("Niche_Range_Volume_", genus_tag, ".csv", sep = "")
)
