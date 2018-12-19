####################################################################
### Niche overlap
####################################################################

library(ecospat)
library(raster)
library(nichePlot)

genus_name <- "Acaena"

# Import species occurrence data
# source(".//functions//F02_clean_up_species_records18Sep.R")
load("Y://Scores_Acaena_landcover18sep.data")

# # Load ensamble projection data
# load(paste("Y://ensemblePrediction_", genus_name, "12Dec.data", sep = ""))
# Load ensamble projection data
load(paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = ""))

####################################################################
### Calculate niche overlap by ecospat
####################################################################

### Create background object comprising of all cells across NZ
background <- scores[, c("x", "y", "PC1","PC2")]
background[, "prob"] <- rep(1, nrow(background))

spname <- colnames(scores)[grep(paste("^", genus_name, sep=""), colnames(scores))]

D <- list()

for(i in 1:length(spname)){
  # Prepare observed data
  data1 = scores[scores[, spname[i]] == 1, ]
  
  # Prepare prediction data
  data2 = cbind(coordinates(pred[[i]]), values(pred[[i]]))
  
  # Probability should be dicimal
  data2[,3] <- data2[,3] / 1000
  colnames(data2)[3] <- "prob"
  
  # Bind PC scores to prediction data by coodinates
  data3 <- merge(background, data2, by = c("x", "y")) 
  
  colnames(data3)[6] <- "prob"
  
  data3 <- data3[,c("PC1","PC2","prob")]
  
  # Calculate niche overlap
  D[[i]] <- SchoenerD_ecospat(background, "PC1","PC2", data1, data3)
}

d2 <- sapply(D, "[[", 1) %>% t %>% data.frame
d2$spname <- spname

write.csv(d2, file = paste(".//nichefilling_", genus_name, ".csv", sep=""))