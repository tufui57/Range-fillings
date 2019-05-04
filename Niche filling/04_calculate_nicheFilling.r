####################################################################
### Niche overlap
####################################################################

library(dplyr)
library(ecospat)
library(raster)
library(nichePlot)

genus_name <- "Chionochloa"

# Import species occurrence data
# source(".//functions//F02_clean_up_species_records18Sep.R")
load(paste("Y://Scores_", genus_name,"_landcover.data", sep=""))

# Load ensamble projection data
load(paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = ""))

####################################################################
### Calculate niche overlap by ecospat
####################################################################

### Create background object comprising of all cells across NZ
background <- scores[, c("x", "y", "PC1","PC2")]
background[, "prob"] <- rep(1, nrow(background))
# Change name to be consistent with prediction data
colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))] <- 
  colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))] %>% gsub("_", ".", .)

spname <- names(pred)

D <- list()

for(i in 17:length(spname)){
  
  if(spname[i] %in% colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))] == TRUE){
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
  }else{
    
    # Either of prediction or occurrence data isn't provided.
    D[[i]] <- "NO DATA"
  }
  
}

test <- sapply(D, "[[", 1) %>% sapply(., length)


d2 <- sapply(D[which(test!=1)], "[[", 1) %>% t %>% data.frame
d2$spname <- spname[which(test!=1)]

write.csv(d2, file = paste(".//nichefilling_", genus_name, ".csv", sep=""))
