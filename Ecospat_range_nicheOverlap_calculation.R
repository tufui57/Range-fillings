####################################################################
### Niche overlap
####################################################################

library(ecospat)
library(raster)
library(nichePlot)
library(dplyr)

genus_name <- "Acaena"

# Import species occurrence data
# source(".//functions//F02_clean_up_species_records18Sep.R")
load(paste(".//Scores_", genus_name, "_landcover.data", sep=""))

# Load ensamble projection data
load(paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = ""))


####################################################################
### Calculate range overlap by ecospat
####################################################################

### Create background object comprising of all cells across NZ
probAll <- pred[1]
values(probAll[[1]]) <- ifelse(is.na(values(probAll[[1]])), NA, 1)

sp.all <- data.frame(
  cbind(coordinates(probAll[[1]]), values(probAll[[1]]))
)
background <- sp.all[!is.na(sp.all[,3]),]

colnames(background) <- c("x","y","prob")


# Get species name list
spname <- names(pred)

# Change name to be consistent with prediction data
colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))] <- 
  colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))] %>% gsub("_", ".", .)

D <- list()

# Calculate range overlap
for(i in spname){
  
  # if both prediction and observation data of the species is available
  if(sum(i %in% colnames(scores)) > 0 && (i %in% names(pred))){
    # PC scores of Observed occurrence records
    data1 = scores[scores[, i] == 1, ]
    # PC scores of predicted probability
    data2 = cbind(coordinates(pred[[i]]), values(pred[[i]]))
    data3 <- data2[!is.na(data2[,3]),]
    # Change colnames
    colnames(data3) <- c("x","y","prob")
    # Range overlap between 
    D[[i]] <- SchoenerD_ecospat(background, "x", "y", data1, data3)
  }else{
    D[[i]] <- NA
  }

}

### Format results
# Remove species with no Schoenner's D calculated
test <- lapply(D, "[[", 1) %>% sapply(., length)
# Get corrected Schoenner's D
d2 <- sapply(D[which(test!=1)], "[[", 1) %>% t %>% data.frame
d2$spname <- spname[which(test!=1)]

write.csv(d2, file = paste("Y://rangefilling_ecospat_", genus_name, ".csv", sep=""))


####################################################################
### Calculate niche overlap by ecospat
####################################################################

### Create background object comprising of all cells across NZ
background <- scores[, c("x", "y", "PC1","PC2")]
background[, "prob"] <- rep(1, nrow(background))

spname <- colnames(scores)[grep(paste("^", genus_name, sep=""), colnames(scores))]

D <- list()

for(i in spname){
  
  if(i %in% names(pred)){
      # Prepare observed data
  data1 = scores[scores[, i] == 1, ]
  
  # Prepare prediction data
  data2 = cbind(coordinates(pred[[i]]), values(pred[[i]]))
  
  # If you use probability data for overlap calculation, 
  if(any(data2[!is.na(data2[,3]),3] > 2)){
    # Probability should be dicimal
    data2[,3] <- data2[,3] / 1000
  }
  colnames(data2)[3] <- "prob"
  
  # Bind PC scores to prediction data by coodinates
  data3 <- merge(background, data2, by = c("x", "y")) 
  
  colnames(data3)[6] <- "prob"
  
  data3 <- data3[,c("PC1","PC2","prob")]
  
  # Calculate niche overlap
  D[[i]] <- SchoenerD_ecospat(background, "PC1","PC2", data1, data3)
  }else{
    
    print("No data")
    D[[i]] <- "NA"
  }

}

### Format results
# Remove species with no Schoenner's D calculated
test <- lapply(D, "[[", 1) %>% sapply(., length)
# Get corrected Schoenner's D
d2 <- sapply(D[which(test!=1)], "[[", 1) %>% t %>% data.frame
d2$spname <- spname[which(test!=1)]

write.csv(d2, file = paste("Y://ecospat_nicheOverlap_", genus_name, ".csv", sep=""))
