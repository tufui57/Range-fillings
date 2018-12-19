####################################################################
### Niche overlap
####################################################################

library(ecospat)
library(raster)

genus_name <- "Acaena"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

# Import species occurrence data
# source(".//functions//F02_clean_up_species_records18Sep.R")
load("Y://Scores_Acaena_landcover18sep.data")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_name, "12Dec.data", sep = ""))


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
spname <- colnames(scores)[grep(paste("^", genus_name, sep = ""), colnames(scores))]

D <- list()

# Calculate range overlap
for(i in 1:length(spname)){
  
  # PC scores of Observed occurrence records
  data1 = scores[scores[, spname[i]] == 1, ]
  # PC scores of predicted probability
  data2 = cbind(coordinates(pred[[i]]), values(pred[[i]]))
  data3 <- data2[!is.na(data2[,3]),]
  # Change colnames
  colnames(data3) <- c("x","y","prob")
  # Range overlap between 
  D[[i]] <- SchoenerD_ecospat(background, "x", "y", data1, data3)
}

d2 <- sapply(D, "[[", 1) %>% t %>% data.frame
d2$spname <- spname

write.csv(d2, "Y://ecospat_rangeOverlap.csv")

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

write.csv(d2, "Y://ecospat_nicheOverlap.csv")
