
# data frame of occurrence data and climate data
if (genus_name == "Acaena"){
  datapath <- "Y://1st chapter_Acaena project//Acaena manuscript//meta data//Acaena_bioclim_landcover_history_worldclim1_1km.csv"
}else{
  datapath <- "Y://Chionochloa_bioclim_landcover_history_worldclim1_1km.csv"
}

dat1 <- read.csv(datapath)
d <- dat1[is.na(dat1$landCoverChange) == F, ]

# species names
sname <- colnames(d)[grepl(paste("^", genus_name, sep = ""), colnames(d))]

for(i in sname){
  d[is.na(d[,i]),i] <- 0
}



# get env. corrdinates (PCA axes)
pca <- prcomp(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
              center = TRUE,
              scale. = TRUE)
scores <- data.frame(d[, c(colnames(d)[grep("^bioclim", colnames(d))], sname,
                           "x", "y", "preLandcover", "currentLandcover", "landCoverChange")], pca$x[, 1:2])
scores$landCoverChange <- factor(scores$landCoverChange)
scores$pre <- factor(ifelse(scores$preLandcover == 1, "NF", "nonF"))
scores$post <- factor(ifelse(scores$currentLandcover == 1, "NF",
                             ifelse(scores$currentLandcover == 3, "nonF", "non potential habitat" # EF(2) is also non potential habitat
                             ))
)

### Save PCA scores
save(scores, file = paste(".\\Scores_", genus_name, "_landcover.data", sep = ""))
