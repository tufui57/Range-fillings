#########################################################################
### Calculate range filling 
#########################################################################

genus_name <- "Acaena"  # "Chionochloa"
genus_tag <- "acaena" # "chion"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")

# Load ensamble projection data
scores.prob <- get(load(paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = "")))

# Load PCA scores of occurrence data
if(file.exists(paste(".\\Scores_", genus_name, "_landcover.data", sep = "")) == FALSE){
  source(".//Range fillings//03_generate_PCAscores.r")
}else{
  load(paste(".\\Scores_", genus_name, "_landcover.data", sep = "")) 
}

colnames(scores)[grep(paste("^", genus_name, sep = ""), colnames(scores))] <- gsub("_", "\\.", 
                                                                                   colnames(scores)[grep(paste("^", genus_name, sep=""), colnames(scores))])

spname <- names(scores.prob)

#########################################################################
### Range filling 
#########################################################################

rangefilling <- list()
for(i in spname){
  tryCatch(
    {
      predictedP <- (values(scores.prob[[i]]) == 1) %>% sum(., na.rm = T)
      # Range filling 
      rangefilling[[i]] <- (sum(scores[,i] == 1)/ predictedP)
      rangefilling[[i]] <- cbind(rangefilling[[i]], sum(scores[,i] == 1), predictedP)
    },
    error = function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  
}

rangefilling2 <- do.call(rbind, rangefilling)
rangefilling3 <- data.frame(names(rangefilling), rangefilling2)
colnames(rangefilling3) <- c("spname", "rangefilling", "occurrence", "predictedOccurrence")

write.csv(rangefilling3, file = paste(".//rangefilling_", genus_name, ".csv", sep=""))