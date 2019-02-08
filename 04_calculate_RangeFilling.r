#########################################################################
### Calculate range filling 
#########################################################################

genus_name <- "Acaena"  # "Chionochloa"

library(dismo)
library(dplyr)
proj.name = "SAI_cinl7Feb19_ensamble"

# Load ensamble projection data
scores.prob <- get(load(paste("Y://ensemblePredictionBinary_", genus_name, proj.name,".data", sep = "")))

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
### Range filling as the ratio of observed vs. predicted occurrences
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

write.csv(rangefilling3, file = paste("Y://rangefilling_", genus_name, proj.name,".csv", sep=""))

###################################################################################################
### Range filling as the overlap between observed and predicted occurrences by ecospat
###################################################################################################

# Go to ".//Rnage fillings//Ecospat_range_nicheOverlap_calculation.R"

