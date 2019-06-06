#########################################################################
### Calculate range filling 
#########################################################################

genus_name <- "Chionochloa"

library(dismo)
library(dplyr)

# Model names to extract predictions from
ensambleProj.name = "SAIdiff_4Mar19_ensamble"
resolution = 5

# Load ensamble projection data
scores.prob <- get(load(paste("Y://ensemblePredictionBinary_", genus_name, ensambleProj.name, "binary.data", sep = "")))

# Load PCA scores of occurrence data
if(resolution == 1){
  if(file.exists(paste("Y:\\5th chapter SAI chapter\\raw data\\Scores_", genus_name, "_landcover.data", sep = "")) == FALSE){
  source("C:\\Users\\nomur\\Documents\\Range fillings\\03_generate_PCAscores.r")
}else{
  load(paste("Y:\\5th chapter SAI chapter\\raw data\\Scores_", genus_name, "_landcover.data", sep = "")) 
}

}else{
  if(file.exists(paste("Y:\\5th chapter SAI chapter\\raw data\\Scores_", genus_name, "_landcover_worldclim1_5km.data", sep = "")) == FALSE){
    source("C:\\Users\\nomur\\Documents\\Range fillings\\03_generate_PCAscores_5km.r")
  }else{
    load(paste("Y:\\5th chapter SAI chapter\\raw data\\Scores_", genus_name,"_landcover_worldclim1_5km.data", sep = "")) 
  }
}

colnames(scores)[grep(paste("^", genus_name, sep = ""), colnames(scores))] <- gsub("_", "\\.", 
                                                                                   colnames(scores)[grep(paste("^", genus_name, sep=""), colnames(scores))])

spname <- names(scores.prob)

# #########################################################################
# ### Range filling as the ratio of observed vs. predicted occurrences
# #########################################################################
# 
# rangefilling <- list()
# for(i in spname){
#   tryCatch(
#     {
#       predictedP <- (values(scores.prob[[i]]) == 1) %>% sum(., na.rm = T)
#       # Range filling 
#       rangefilling[[i]] <- (sum(scores[,i] == 1)/ predictedP)
#       rangefilling[[i]] <- cbind(rangefilling[[i]], sum(scores[,i] == 1), predictedP)
#     },
#     error = function(e){cat("ERROR :",conditionMessage(e), "\n")}
#   )
#   
# }
# 
# rangefilling2 <- do.call(rbind, rangefilling)
# rangefilling3 <- data.frame(names(rangefilling), rangefilling2)
# colnames(rangefilling3) <- c("spname", "rangefilling", "occurrence", "predictedOccurrence")
# 
# write.csv(rangefilling3, file = paste("Y://rangefilling_", resolution,"km", genus_name, ensambleProj.name,".csv", sep=""))

#########################################################################
### Range filling as the ratio of true positive 
#########################################################################

rangefilling <- list()
for(i in spname){
  tryCatch(
    {
      predictedP <- values(scores.prob[[i]])
      predictedP <- predictedP[!is.na(predictedP)]
      
      obs <- cbind(scores[,i], predictedP)
      
      # Range filling 
      rangefilling[[i]] <- sum(ifelse(obs[,1]==1 & obs[,2]==1, 1, 0)) / sum(predictedP, na.rm = T)
      rangefilling[[i]] <- c(sum(scores[,i] == 1), rangefilling[[i]][1])
    },
    error = function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  
}

rangefilling2 <- do.call(rbind, rangefilling)
rangefilling3 <- data.frame(cbind(rownames(rangefilling2), rangefilling2))
colnames(rangefilling3) <- c("spname", "occurrence", "rangefilling")

write.csv(rangefilling3, file = paste("Y://rangefilling_within_obs", resolution,"km", genus_name, ensambleProj.name,".csv", sep=""))
