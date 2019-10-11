#################################################################################################################
###   Show thresholds that are used for binary conversion of Ensemble model projection     
#################################################################################################################

library(dplyr)
library(biomod2)

genus_name = "Chionochloa" # Acaena

setwd("Y:\\BIOMOD for Grid2")
# Get folder names of ensemble models
folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]

#### Function to load the models
loadEnsembleModel <- function(spname, # species name
                               ensambleModel.name
) {

  # Load BIOMOD.ensemble.model.out
  EMmod <- load(paste(".\\", spname, "\\", spname, ".", ensambleModel.name, "ensemble.models.out",
                    sep = "")
  )
  myBiomodEM <- get(EMmod)

  return(myBiomodEM)

}


# Repeat for all species
cutoff <- list()
for(i in folders){
  tryCatch(
    {
      
      EM1 <- loadEnsembleModel(i, ensambleModel.name = "SAIdiff_4Mar19" 
                               )
      # You can Check variables used to build the model in the output of this model object
      print(EM1@expl.var.names)
      
      cutoff[i] <- get_evaluations(EM1)
      
      },
    
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
    
  )
}

# Show thresholds (it's called cutoff in BIOMOD objects)
sapply(cutoff, function(x){
  return(x[2])
}
)


#################################################################################################################
###   Show histograms of Ensemble model projection     
#################################################################################################################

ensambleProj.name = "SAIdiff_4Mar19_ensamble"
binary = "prob"

load(paste("Y://ensemblePredictionBinary_", genus_name, ensambleProj.name, binary, ".data", sep = ""))

par(mfrow = c(3,3))

for(i in folders){
  tryCatch(
  {
    hist(values(pred[[i]]), main = i, xlab = "probability")
    abline(v = cutoff[i][[1]][2], col = "red")
  }, error = function(e){}
)
}

