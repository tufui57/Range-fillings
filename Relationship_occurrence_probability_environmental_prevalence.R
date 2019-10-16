#################################################################################################################
###   Show environmental prevalence vs climatic suitability of species
#################################################################################################################
# How do I compare these two?


library(dplyr)
library(biomod2)

genus_name = "Chionochloa" # Acaena

## Plot ensemble projection
folders <- list.dirs("Y://BIOMOD for Grid2//", full.names = FALSE, recursive = F)
folders <- (grepl(genus_name, folders) %>% folders[.])

ensambleProj.name = "SAIdiff_4Mar19_ensamble"
binary = "prob"


# load climatic suitability of species
load(paste("Y://ensemblePredictionBinary_", genus_name, ensambleProj.name, binary, ".data", sep = ""))
load("Scores_Acaena_landcover5km.data")

# Load SAI values
load("EPcc_NZ_4var.data")
sai <- load("EPcc_NZ_4var.data")
sai <- get(sai)


# Linear test
for(i in folders){
tryCatch(
  {
    values <- na.omit(values(pred[[i]]))
    print(i)
    print(
      summary(
        lm(values ~ unlist(sai) + scores$bioclim1 + scores$bioclim16 + scores$bioclim12 + scores$bioclim15)
      )
    )
  }, error = function(e) e
    
    )
}

