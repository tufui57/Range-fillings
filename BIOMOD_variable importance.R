library(dplyr)
library(biomod2)

genus_name = "Acaena"

## PLOTS THE PROJECTIONS
setwd("Y:\\BIOMOD for Grid2")
# Get folder names
folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]

# Names of BIOMOD models
folder.name = "SAI_28Feb19"
BIOMODproj.name = "SAI_28Feb19"

# Calculate rank
ave.imp.rank <- list()

for(i in folders){
  
  try(
    {
      # Load BIOMOD.model.out
      mod <- load(paste(".\\", i, "\\", i, ".", folder.name, ".models.out",
                        sep = "")
      )
      model <- get(mod)
      
      # Extract variable importances
      imp <- as.data.frame(model@variables.importances@val)
      
      # Rank
      imp.rank <- apply(-imp, 2, rank, ties.method = "min")
      
      # Averaged rank for each variable
      ave.imp.rank[[i]] <- apply(imp.rank, 1, mean)
    }
    
  )

}

# Create a data frame
ave.imp.rank.data <- do.call(rbind, ave.imp.rank)

write.csv(ave.imp.rank.data, file = paste("Y://averaged_rank_importance_", genus_name, folder.name, ".csv", sep=""))

# Rank the averaged rank
rank.ave <- do.call(rbind, lapply(ave.imp.rank, rank, ties.method = "min"))

apply(rank.ave, 2, mean)



