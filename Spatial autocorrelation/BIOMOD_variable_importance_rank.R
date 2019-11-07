##########################################################################################
### Delta AIC as vvariable importance vs. variable importance by BIOMOD
##########################################################################################

library(dplyr)
library(biomod2)

genus_name = "Chionochloa"

## PLOTS THE PROJECTIONS
setwd("Y:\\BIOMOD for Grid2")
# Get folder names
folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]

# Names of BIOMOD models
folder.name = "SAI_cinl8Feb19"
BIOMODproj.name = "SAI_cinl8Feb19"

model.type <- "GLM"

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
      imp <- as.data.frame(model@variables.importances@val) # model@variables.importances@val == get_variables_importance(model)
      
      glm.imp <- imp[,grepl(model.type, colnames(imp))]
      
      # Rank
      imp.rank <- apply(-glm.imp, 2, rank, ties.method = "min")
      
      # Averaged rank for each variable
      ave.imp.rank[[i]] <- apply(imp.rank, 1, mean)
    }
    
  )
  
}

# Create a data frame
ave.imp.rank.data <- do.call(rbind, ave.imp.rank)


dat <- data.frame(do.call(cbind, ave.imp.rank)) %>% t %>% as.data.frame
dat$spname <- rownames(dat)
colnames(dat) <- c("AMT","MTC","AP","PS","EPcc", "EPcl", "spname")


library(reshape2)
melt.dat <- melt(dat[, c("AMT","MTC","AP","PS","EPcc", "EPcl")])

png(paste("Y://", genus_name, "_BIOMOD_",model.type,"_rank.png", sep=""))
par(cex.lab = 1.5, las = 3)
boxplot(melt.dat$value ~ melt.dat$variable,
        ylim = rev(range(melt.dat$value)),
        main = paste(genus_name, "Variable importance rank by BIOMOD", model.type),
        xlab = "",
        ylab = "Variable Importance rank"
)

dev.off()

