#################################################################################################################
### Backward stepwise variable selection for GLM for target species
#################################################################################################################

library(dplyr)
library(mgcv)
library(MuMIn)
library(reshape2)
library(ggplot2)

model.type <- "GLM"

genus_name <- "Nothofagus"

# Load data and calcualte AIC weights
source(".\\GitHub\\Range-fillings\\Spatial autocorrelation\\Varibale_Importance_AICweights.R")

melt.dat <- melt(dat[, c("AMT","MTC","AP","PS","EPcc", "EPcl", "spname")])
melt.dat$Genus <- rep(genus_name, nrow(melt.dat))

genus_name <- "Acaena"

# Load data and calcualte AIC weights
source(".\\GitHub\\Range-fillings\\Spatial autocorrelation\\Varibale_Importance_AICweights.R")

melt.dat.aca <- melt(dat[, c("AMT","MTC","AP","PS","EPcc", "EPcl", "spname")])

melt.dat.aca$Genus <- rep(genus_name, nrow(melt.dat.aca))

genus_name <- "Chionochloa"

# Load data and calcualte AIC weights
source(".\\GitHub\\Range-fillings\\Spatial autocorrelation\\Varibale_Importance_AICweights.R")

melt.dat.chi <- melt(dat[, c("AMT","MTC","AP","PS","EPcc", "EPcl", "spname")])

melt.dat.chi$Genus <- rep(genus_name, nrow(melt.dat.chi))

melt.dat2 <- rbind(melt.dat, melt.dat.aca, melt.dat.chi)

# Plot
ggplot() + 
  geom_boxplot(data = melt.dat2, mapping = aes(variable, value, fill = Genus))  + 
  ylim(0, 1) +
  ggtitle(paste("Sum of AIC weights of", model.type)) +
  xlab("") +
  ylab("Variable Importance")

ggsave(paste("Y://",  model.type,"_importance_sumAICweights.png", sep=""))





#################################################################################################################
#### Plot variable importance by sepcies range size
#################################################################################################################

genus_name <- "Chionochloa"

sp.occ <- read.csv(paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))
sp.occ$spname <- gsub("_",".", sp.occ$spname)

if(genus_name == "Acaena"){
  melt.dat2 <- melt.dat.aca
  range.size <- c(50, 100)
}

if(genus_name == "Chionochloa"){
  melt.dat2 < - melt.dat.chi
  range.size <- c(100, 250)
}

melt.dat2$Range <- ifelse(melt.dat2$spname %in% (sp.occ[sp.occ$sp.occ < range.size[1], "spname"]), "Small", 
                             ifelse(melt.dat2$spname %in% (sp.occ[sp.occ$sp.occ < range.size[2], "spname"]) , "Middle", "Large")
)


ggplot() + 
  geom_boxplot(data = melt.dat2, mapping = aes(variable, value, fill = Range))  + 
  ylim(0, 1) +
  ggtitle(paste(genus_name, "Sum of AIC weights of", model.type)) +
  xlab("") +
  ylab("Variable Importance")

ggsave(paste("Y://", genus_name, model.type,"_importance_sumAICweights.png", sep=""))


