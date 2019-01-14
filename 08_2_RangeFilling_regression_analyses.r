###################################################################################
### Range filling analyses
###################################################################################

####################################################
# Compare range/niche filling between Acaena and Chionochloa
####################################################

aca <- read.csv("Y://rangefilling_comparison_Acaena.csv")
chi <- read.csv("Y://rangefilling_comparison_Chionochloa.csv")

### Compare range filling between genera
var.test(aca$rangefilling, chi$rangefilling)

t.test(aca$rangefilling, chi$rangefilling, var.equal = FALSE)

### Compare niche filling between genera
var.test(aca$nichefilling, chi$nichefilling)

t.test(aca$nichefilling, chi$nichefilling, var.equal = FALSE)

####################################################
# Range filling ~ SAI + dispersal traits
####################################################

### Calculate averaged SAI over species occurrence cells

### Load data
# Load 5km SAI of current climate at the LGM
load("SAI_5km_LGM_PC1_2.data")

dat.genus <- list()
for(i in c("Acaena", "Chionochloa")){
  # Load PCA scores
  load(paste(".//Scores_", i, "_landcover5km.data", sep=""))
  
  # Combine SAI to coordinate data
  sai.dat <- cbind(scores[, c("x", "y")], unlist(sai))
  colnames(sai.dat)[3] <- "AUC"
  
  # Species names
  spname <- colnames(scores)[grep(paste("^", i, sep = ""), colnames(scores))]
  
  ### Average SAI
  mean.sai <- sapply(spname, function(i){
    occ.sai <- sai.dat[scores[,i] == 1, ]
    mean(occ.sai[,3])
  }
  )
  
  if(i == "Acaena"){
    dat.genus[[i]] <- cbind(aca, mean.sai, rep("animal", nrow(aca)))
  }else{
    dat.genus[[i]] <- cbind(chi, mean.sai, rep("gravity", nrow(chi)))
  }
  
  
  colnames(dat.genus[[i]])[ncol(dat.genus[[i]])] <- "dispersal"

}


dat <- rbind(dat.genus[[1]][, c("rangefilling", "occurrence", "nichefilling", "mean.sai", "dispersal")], 
             dat.genus[[2]][, c("rangefilling", "occurrence",  "nichefilling", "mean.sai", "dispersal")]
)

### Regression
summary(lm(dat$rangefilling ~ dat$mean.sai + dat$dispersal))

########################################################################################################
### Isn't the significance just because range filling and SAI are based on climate?
########################################################################################################

# Range filling is calculated based on climate. 
# To test this hypothesis, use another something related to climate.

median <- read.csv("Y:\\1st chapter_Acaena project\\Acaena manuscript\\meta data\\Acaena_data_analyses18sep.csv")

test <- cbind(dat.genus[[1]], median)

# Not everything climate-related is correlated with range filling, but temperature-related variable is.
summary(lm(test$rangefilling ~ test$median.of.temp))
summary(lm(test$rangefilling ~ test$mean.sai))

# However, multi-colinearity of the climate-related varibales isn't high.
library(car)
vif(lm(test$rangefilling ~ test$median.of.temp + test$median.of.prec + test$mean.sai))

########################################################################################################
### Does phylogeny explain Range or niche filling better?
########################################################################################################

# Range filling ~ phylogeny
summary(lm(test$rangefilling ~ test$speciesAge))
# Niche filling ~ phylogeny
summary(lm(test$nichefilling ~ test$speciesAge))

# Range size ~ phylogeny
summary(lm(test$occurrence ~ test$speciesAge))

########################################################################################################
### Does phylogeny explain niche filling better than current niche volume?
########################################################################################################
# Niche volume ~ phylogeny
summary(lm(test$niche_volume ~ test$speciesAge))

# Niche filling ~ phylogeny
summary(lm(test$nichefilling ~ test$speciesAge))
