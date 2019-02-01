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
### Test the relationship between range filling and current climate
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
# Range filling is calculated based on climate. 
# To test this hypothesis, use another something related to climate.

aca <- read.csv("Y:\\Analysis_acaena.csv")
aca$spname <- gsub("_", "\\.", aca$spname)

aca2 <- merge(dat.genus[[1]], aca, by.x="X", by.y = "spname")

write.csv(aca2, file = "Analysis_Acaena_SAI.csv")

### Each genus
## Acaena
# Range filling ~ phylogeny
summary(lm(aca2$rangefilling ~ aca2$speciesAge))

# Range size ~ phylogeny
summary(lm(aca2$occurrence ~ aca2$speciesAge))

## Chion

chi <- read.csv("Y:\\Analysis_chion.csv")
chi$spname <- gsub("_", "\\.", chi$spname)

chi2 <- merge(dat.genus[[2]], chi, by = "spname")

write.csv(chi2, file = "Analysis_Chionochloa_SAI.csv")

# Range filling ~ phylogeny
summary(lm(chi2$rangefilling ~ chi2$speciesAge))

# Range size ~ phylogeny
summary(lm(chi2$occurrence ~ chi2$speciesAge))
