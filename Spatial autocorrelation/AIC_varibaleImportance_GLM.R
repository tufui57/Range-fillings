#################################################################################################################
### Backward stepwise variable selection for GLM for target species
#################################################################################################################

library(dplyr)
library(mgcv)
library(MuMIn)
library(reshape2)

genus_name <- "Chionochloa"

model.type <- "GLM"


# Import species occurrence data
load(paste(".//Scores_", genus_name, "_landcover_worldclim1_5km.data", sep = ""))

# Load EPcc
load(".//EPcc_NZ_4var_test.data")
epcc <- load(".//EPcc_NZ_4var_test.data")
epcc <- get(epcc)
colnames(epcc)[ncol(epcc)] <- "EPcc"

# Load EPcl
load(".//EPcl_NZ_4var.data")
epcl <- load(".//EPcl_NZ_4var.data")
epcl <- get(epcl)
colnames(epcl)[ncol(epcl)] <- "EPcl"

spname <- colnames(scores)[grepl(paste("^", genus_name, sep = ""), colnames(scores))]

scores.ep <- merge(scores, epcc[, c("x", "y", "EPcc")], by = c("x","y")) %>% 
  merge(., epcl[, c("x","y","EPcl")], by = c("x","y"))

#################################################################################################################
### Backward stepwise variable selection for glm for target species
#################################################################################################################

predictor <- c(paste("bioclim", c(1,6,12,15), sep = ""), "EPcc","EPcl")


imp <- list()

for(i in 1:length(spname)){
  
  if(model.type=="GLM"){
    m1 <- glm(as.formula(paste(spname[i] ,"~",
                               paste(predictor, collapse = "+", sep = "")
                               )
                         ),
              data = scores.ep, family=binomial(logit), na.action = "na.fail")
  
  
  }
  
  if(model.type=="GAM"){
    m1 <- gam(as.formula(paste(spname[i] ,"~",
                                 paste(predictor, collapse = "+", sep = "")
    )
    ),
    data = scores.ep, family=binomial(logit), na.action = "na.fail")
  }
  
  ms1 <- dredge(m1)
  
  # Importance can be calculated/extracted from various objects:
  imp[[i]] <- importance(ms1)
  
}




dat <- data.frame(do.call(cbind, imp)) %>% t %>% as.data.frame
dat$spname <- rownames(dat)
colnames(dat) <- c("AMT","MTC","AP","PS","EPcc", "EPcl", "spname")


melt.dat <- melt(dat[, c("AMT","MTC","AP","PS","EPcc", "EPcl")])

png(paste("Y://", genus_name, model.type,"_importance_sumWeightedAIC.png", sep=""))
par(cex.lab = 1.5, las = 3)
boxplot(melt.dat$value ~ melt.dat$variable,
        ylim = range(melt.dat$value),
        main = paste(genus_name, "Variable importance; sum of AIC weighted coefficients of", model.type),
        xlab = "",
        ylab = "Variable Importance"
)

dev.off()

