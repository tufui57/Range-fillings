#################################################################################################################
### Backward stepwise variable selection for GLM for target species
#################################################################################################################

library(dplyr)
library(mgcv)
library(MuMIn)
library(reshape2)

genus_name <- "Nothofagus"

model.type <- "GLM"

if(genus_name == "Nothofagus"){
  # Import species occurrence data
  scores <- read.csv("Y://Nothofagus_in_nz.csv")
  spname <- colnames(scores)[grepl(paste("^", genus_name, sep = ""), colnames(scores))]
  
  scores <- scores[!is.na(scores[, spname[1]]), ]
  
}else{
  
  # Import species occurrence data
  load(paste(".//Scores_", genus_name, "_landcover_worldclim1_5km.data", sep = ""))
}

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

scores.ep <- merge(scores, epcc[, c("x", "y", "EPcc")], by = c("x","y")) %>% 
  merge(., epcl[, c("x","y","EPcl")], by = c("x","y"))

colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))] <-
  colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))] %>% gsub("_", ".", .)

spname <- colnames(scores.ep)[grepl(paste("^", genus_name, sep = ""), colnames(scores.ep))]


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
  imp[[i]] <- MuMIn::importance(ms1)
  
}




dat <- data.frame(do.call(cbind, imp)) %>% t %>% as.data.frame
dat$spname <- spname
colnames(dat) <- c("AMT","MTC","AP","PS","EPcc", "EPcl", "spname")


melt.dat <- melt(dat[, c("AMT","MTC","AP","PS","EPcc", "EPcl")])

png(paste("Y://", genus_name, model.type,"_importance_sumWeightedAIC.png", sep=""))
par(cex.lab = 1.5, las = 3)
boxplot(melt.dat$value ~ melt.dat$variable,
        ylim = range(melt.dat$value),
        main = paste(genus_name, "Variable importance; sum of AIC weights of", model.type),
        xlab = "",
        ylab = "Variable Importance"
)

dev.off()

#### Plot variable importance by sepcies range size

sp.occ <- read.csv(paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))
dat$spname <- gsub("_", ".", dat$spname)
dat2 <- merge(sp.occ, dat, by = "spname")

if(genus_name == "Acaena"){
  range.size <- c(50, 100)
}

if(genus_name == "Chionochloa"){
  range.size <- c(100, 250)
}

dat3 <- dat2[dat2$sp.occ < range.size[1], ]
melt.dat2 <- melt(dat3[, c("AMT","MTC","AP","PS","EPcc", "EPcl")])

png(paste("Y://Small-ranged", genus_name, model.type,"_importance_sumWeightedAIC.png", sep=""))
par(cex.lab = 1.5, las = 3)
boxplot(melt.dat2$value ~ melt.dat2$variable,
        ylim = range(melt.dat2$value),
        main = paste("Small-ranged", genus_name, "Variable importance; sum of AIC weights of", model.type),
        xlab = "",
        ylab = "Variable Importance"
)

dev.off()


dat3 <- dat2[dat2$sp.occ < range.size[2] & dat2$sp.occ > range.size[1], ]
melt.dat2 <- melt(dat3[, c("AMT","MTC","AP","PS","EPcc", "EPcl")])

png(paste("Y://Middle-ranged", genus_name, model.type,"_importance_sumWeightedAIC.png", sep=""))
par(cex.lab = 1.5, las = 3)
boxplot(melt.dat2$value ~ melt.dat2$variable,
        ylim = range(melt.dat2$value),
        main = paste("Middle-ranged", genus_name, "Variable importance; sum of AIC weights of", model.type),
        xlab = "",
        ylab = "Variable Importance"
)

dev.off()


dat3 <- dat2[dat2$sp.occ > range.size[2], ]
melt.dat2 <- melt(dat3[, c("AMT","MTC","AP","PS","EPcc", "EPcl")])

png(paste("Y://Large-ranged", genus_name, model.type,"_importance_sumWeightedAIC.png", sep=""))
par(cex.lab = 1.5, las = 3)
boxplot(melt.dat2$value ~ melt.dat2$variable,
        ylim = range(melt.dat2$value),
        main = paste("Large-ranged", genus_name, "Variable importance; sum of AIC weights of", model.type),
        xlab = "",
        ylab = "Variable Importance"
)

dev.off()



