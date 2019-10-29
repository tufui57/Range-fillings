#################################################################################################################
### Backward stepwise variable selection for GAM for target species
#################################################################################################################

library(dplyr)

genus_name <- "Acaena"

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
### Backward stepwise variable selection for GAM for target species
#################################################################################################################
library(mgcv)
library(gam)

predictor <- c(paste("bioclim", c(1,6,12,15), sep = ""), "EPcc","EPcl")

delta.AIC <- list()

for(i in 1:length(spname)){
  gam1 <- gam(as.formula(paste(spname[i] ,"~",
                               paste(predictor, collapse = "+", sep = "")
                               )
                         ),
              data = scores.ep, family = binomial)
  fullmodel.AIC<- AIC(gam1)
  
  delta.AIC.sp <- list()
  for(j in 1:length(predictor)){
    gam.i <- gam(as.formula(paste(spname[i] ,"~",
                                  paste(predictor[-j], collapse = "+", sep = "")
                                  )
                            ),
    data = scores.ep, family = binomial)
    
    delta.AIC.sp[j] <- (fullmodel.AIC - AIC(gam.i))
  }
delta.AIC[[i]] <- data.frame(predictor, unlist(delta.AIC.sp))
  
  
}

names(delta.AIC) <- spname

save(delta.AIC, file = paste("Y://", genus_name, "deltaAIC.data", sep=""))

#################################################################################################################
### Delta AIC of varibales as importance of variables
#################################################################################################################
load(paste("Y://", genus_name, "deltaAIC.data", sep=""))

# Calculate rank
imp.rank <- list()

for(i in 1:length(delta.AIC)){
  # Rank the importance
  imp.rank[[i]] <- rank(delta.AIC[[i]][,2]*(-1),  ties.method = "min")
}


dat <- data.frame(do.call(cbind, imp.rank)) %>% t %>% as.data.frame
dat$spname <- names(delta.AIC)
colnames(dat) <- c("AMT","MTC","AP","PS","EPcc", "EPcl", "spname")


library(reshape2)
melt.dat <- melt(dat[, c("AMT","MTC","AP","PS","EPcc", "EPcl")])

png(paste("Y://", genus_name, "_deltaAIC_rank.png", sep=""))
par(cex.lab = 1.5, las = 3)
boxplot(melt.dat$value ~ melt.dat$variable,
        ylim = rev(range(melt.dat$value)),
        main = "Variable importance rank by AIC",
        xlab = "",
        ylab = "Variable Importance rank"
)

dev.off()

