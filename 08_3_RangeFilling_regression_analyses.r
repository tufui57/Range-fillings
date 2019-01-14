###################################################################################
### Medians of past climate
###################################################################################
# 
# genus_name <- "Chionochloa"

lgm.median.of.temp <- list()
for(i in c("Acaena", "Chionochloa")){
  
load(paste(".//Scores_", i, "_landcover5km.data", sep=""))

# species names
sname <- colnames(scores)[grepl(paste("^", i, sep = ""), colnames(scores))]

spdat <- list()
for(i in sname){
  spdat[[i]] <- scores[scores[,i] == 1, ]
  }

lgm.median.of.temp[[i]] <- sapply(sname, function(i){
  median.default(spdat[[i]]$PC1)
  })
}

###################################################################################
### Medians of current climate
###################################################################################
median.of.temp <- list()
for(i in c("Acaena", "Chionochloa")){
  
  load(paste(".//Scores_", i, "_landcover.data", sep=""))
  
  # species names
  sname <- colnames(scores)[grepl(paste("^", i, sep = ""), colnames(scores))]
  
  spdat <- list()
  for(i in sname){
    spdat[[i]] <- scores[scores[,i] == 1, ]
  }
  
  median.of.temp[[i]] <- sapply(sname, function(i){
    median.default(spdat[[i]]$PC1)
  })
}

###################################################################################
### Collate data
###################################################################################

dat.aca <- cbind(dat.genus[[1]], median.of.temp[[1]][names(lgm.median.of.temp[[1]])],
      lgm.median.of.temp[[1]])
colnames(dat.aca)[c(length(colnames(dat.aca)) - 1, length(colnames(dat.aca)))] <- c("median.of.temp", "lgm.median.of.temp")

dat.chi <- cbind(dat.genus[[2]], median.of.temp[[2]][names(lgm.median.of.temp[[2]])],
                 lgm.median.of.temp[[2]])
colnames(dat.chi)[c(length(colnames(dat.chi)) - 1, length(colnames(dat.chi)))] <- c("median.of.temp", "lgm.median.of.temp")


###################################################################################
### Analyses for each genus separately
###################################################################################

### Acaena
summary(lm(dat.aca$rangefilling ~ dat.aca$lgm.median.of.temp + dat.aca$mean.sai))
summary(lm(dat.aca$rangefilling ~ dat.aca$lgm.median.of.temp))
summary(lm(dat.aca$rangefilling ~ dat.aca$median.of.temp + dat.aca$mean.sai))
summary(lm(dat.aca$rangefilling ~ dat.aca$median.of.temp ))

summary(lm(dat.aca$rangefilling ~ dat.aca$mean.sai))

### Chionochloa
summary(lm(dat.chi$rangefilling ~ dat.chi$lgm.median.of.temp + dat.chi$mean.sai))
summary(lm(dat.chi$rangefilling ~ dat.chi$lgm.median.of.temp))
summary(lm(dat.chi$rangefilling ~ dat.chi$median.of.temp + dat.chi$mean.sai))
summary(lm(dat.chi$rangefilling ~ dat.chi$median.of.temp ))

summary(lm(dat.chi$rangefilling ~ dat.chi$mean.sai))


###################################################################################
### Analyses for both genera
###################################################################################

test <- rbind(dat.aca[, c("rangefilling", "occurrence", "nichefilling", "mean.sai", "dispersal", "median.of.temp", "lgm.median.of.temp")],
               dat.chi[, c("rangefilling", "occurrence", "nichefilling", "mean.sai", "dispersal", "median.of.temp", "lgm.median.of.temp")]
)

### Regression
summary(lm(test$rangefilling ~ test$lgm.median.of.temp + test$dispersal + test$mean.sai))
summary(lm(test$rangefilling ~ test$lgm.median.of.temp + test$dispersal))
summary(lm(test$rangefilling ~ test$median.of.temp+ test$dispersal + test$mean.sai))
summary(lm(test$rangefilling ~ test$median.of.temp+ test$dispersal))

summary(lm(test$rangefilling ~ test$mean.sai+ test$dispersal))


library(car)
vif(lm(test$rangefilling ~ test$median.of.temp + test$mean.sai + test$lgm.median.of.temp + test$dispersal))
