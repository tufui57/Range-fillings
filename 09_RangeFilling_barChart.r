########################################################################################
### Map and climate space of occurrence probability
########################################################################################

library(dplyr)
library(ggplot2)
library(gridExtra)
source(".//Range fillings//F_get_probability.r")

###############################################################
## Data preparation 
###############################################################

genus_name = "Acaena"

# Load ensamble projection data
scores.prob <- get(load(paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = "")))

# Load PCA scores of occurrence data
if(file.exists(paste(".\\Scores_", genus_name, "_landcover.data", sep = "")) == FALSE){
  source(".//Range fillings//03_generate_PCAscores.r")
}else{
  load(paste(".\\Scores_", genus_name, "_landcover.data", sep = "")) 
}

colnames(scores)[grep(paste("^", genus_name, sep = ""), colnames(scores))] <- gsub("_", "\\.", 
                                                                                 colnames(scores)[grep(paste("^", genus_name, sep=""), colnames(scores))])

spname <- names(scores.prob)

#########################################################################
### Niche filling 
#########################################################################

rangefilling <- list()
for(i in spname){
  tryCatch(
    {
      predictedP <- (values(scores.prob[[i]]) == 1) %>% sum(., na.rm = T)
      # Range filling 
      rangefilling[[i]] <- (sum(scores[,i] == 1)/ predictedP)
    },
    error = function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  
}

rangefilling <- unlist(rangefilling)


#########################################################################
### Bar plot of range filling 
#########################################################################
tag <- makeTag_separate(spname, genus_name, separate = "_")[,2]
tag <- data.frame(tag)
rangefilling <- cbind(tag, rangefilling)

# Change order of data frame doesn't change order of bars
# Change order of factor (in this case, species name) levels
rangefilling2 <- rangefilling[order(rangefilling),]
rangefilling2$tag <- factor(rangefilling2$tag, levels = rangefilling2$tag[order(rangefilling2$nichefilling)])


#########################################################################
### Bar plot of range filling 
#########################################################################

# Change order of data frame doesn't change order of bars
# Change order of factor (in this case, species name) levels
# vols2 <- vols[order(vols$rangefilling),]
# vols2$tag <- factor(vols2$tag, levels = vols2$tag[order(vols2$rangefilling)])

# Bar plot
myplot <- ggplot(vols2) +
  geom_bar(aes(x = tag, y = rangefilling), stat = "identity") +
  ylab("Proportion of Range filled") +
  xlab(paste(genus_name, "species")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# save
ggsave(paste("Y:\\rangeFilling_", genus_tag, ".png", sep = ""), plot = myplot)
