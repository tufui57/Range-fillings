########################################################################################
### Map and climate space of occurrence probability
########################################################################################

library(dplyr)
library(ggplot2)
library(gridExtra)
source(".//Range fillings//F_get_probability.r")
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")

###############################################################
## Data preparation 
###############################################################

genus_name = "Acaena" # "Chionochloa"

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
### Range filling 
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


### make species name tags
tag <- makeTag_separate(spname, genus_name, separate = ".")[,2]
tag <- data.frame(cbind(spname, tag))
rangefilling <- data.frame(names(rangefilling), rangefilling)
rangefilling <- merge(tag, rangefilling, by.x = "spname", by.y = "names.rangefilling.")

### Change order of data frame doesn't change order of bars
# Change order of factor (in this case, species name) levels
rangefilling2 <- rangefilling[order(rangefilling$rangefilling),]
rangefilling2$tag <- factor(rangefilling2$tag, levels = rangefilling2$tag[order(rangefilling2$rangefilling)])

#########################################################################
### Bar plot of range filling 
#########################################################################

myplot <- ggplot(rangefilling2) +
  geom_bar(aes(x = tag, y = rangefilling), stat = "identity") +
  ylab("Proportion of Range filled") +
  xlab(paste(genus_name, "species")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# save
ggsave(paste("Y:\\rangeFilling_", genus_name, ".png", sep = ""), plot = myplot)
