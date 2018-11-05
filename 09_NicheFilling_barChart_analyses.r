
genus_name <- "Chionochloa"
"Acaena"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")

vols <- read.csv(paste("Niche_Range_Volume_", genus_tag, ".csv", sep = ""))
#########################################################################
### Niche filling 
#########################################################################

### Niche filling 
vols$nichefilling <- (vols$nicheVolume / as.numeric(as.character(vols$potentialNicheVolume.D)))
mean(vols$nichefilling)

### Range filling 
vols$rangefilling <- (vols$rangeVolume / as.numeric(as.character(vols$potentialRangeVolume.D)))
mean(vols$rangefilling)

#########################################################################
### Bar plot of range filling 
#########################################################################
tag <- makeTag_separate(vols$spname, genus_name, separate = "_")[,2]
tag<-data.frame(tag)
vols <- cbind(tag,vols)

# Change order of data frame doesn't change order of bars
# Change order of factor (in this case, species name) levels
vols2 <- vols[order(vols$nichefilling),]
vols2$tag <- factor(vols2$tag, levels = vols2$tag[order(vols2$nichefilling)])

# Bar plot
myplot <- ggplot(vols2) +
  geom_bar(aes(x = tag, y = nichefilling), stat = "identity") +
  ylab("Proportion of climatic Niche filled") +
  xlab(paste(genus_name, "species")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# save
ggsave(paste("Y:\\nicheFilling_", genus_tag, ".png", sep = ""), plot = myplot)
  
#########################################################################
### Bar plot of niche filling 
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
