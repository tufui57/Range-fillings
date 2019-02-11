########################################################################################
### Map and climate space of occurrence probability
########################################################################################

library(dplyr)
library(ggplot2)
library(gridExtra)
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")

###############################################################
## Data preparation 
###############################################################

genus_name = "Chionochloa"

proj.name = "SAI_cinl8Feb19_ensamble"
# Load range filling
rangefilling <- read.csv(paste("Y://rangefilling_", genus_name, proj.name, ".csv", sep=""))
tag <- makeTag_separate(rangefilling$spname, genus_name = genus_name, separate = "\\.") %>% 
  .[,"tag"]
rangefilling2 <- cbind(rangefilling,tag)

#########################################################################
### Bar plot of range filling 
#########################################################################

myplot <- ggplot(rangefilling2) +
  geom_bar(aes(x = reorder(tag, rangefilling), y = rangefilling),  stat = "identity") +
  ylab("Range filling") +
  xlab("") +
  ylim(0,1) +
  theme_bw()+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1))

# save
ggsave(paste("Y:\\rangeFilling_bars_", genus_name, ".png", sep = ""), plot = myplot)
