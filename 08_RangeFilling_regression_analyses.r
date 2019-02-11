
genus_name <-"Chionochloa"
genus_tag <- "chion"
proj.name = "SAI_cinl8Feb19_ensamble"

source(".//Chionochloa niche evolution//00_DataPreparation.R")

# Load range filling
rangefilling3 <- read.csv(paste("Y://rangefilling_", genus_name, proj.name,".csv", sep=""))
# Load niche volume
vols <- read.csv(paste("Y://NicheVolume_age_", genus_tag, ".csv", sep = ""))
vols$spname <- gsub("_", "\\.", vols$spname)
# Bind niche volume to range filling
dat <- merge(rangefilling3, vols, by = "spname")

##############################################################
### Range filling ~ species age
##############################################################
summary(lm(rangefilling ~ speciesAge, data = dat))

myplot <- plotAnalysis(data = dat,
                       genus_name = genus_name,
                       xv = "speciesAge", yv = "rangefilling", 
                       nodeNumbercol = "node1", showStats = T,
                       xlabname = "Species age", ylabname = "Range filling"
) +
  theme(text = element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save
ggsave(paste("Y:\\rangefilling_speciesAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = "mm")

rm(myplot)

############################################################################################################
##### Compare potential and actual range volume
############################################################################################################

# summary(glm(predictedOccurrence ~ occurrence, data = dat))
# 
# myplot <- plotAnalysis(data = dat,
#                        xv = "occurrence", yv = "predictedOccurrence", 
#                        nodeNumbercol = "node1", showStats = T,
#                        genus_name = genus_name,
#                        xlabname = "Actual range", ylabname = "Potential range"
# )+
#   theme(text = element_text(size=10),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))
# 
# # save
# ggsave(paste("Y:\\range_potential_actual_", genus_tag, ".png", sep = ""), plot = myplot,
#        width = 100, height = 80, units = 'mm')
# 
# rm(myplot)

############################################################################################################
##### Range filling and actual range size
############################################################################################################

summary(glm(rangefilling ~ occurrence, data = dat))

myplot <- plotAnalysis(data = dat,
                       xv = "occurrence", yv = "rangefilling", 
                       nodeNumbercol = "node1", showStats = T,
                       genus_name = genus_name,
                       xlabname = "Actual range", ylabname = "Range filling"
)+
  theme(text = element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save
ggsave(paste("Y:\\range_filling_actual_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)
