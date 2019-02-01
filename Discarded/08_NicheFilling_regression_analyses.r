
genus_name <- "Acaena"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")

vols <- read.csv(paste("Niche_Range_Volume_", genus_tag, ".csv", sep = ""))

#########################################################################
### Potential niche ~ species age
#########################################################################

myplot <- plotAnalysis(data = vols,
                       genus_name = genus_name,
                       xv = "speciesAge", yv = "potentialNicheVolume.D", 
                       nodeNumbercol = "node1", showStats = T,
                       xlabname = "Species age", ylabname = "Potential niche volume"
) +
  theme(text = element_text(size=10),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save
ggsave(paste("Y:\\potentialNicheVolume_speciesAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = "mm")

rm(myplot)

############################################################################################################
##### Compare potential and actual niche volume
############################################################################################################

myplot <- plotAnalysis(data = vols,
                       xv = "nicheVolume", yv = "potentialNicheVolume.D", 
                       nodeNumbercol = "node1", showStats = T,
                       genus_name = genus_name,
                       xlabname = "Actual niche volume", ylabname = "Potential niche volume"
)+
  theme(text = element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# save
ggsave(paste("Y:\\nicheVolume_potential_actual_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)



#########################################################################
### Potential range ~ species age
#########################################################################

myplot <- plotAnalysis(data = vols,
                       genus_name = genus_name,
                       xv = "speciesAge", yv = "potentialRangeVolume.D", 
                       nodeNumbercol = "node1", showStats = T,
                       xlabname = "Species age", ylabname = "Potential range volume"
) +
  theme(text = element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save
ggsave(paste("Y:\\potentialRangeVolume_speciesAge_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = "mm")

rm(myplot)

############################################################################################################
##### Compare potential and actual range volume
############################################################################################################

myplot <- plotAnalysis(data = vols,
                       xv = "rangeVolume", yv = "potentialRangeVolume.D", 
                       nodeNumbercol = "node1", showStats = T,
                       genus_name = genus_name,
                       xlabname = "Actual range volume", ylabname = "Potential range volume"
)+
  theme(text = element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save
ggsave(paste("Y:\\rangeVolume_potential_actual_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)

#########################################################################
### Niche filling
#########################################################################

### Niche filling 
vols$nichefilling <- (vols$nicheVolume / as.numeric(as.character(vols$potentialNicheVolume.D)))
mean(vols$nichefilling)

### Range filling
vols$rangefilling <- (vols$rangeVolume / as.numeric(as.character(vols$potentialRangeVolume.D)))
mean(vols$rangefilling)

############################################################################################################
##### Compare range and niche filling
############################################################################################################

myplot <- plotAnalysis(data = vols,
                       xv = "nichefilling", yv = "rangefilling", 
                       nodeNumbercol = "node1", showStats = T,
                       genus_name = genus_name,
                       xlabname = "Niche filling", ylabname = "Range filling"
)+
  theme(text = element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save
ggsave(paste("Y:\\RangeVsNiche_filling_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)
