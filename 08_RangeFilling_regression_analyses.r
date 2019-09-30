########################################################################################
# Range filling analyses on 5km resolution
########################################################################################

genus_name <- "Acaena"
genus_tag <- "chion"
ensambleProj.name = "SAIdiff_4Mar19_ensamble"

source(".//Chionochloa niche evolution//scripts//00_DataPreparation.R")

# Load range filling
rangefilling <- read.csv(
  paste("Y://rangefilling_within_obs5km", genus_name, ensambleProj.name, ".csv", sep="")
  )

# Load niche volume
vols <- read.csv(paste("Y://NicheVolume_age_", genus_tag, ".csv", sep = ""))
vols$spname <- gsub("_", "\\.", vols$spname)

# Bind niche volume to range filling
dat <- merge(rangefilling, vols, by = "spname")

# Load SAIcc
saicc <- read.csv(paste(genus_name, "_averaged_SAI_over_occ.csv", sep=""))

dat2 <- merge(dat, saicc[, c("spname", "tag", "SAIcc")], by="spname")

########################################################################################
# Compare range filling between Acaena and Chionochloa on 5km resolution
########################################################################################

aca <- read.csv(paste("Y://rangefilling_within_obs5kmAcaena", ensambleProj.name, ".csv", sep=""))
chi <- read.csv(paste("Y://rangefilling_within_obs5kmChionochloa", ensambleProj.name, ".csv", sep=""))

### Compare range filling between genera
var.test(aca$rangefilling, chi$rangefilling)

t.test(aca$rangefilling, chi$rangefilling, var.equal = FALSE)

boxplot(aca$rangefilling, chi$rangefilling)

##############################################################
### Range filling ~ species age
##############################################################
summary(lm(rangefilling ~ speciesAge, data = dat2))

myplot <- plotAnalysis(data = dat2,
                       genus_name = genus_name,
                       xv = "speciesAge", yv = "rangefilling", 
                       nodeNumbercol = "tag", showStats = F,
                       label.point = T,
                       xlabname = "Species age", ylabname = "Range filling"
) +
  theme(text = element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save
ggsave(paste("Y:\\rangefilling_speciesAge_", genus_name, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = "mm")

rm(myplot)



############################################################################################################
##### Range filling ~ actual range size
############################################################################################################

summary(glm(rangefilling ~ occurrence, data = dat2))

myplot <- plotAnalysis(data = dat2,
                       xv = "occurrence", yv = "rangefilling", 
                       nodeNumbercol = "tag", showStats = F,
                       genus_name = genus_name, label.point = T,
                       xlabname = "Actual range", ylabname = "Range filling"
)+
  theme(text = element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save
ggsave(paste("Y:\\range_filling_actual_", genus_name, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)

############################################################################################################
##### Range filling ~ Averaged SAI over species habitat
############################################################################################################

# "ShowStats" has bug!!!

myplot <- plotAnalysis(data = dat2,
                       xv = "SAIcc", yv = "rangefilling", 
                       nodeNumbercol = "tag", showStats = F,
                       genus_name = genus_name, label.point = F,
                       xlabname = "Availability of climate\noccupied by species", ylabname = "Range filling"
)+
  xlim(0.55, 0.75) +
  theme(text = element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save
ggsave(paste("Y:\\range_filling_averageSAIcc_", genus_name, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)
