########################################################################################
### Map and climate space of occurrence probability
########################################################################################

###############################################################
## Data preparation 
###############################################################
genus_name = "Chionochloa"

source(".\\Chionochloa niche evolution\\Chionochloa2ndary open habitat analysis\\11_Data_preparation_for_drawing_EnvSpace_and_Maps.r")
source(".//Chionochloa niche evolution//00_DataPreparation.R")
source(".//Chionochloa niche evolution//Niche filling//F_get_probability.r")

# Load ensamble projection data
scores.prob <- get(load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = "")))

vols <- read.csv(paste("Niche_Range_Volume_", genus_tag, ".csv", sep = ""))
splist <- read.csv(paste(".\\NicheVolume_age_", genus_tag, ".csv", sep = ""))

spname <- (!is.na(splist$spname)) %>% splist$spname[.] %>% as.character

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
### map and nihce of species with max/min/median niche filling
#########################################################################

maxsp <- vols[which(vols$nichefilling == max(vols$nichefilling)), "spname"] %>% as.character
minsp <- vols[which(vols$nichefilling == min(vols$nichefilling)), "spname"] %>% as.character
medsp <- vols[which(vols$nichefilling == median(vols$nichefilling)), "spname"] %>% as.character


####################################################################
### Potential niche volume
####################################################################
# Plot niche of occurrence 
species=medsp
extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

plot.niche.prob.and.occurrences <- function(species,# character string of species name
                                            scores, # PCA score
                                            scores.prob # raster of probability
                                            ){
  # get probability of occurreces
  prob1 <- get_occurrenceProbability_to_scores(species, scores.prob)
  # get occurrence data
  scores.sp <- scores[scores[, species] == 1, ]
  
  # Plot niche space
  pMain <- ggplot() +
    # plot all NZ data points
    geom_point(data = scores, aes(PC1, PC2), color = 'gray90', alpha = 0.25) +
    # point of probability
    geom_point(data = prob1, aes_string("PC1", "PC2",
                                        colour = paste("prob_", species, sep = "")
    ), alpha = 0.1) +
    scale_colour_gradient(low = "gray90", high = "black") +
    # point of each sp
    geom_point(data = scores.sp, aes(PC1, PC2), colour = "red", alpha = 0.1) +
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    ggtitle(species) +
    # legend position inside plot
    theme(axis.title = element_text(size = 15),
          # legend.position = "none",
          panel.background = element_rect(fill = 'gray96')
    )
  
  return(pMain)
  
}


  
# Plot all occurrence records regardless of habitat
map_plot_monoColour <- function(species, # character string of species name
                                scores, # PCA score
                                scores.prob # raster of probability
){
  
  # get probability of occurreces
  prob1 <- get_occurrenceProbability_to_scores(species, scores.prob)
  
  # subset data for a species
  d.s <- scores[scores[, species] == 1, ]
  
  # Plot map
  pMap <- ggplot() +
    # Plot NZ outline
    #geom_polygon(data = nzland, aes(x = long, y = lat, group = group), colour = "gray50", fill = 'gray90') +
    # Plot probability
    geom_point(data = prob1, aes_string("x", "y", colour = paste("prob_", species, sep = "")), alpha = 0.1) +
    scale_colour_gradient(low = "gray90", high = "black") +
    # Plot occurrence points
    geom_point(data = d.s, aes(x = x, y = y), color = "red", alpha = 0.1) +
    ggtitle(paste("N =", nrow(d.s))) +
    guides(colour = guide_legend(override.aes = list(size = 5, shape = 16, alpha = 0.7))) +
    labs(x = "", y = "") +
    # legend position inside plot at topleft
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          plot.title = element_text(family = "Times New Roman", size = 20),
          legend.justification = c(1, 1), legend.position = c(0.3, 1),
          panel.background =  element_blank(), #element_rect(fill = 'gray96'),
          #axis.text.y = element_text(angle = 90, hjust = 0.5)
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
  
  return(pMap)
  
}


plot.niche.and.map.of.prob.and.occurrences <- function(species,# character string of species name
                                                       scores, # PCA score
                                                       scores.prob # raster of probability
){
  # get probability of occurreces
  prob1 <- get_occurrenceProbability_to_scores(species, scores.prob)
  
  pMain <- plot.niche.prob.and.occurrences(species, scores, scores.prob)
  pMap <- map_plot_monoColour(species,scores, scores.prob)
  
  png(paste("prob_", species, ".png",  sep = ""), width = 1300, height = 600)
  grid.arrange(pMap, pMain, nrow = 1, widths = c(2,3))
  dev.off()
}

lapply(c(minsp, medsp, maxsp), plot.niche.and.map.of.prob.and.occurrences,
       scores, scores.prob)
