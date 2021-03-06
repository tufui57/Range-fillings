########################################################################################
### Map and climate space of occurrence probability
########################################################################################

library(dplyr)
library(raster)
library(ggplot2)
library(gridExtra)
source(".//Range fillings//F_get_probability.r")
source(".//functions//F_speciseNameCleaning_spnameFromPhylogenyTree.r")

###############################################################
## Data preparation 
###############################################################

genus_name = "Chionochloa"

# Load ensamble projection data
scores.prob <- get(load(paste("Y://ensemblePredictionBinary_", genus_name, ".data", sep = "")))

# Load PCA scores of occurrence data
if(file.exists(paste(".\\Scores_", genus_name, "_landcover.data", sep = "")) == FALSE){
  source(".//Range fillings//03_generate_PCAscores.r")
}else{
  load(paste(".\\Scores_", genus_name, "_landcover.data", sep = "")) 
}

colnames(scores)[grep(paste("^", genus_name, sep=""), colnames(scores))] <- gsub("_", "\\.", 
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
      rangefilling[[i]] <- (sum(scores[,i] == 1) / predictedP)
    },
    error = function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
  
}

rangefilling <- unlist(rangefilling)

#########################################################################
### max/min/median range filling
#########################################################################

maxsp <- spname[which(rangefilling == max(rangefilling))]
minsp <- spname[which(rangefilling == min(rangefilling))]

# Find the closest value to median
library(MALDIquant)
med <- sort(rangefilling)[match.closest(median(rangefilling), sort(rangefilling))]
medsp <- spname[which(rangefilling == med)]


####################################################################
### map and niche
####################################################################

extent_x = c(min(scores$PC1), max(scores$PC1))
extent_y = c(min(scores$PC2), max(scores$PC2))

####################################################################
### Plot predicted and actual niche
####################################################################

plot.niche.prob.and.occurrences <- function(species,# character string of species name
                                            scores, # PCA score
                                            scores.prob, # raster of probability
                                            tag # species name tag for legend title
){
  # get probability of occurreces
  prob1 <- get_occurrenceProbability_to_scores(species, scores.prob, scores)
  # Set colours
  cols <- c("1" = "black", "0" = "")
  
  # get occurrence data
  scores.sp <- scores[scores[, species] == 1, ]
  
  # Plot niche space
  pMain <- ggplot() +
    # point of probability
    geom_point(data = prob1, aes_string("PC1", "PC2",
                                        colour = paste("prob_", species, sep="")
    ), alpha = 0.1) +
    scale_colour_gradient(name = tag, low = "gray90", high = "black") +
    # point of each sp
    geom_point(data = scores.sp, aes(PC1, PC2), colour = "red", alpha = 0.1) +
    # extent
    xlim(extent_x) +
    ylim(extent_y) +
    ggtitle(species) +
    # legend position inside plot
    theme(axis.title = element_text(size = 15),
          # legend.position = "none",
          panel.background = element_blank(),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)
    )
  
  return(pMain)
  
}


####################################################################
### Plot map of all occurrence records regardless of habitat
####################################################################

map_plot_monoColour <- function(species, # character string of species name
                                scores, # PCA score
                                scores.prob, # raster of probability
                                tag
){
  
  # get probability of occurreces
  prob1 <- get_occurrenceProbability_to_scores(species, scores.prob, scores)
  
  # subset data for a species
  d.s <- scores[scores[, species] == 1, ]
  
  # Plot map
  pMap <- ggplot() +
    # Plot prediction
    geom_point(data = prob1, aes_string("x", "y", colour = paste("prob_", species, sep="")), alpha = 0.1) +
    scale_colour_gradient(name = tag, low = "gray90", high = "black") +
    # Plot occurrence points
    geom_point(data = d.s, aes(x = x, y = y), color = "red", alpha = 0.1) +
    
    # Show n and range filling
    ggtitle(paste("N =", nrow(d.s), 
                  "\n Range filling =", round(rangefilling[species], 3)
                  )
            ) +
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

####################################################################
### Combine map and Climate space plot
####################################################################

plot.niche.and.map.of.prob.and.occurrences <- function(species,# character string of species name
                                                       scores, # PCA score
                                                       scores.prob, # raster of probability
                                                       tag
){
  # get probability of occurreces
  prob1 <- get_occurrenceProbability_to_scores(species, scores.prob, scores)
  
  pMain <- plot.niche.prob.and.occurrences(species, scores, scores.prob, tag)
  pMap <- map_plot_monoColour(species, scores, scores.prob, tag)
  
  png(paste("prob_", species, ".png",  sep = ""), width = 1300, height = 600)
  grid.arrange(pMap, pMain, nrow = 1, widths = c(2,3))
  dev.off()
}


####################################################################
### Plot
####################################################################

for(x in spname){
  
  tryCatch(plot.niche.and.map.of.prob.and.occurrences(x, scores, scores.prob, 
                                             tag = makeTag_separate(x, genus_name, separate = "\\.")[,2]),
           error = function(e){cat("ERROR :",conditionMessage(e), "\n")}
           
  )
           }
