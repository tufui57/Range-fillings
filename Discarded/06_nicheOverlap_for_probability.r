
genus_name <- "Acaena"

library(dismo)

source(".//Chionochloa niche evolution//00_DataPreparation.R")
source(".//Chionochloa niche evolution//Analyze Prediction by BIOMOD//F_get_probability.r")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

####################################################################
### Potential range overlap between sister species
####################################################################

# Extract probability by node number
probD <- list()

for(i in spname[spname %in% sapply(tree$tip.label, clean_speciesname)]){
  
  if(get_nodeID_from_spname(i,tree) %in% sispairs[,1]){
    
    # Get sister species name
    spnode <- which(sispairs[,1] == get_nodeID_from_spname(i,tree))
    sisname <- get_sisterSpNames(sispairs[spnode,1], tree)[[2]]
    
    prob1 <- get_occurrenceProbability_to_scores(i)
    prob2 <- get_occurrenceProbability_to_scores(sisname)
    
    # dismo::nicheOverlap() requires raster of probability
    
    ras1 <- raster_from_dataframe(prob1)
    ras2 <- raster_from_dataframe(prob2)
    
    ### Use dismo::nicheOverlap
    probD[[i]] <- nicheOverlap(ras1,ras2, stat = 'D', mask = TRUE, checkNegatives = TRUE)
  
    }else{
      print("No sister pair")
    }
  }


# Import data
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))

### Node numbers of sister species pairs
sisOverlapPd <- (overlapPdData$node1 %in% sispairs[,1]) %>% overlapPdData[., ]
if(genus_tag=="chion"){
  pro <- unlist(probD)[-length(unlist(probD))]
}else{
  pro <- unlist(probD)
}


overlaps <- cbind(sisOverlapPd, pro)
colnames(overlaps)[colnames(overlaps) == "pro"] <- "potentialNicheOverlap"

write.csv(overlaps, paste("Nicheovrlap_potential_actual_", genus_tag, "2.csv", sep = ""))

############################################################################################################
##### Potential range overlap ~ actual niche overlap between sister species
############################################################################################################

overlaps <- read.csv(paste("Nicheovrlap_potential_actual_", genus_tag, "2.csv", sep = ""))

m <- lm(potentialNicheOverlap ~ nicheOverlap, overlaps)
myplot <- plotAnalysis(data = overlaps,
                       genus_name = genus_name,
                       xv = "nicheOverlap", yv = "potentialNicheOverlap", 
                       nodeNumbercol = "node1", showStats = T,
                       xlabname = "Actual niche overlap", ylabname = "Potential niche overlap"
) +
  theme(text = element_text(size=10))

# save
ggsave(paste("Y:\\Comparison_sister_nicheoverlap_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = "mm")

rm(myplot)


#########################################################################
### potetntial range overlap between Sister species ~ divergence time
#########################################################################

myplot <- plotAnalysis(data = overlaps,
                       xv = "divergenceTime", yv = "potentialNicheOverlap", 
                       nodeNumbercol = "node1", showStats = T,
                       genus_name = genus_name,
                       xlabname = "Divergence Time", ylabname = "Potential niche overlap"
)+
  theme(text = element_text(size=10))

# save
ggsave(paste("Y:\\sister_predNicheoverlap_divergenceTime_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)


