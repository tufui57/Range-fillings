
genus_name <- "Acaena"

library(dismo)

source(".//Chionochloa niche evolution//scripts//03_DataPreparation.R")

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

# Extract probability by node number
get_BIOMOD_probability_by_nodeID <- function(i # node ID number
){
  # Species name codes
  nodeName <- pull(codes[codes$X %in% rownames(nodes)[i], ], X)
  
  prob <- (spname == nodeName) %>% pred[.]
  
  return(prob)
}

####################################################################
### Get Schoenner's D between two sipecies occurrence probability
####################################################################

# Extract probability by node number
probD <- list()

for(i in sispairs[,1]){
  
  prob1 <- get_BIOMOD_probability_by_nodeID(i)
  prob2 <- get_BIOMOD_probability_by_nodeID(allnodesister[[i]])
  
  ### Use dismo::nicheOverlap
  probD[[i]] <- nicheOverlap(prob1[[1]], prob2[[1]], stat = 'D', mask = TRUE, checkNegatives = TRUE)
}

# Import data
overlapPdData <- read.csv(paste("Nicheovrlap_PD_", genus_tag, ".csv", sep = ""))

### Node numbers of sister species pairs
sisOverlapPd <- (overlapPdData$node1 %in% sispairs[,1]) %>% overlapPdData[., ]
# Acaena
if(genus_name == "Acaena"){
  pro <- unlist(probD)
}
# Chionochloa 
if(genus_name == "Chionochloa"){
  pro <- unlist(probD)[-length(unlist(probD))]
}
overlaps <- cbind(sisOverlapPd, pro)
colnames(overlaps)[colnames(overlaps) == "pro"] <- "potentialNicheOverlap"

############################################################################################################
##### Compare Schoenner's D from probability and the one from occurrence records
############################################################################################################

m <- lm(probD ~ nicheOverlap, overlaps)
myplot <- plotAnalysis(data = overlaps,
                       genus_name = genus_name,
                       xv = "nicheOverlap", yv = "probD", 
                       nodeNumbercol = "node1", showStats = T,
                       xlabname = "Niche overlap of occurrence records", ylabname = "Niche overlap of model prediction"
)

# save
ggsave(paste("Y:\\Comparison_sister_nicheoverlap_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)


#########################################################################
### Sister species pairs' niche overlap of predictions ~ divergence time
#########################################################################

myplot <- plotAnalysis(data = overlaps,
                       yv = "divergenceTime", xv = "probD", 
                       nodeNumbercol = "node1", showStats = T,
                       genus_name = genus_name,
                       ylabname = "Divergence Time", xlabname = "Niche overlap of model prediction"
)

# save
ggsave(paste("Y:\\sister_predNicheoverlap_phyloDistance_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot)


