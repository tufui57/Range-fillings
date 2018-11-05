
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
### Actual range overlap between sister species
####################################################################
# Import species occurrence data
source(".\\Chionochloa niche evolution\\scripts\\F02_clean_up_species_records18Sep.R")

### Get rid of species whose occurren resords < 5
dat2 <- dat[sapply(dat, nrow) >= 5]

# Load ensamble projection data
load(paste("Y://ensemblePrediction_", genus_tag, ".data", sep = ""))

# Create imaginary speices occurring at all cells in NZ
probAll <- pred[1]
values(probAll[[1]]) <- ifelse(is.na(values(probAll[[1]])), NA, 1)

sp.all <- data.frame(
  cbind(coordinates(probAll[[1]]), values(probAll[[1]]))
)
colnames(sp.all)[1:2] <- c("lon", "lat")
sp.all2 <- sp.all[!is.na(sp.all[,3]),]

# Calculate Shoener's D
D <- list()

for(i in spname[spname %in% sapply(tree$tip.label, clean_speciesname)]){
  
  if(get_nodeID_from_spname(i,tree) %in% sispairs[,1]){
    
    # Get sister species name
    spnode <- which(sispairs[,1] == get_nodeID_from_spname(i,tree))
    sisname <- get_sisterSpNames(sispairs[spnode,1], tree)[[2]]
    
    D[[i]] <- SchoenerD_ecospat(sp.all2, "lon", "lat",
                                dat2[[i]], dat2[[sisname]]
                                )
    
  }else{
    print(paste(i, "has no sister pair"))
  }
}
    
    

####################################################################
### Potential range overlap between sister species
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

if(genus_name=="Chionochloa"){
  pro <- unlist(probD)[-length(unlist(probD))]
}else{
  pro <- unlist(probD)
}


overlaps <- cbind(sisOverlapPd, pro)
colnames(overlaps)[colnames(overlaps) == "pro"] <- "potentialRangeOverlap"

write.csv(overlaps, paste("Rangeovrlap_potential_actual_", genus_tag, ".csv", sep = ""))

############################################################################################################
##### Potential range overlap ~ actual niche overlap between sister species
############################################################################################################

overlaps <- read.csv(paste("Rangeovrlap_potential_actual_", genus_tag, ".csv", sep = ""))

m <- lm(potentialNicheOverlap ~ nicheOverlap, overlaps)
myplot <- plotAnalysis(data = overlaps,
                       genus_name = genus_name,
                       xv = "Overlap", yv = "potentialRangeOverlap", 
                       nodeNumbercol = "node1", showStats = T,
                       xlabname = "Actual range overlap", ylabname = "Potential range overlap"
) +
  theme(text = element_text(size=10))

# save
ggsave(paste("Y:\\sister_RangeOverlap_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = "mm")

rm(myplot)


#########################################################################
### potetntial range overlap between Sister species ~ divergence time
#########################################################################

myplot <- plotAnalysis(data = overlaps,
                       xv = "divergenceTime", yv = "potentialRangeOverlap", 
                       nodeNumbercol = "node1", showStats = T,
                       genus_name = genus_name,
                       xlabname = "Divergence Time", ylabname = "Potential range overlap"
                       ) +
  theme(text = element_text(size=10))

# save
ggsave(paste("Y:\\sister_potentialRangeoverlap_divergenceTime_", genus_tag, ".png", sep = ""), plot = myplot,
       width = 100, height = 80, units = 'mm')

rm(myplot)


