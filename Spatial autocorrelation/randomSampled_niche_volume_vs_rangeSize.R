library(dplyr)

########################################################################################################
# Random sampling for testing range size vs. niche volume (calcualted by ranges of annual temperature and precipitaion)
######################################################################################################

### Data preparation

genus_name <- "Acaena"
# Import species occurrence data
load(paste(".//Scores_", genus_name, "_landcover_worldclim1_5km.data", sep = ""))

# Load EPcc
load(".//EPcc_NZ_4var_test.data")
epcc <- load(".//EPcc_NZ_4var_test.data")
epcc <- get(epcc)
colnames(epcc)[ncol(epcc)] <- "EPcc"

# Load EPcl
load(".//EPcl_NZ_4var.data")
epcl <- load(".//EPcl_NZ_4var.data")
epcl <- get(epcl)
colnames(epcl)[ncol(epcl)] <- "EPcl"

scores.ep <- merge(scores, epcc[, c("x", "y", "EPcc")], by = c("x","y")) %>% 
  merge(., epcl[, c("x","y","EPcl")], by = c("x","y"))


### Random sampling
random.niche.volume10 <- list()

for(i in seq(10, 1500, 10)){
  
  test.row <- sample(1:nrow(scores.ep), i)
  test.s <- scores.ep[test.row, ]
  random.niche.volume10[[i]] <- nichePlot::SchoenerD_ecospat(scores.ep, "bioclim1", "bioclim12", scores.ep, test.s) 
  
}

### Plot
png("Y://Climate envelope volumes of Randomly sampled areas.png")
plot(seq(10, 1500, 10), 
     sapply(random.niche.volume10[seq(10, 1500, 10)], function(x){
       x[[1]][1]
     }
     ),
     main = "Climate envelope volumes of Randomly sampled areas",
     xlab = "number of samples",
     ylab = "Climate envelope volume"
)
dev.off()
