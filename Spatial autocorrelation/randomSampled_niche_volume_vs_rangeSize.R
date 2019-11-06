########################################################################################################
# Random sampling for testing range size vs. niche volume (calcualted by ranges of annual temperature and precipitaion)
######################################################################################################

library(dplyr)

### Data preparation

# Import climate and PCA data
load(".//Scores_Acaena_landcover_worldclim1_5km.data")

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
  
  ran.row <- sample(1:nrow(scores.ep), i)
  ran.sam <- scores.ep[ran.row, ]
  random.niche.volume10[[i]] <- nichePlot::SchoenerD_ecospat(scores.ep, "PC1", "PC2", scores.ep, ran.sam) 
  
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


