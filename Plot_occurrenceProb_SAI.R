#################################################################################################################
###   Plot BIOMOD occurrence probability against SAIcc
#################################################################################################################

library(raster)
library(ggplot2)
library(gridExtra)
library(grid)

# Load SAIcc
load("C:\\Users\\nomur\\Documents\\diff_SAIcc_cl_5km_wholeNZ.data")

genus_name="Chionochloa"

### Load 5km presence/absence data
load(paste(".\\Scores_", genus_name,"_landcover5km.data", sep=""))

### Load data
binary = "prob"

# Occurrence probability of LGM
nam <- load(paste("Y://ensemblePredictionBinary_", genus_name, "5km_15Jan19_ensamble", binary,".data", sep = ""))
cur <- get(nam)
# Occurrence probability of the current
nam.lgm <- load(paste("Y://ensemblePredictionBinary_", genus_name, "5kmLGM_15Jan19", binary,".data", sep = ""))
lgm <- get(nam.lgm)

spnames <- colnames(scores)[grepl(paste("^", genus_name, sep=""), colnames(scores))]


plot_SAI <- function(i, # species number
                     variable # which SAI is plotted against current occurrence probability
         ){
  # Current occurrence probability
  dat <- as.data.frame(cbind(coordinates(cur[[i]]), values(cur[[i]])))
  colnames(dat)[3] <- "cur"
  
  # LGM occurrence probability
  dat.lgm <- as.data.frame(cbind(coordinates(lgm[[i]]), values(lgm[[i]])))
  colnames(dat.lgm)[3] <- "lgm"
  
  # Merge current and LGm data
  dat2 <- merge(dat, dat.lgm, c("x","y"))
  
  # Merge occurrence probability data and SAI data
  dat3 <- merge(dat2, sai.diff, c("x","y"))
  dat3$prob.diff <- dat3$cur - dat3$lgm
  
  # log10 of current occurrence probability
  dat3$log.cur <- log10(dat3$cur)
  
  ### Points of occurrence records
  spname <- spnames[i]
  dat.sp <- merge(dat3, scores[scores[, spname] == 1, ], c("x","y"))
  
  # Plot current occurrence probability
  cur.plot <- ggplot(data = dat3, aes_string(x = "log.cur", y = variable)) +
    geom_point() +
    geom_point(data = dat.sp, aes_string(x = "log.cur", y = variable), col="red", pch=16) +
    xlab("log10(current occurrence probability)") +
    ylab(variable)
  
  # Plot the difference of occurrence probabilities
  dif.plot <- ggplot(data = dat3, aes_string(x = "prob.diff", y = variable)) +
    geom_point() + 
    geom_point(data = dat.sp, aes_string(x = "prob.diff", y = variable), col="red", pch=16) +
    xlab("difference of probability") +
    ylab(variable)
  
  res <- grid.arrange(cur.plot, dif.plot, ncol = 2,
                      top = textGrob(names(cur)[i])
  )
  ggsave(paste("Y://", names(cur)[i], "prob_", variable,".png", sep=""), plot = res)
  
}

#################################################################################################################
###   Plot difference of BIOMOD occurrence probability between LGM and current against SAIcc
#################################################################################################################

for(i in 1:length(cur)){
  
  try(plot_SAI(i, "SAIcc")
      )

}

#################################################################################################################
###   Plot difference of BIOMOD occurrence probability between LGM and current against difference of SAIcc-cl
#################################################################################################################

for(i in 1:length(cur)){
  
  try(plot_SAI(i, "diff")
  )
  
}

