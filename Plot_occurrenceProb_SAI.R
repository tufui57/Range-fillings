#################################################################################################################
###   Plot BIOMOD occurrence probability against SAIcc
#################################################################################################################

library(raster)

### Load data
proj.name = "5kmLGM_15Jan19"
binary = "prob"
genus_name="Acaena"

# Occurrence probability
load(paste("Y://ensemblePredictionBinary_", genus_name, proj.name, binary,".data", sep = ""))

# Load SAIcc
load("C:\\Users\\nomur\\Documents\\diff_SAIcc_cl_5km_wholeNZ.data")

variable <- "diff"

### Plot
for(i in 1:length(pred)){
  
  try({
    png(paste(names(pred)[i], "prob_", variable, ".png", sep=""))
    dat <- cbind(coordinates(pred[[i]]), values(pred[[i]]))
    dat2 <- merge(dat, sai.diff, c("x","y"))
    colnames(dat2)[3] <- "prob"
    
    plot(log10(dat2$prob), dat2[,variable],
         xlab = "log10(Occurrence probability)", ylab = variable,
         main = names(pred)[i])
    
    dev.off()
    
    }
  )
  
}

