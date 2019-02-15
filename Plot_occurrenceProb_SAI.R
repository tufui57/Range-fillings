#################################################################################################################
###   Plot BIOMOD occurrence probability against SAIcc
#################################################################################################################

library(raster)

### Load data
proj.name = "5km_15Jan19_ensamble"
binary = "prob"
genus_name="Acaena"

# Occurrence probability
load(paste("Y://ensemblePredictionBinary_", genus_name, proj.name, binary,".data", sep = ""))

# Load SAIcc
load("C:\\Users\\nomur\\Documents\\diff_SAIcc_cl_5km_wholeNZ.data")

### Plot
for(i in 1:length(pred)){
  
  try({
    png(paste(names(pred)[i], "prob_SAI.png", sep=""))
    dat <- cbind(coordinates(pred[[i]]), values(pred[[i]]))
    dat2 <- merge(dat, sai.diff, c("x","y"))
    
    plot(log10(dat2$V3), dat2$SAIcc,
         xlab = "log10(Occurrence probability)", ylab = "SAIcc",
         main = names(pred)[i])
    
    dev.off()
    
    }
  )
  
}

