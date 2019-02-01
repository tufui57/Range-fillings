
############################################################################################################
## Visualize varibale imporatance of SAI
############################################################################################################

### Load variable importance

chi <- read.csv("Y://averaged_rank_importance_Chionochloa.csv")
aca <- read.csv("Y://averaged_rank_importance_Acaena.csv")

### Boxplot 

png("Acaena_varibale_importance.png")
boxplot(aca[,-1],
        main = "Acaena",
        ylab = "Variable Importance")

dev.off()

png("Chionochloa_varibale_importance.png")
boxplot(chi[,-1], 
        main = "Chionochloa",
        ylab = "Variable Importance")

dev.off()

############################################################################################################
## Visualize SAI diferences due to neighbourhood size
############################################################################################################

### Load SAI with different neighbourhood sizes
sai <- read.csv("SAI_current_LGM_dif.csv")

png("SAI_boxplot.png")
par(las = 2,
    mar = c(5.1 +2.2, 4.1, 4.1-2.2, 2.1)
    )
boxplot(sai[, c(-1,-2,-3, -ncol(sai))],
        main = "SAI with 20, 50 and 100 km neighbourhood",
        ylab = "SAI")
dev.off()

############################################################################################################
## Linear models for range filling and variable importances
############################################################################################################

### Load range filling data

aca.range <- read.csv("Y://Analysis_Acaena_SAI.csv")
chi.range <- read.csv("Y://Analysis_Chionochloa_SAI.csv")


# Merge the data
aca.d <- merge(aca.range, aca)
colnames(aca.d)[colnames(aca.d)=="X"] <- "spname"

chi.d <- merge(chi.range, chi, by.x = "spname", by.y = "X")

# data for all species of two genera
cols <- c("spname", "rangefilling", "bioclim1", "bioclim6", "bioclim12", "bioclim15", "sai")

dat <- rbind(aca.d[,cols], chi.d[,cols])

### Avarage rank of SAI
sapply(aca.d[,cols], mean)
sapply(chi.d[,cols], mean)
sapply(dat, mean)
