
############################################################################################################
## Visualize varibale imporatance of SAI
############################################################################################################
# 1km resolution, four bioclim
folder.name = ""
# 5km resolution, four bioclim + SAIcc + SAIcl
folder.name = "SAI_cinl8Feb19"

### Load variable importance

aca <- read.csv(paste("Y://averaged_rank_importance_Acaena", folder.name, ".csv", sep = ""))
chi <- read.csv(paste("Y://averaged_rank_importance_Chionochloa", folder.name, ".csv", sep = ""))

### Boxplot 

par(cex.lab=1.5, las=3)
colnames(aca)[-1] <- c("AMT","MTC","AP","PS","SAIcc","SAIcl")
colnames(chi)[-1] <- c("AMT","MTC","AP","PS","SAIcc","SAIcl")


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
## Linear models for range filling and variable importances
############################################################################################################

### Load range filling data
aca.range <- read.csv("Y://rangefilling_AcaenaSAI_cinl8Feb19_ensamble.csv")
chi.range <- read.csv("Y://rangefilling_ChionochloaSAI_cinl8Feb19_ensamble.csv")

# Merge the data
aca.d <- merge(aca.range, aca, by.x = "spname", by.y = "X")
colnames(aca.d)[colnames(aca.d)=="X"] <- "spname"

chi.d <- merge(chi.range, chi, by.x = "spname", by.y = "X")

# data for all species of two genera
cols <- c("rangefilling","AMT","MTC","AP","PS","SAIcc","SAIcl")

# Linear models
summary(lm(rangefilling ~ ., data = aca.d[,cols]))
summary(lm(rangefilling ~ ., data = chi.d[,cols]))

# Show medians of varibale importance
sapply(aca.d[,cols], median)
sapply(chi.d[,cols], median)


