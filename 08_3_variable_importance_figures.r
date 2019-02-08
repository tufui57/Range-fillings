
############################################################################################################
## Visualize varibale imporatance of SAI
############################################################################################################

folder.name = "SAI_cinl7Feb19"

### Load variable importance

chi <- read.csv(paste("Y://averaged_rank_importance_Chionochloa", folder.name, ".csv", sep = ""))
aca <- read.csv(paste("Y://averaged_rank_importance_Acaena", folder.name, ".csv", sep = ""))

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

# The following figure looks wrong but I have no script how I obtained this data
# ############################################################################################################
# ## Visualize SAI diferences due to neighbourhood size
# ############################################################################################################
# 
# ### Load SAI with different neighbourhood sizes
# sai <- read.csv("SAI_current_LGM_dif.csv")
# 
# png("SAI_boxplot.png")
# par(las = 2,
#     mar = c(5.1 +2.2, 4.1, 4.1-2.2, 2.1)
# )
# boxplot(sai[, c(-1,-2,-3, -ncol(sai))],
#         main = "SAI with 20, 50 and 100 km neighbourhood",
#         ylab = "SAI")
# dev.off()

############################################################################################################
## Visualize SAI diferences due to neighbourhood size
############################################################################################################

sai.c <- list()
for(i in as.character(c(10,20,50,100,1500))){
  a <- load(paste("SAI_5km_currentInCurrent_",  i, "kmWindow_4var.data", sep = ""))
  a <- get(a)
  sai.c[[i]] <- unlist(a)
}


sai.lgm <- list()
for(i in as.character(c(10,20,50,100,1500))){
  a <- load(paste("SAI_5km_LGMInLGM_",  i, "kmWindow_4var.data", sep = ""))
  a <- get(a)
  sai.lgm[[i]] <- unlist(a)
}



### Load SAI with different neighbourhood sizes
sai.c2 <- do.call(cbind,sai.c)
sai.lgm2 <- do.call(cbind,sai.lgm)

png("SAI_current_boxplot.png")
par(las = 2,
    mar = c(5.1 +2.2, 4.1, 4.1-2.2, 2.1)
    )
boxplot(sai.c2,
        main = "SAI with 10, 20, 50 and 100 km neighbourhood and whole NZ",
        ylab = "SAI of current climate")
dev.off()

png("SAI_lgm_boxplot.png")
par(las = 2,
    mar = c(5.1 +2.2, 4.1, 4.1-2.2, 2.1)
)
boxplot(sai.lgm2,
        main = "SAI with 10, 20, 50 and 100 km neighbourhood and whole NZ",
        ylab = "SAI of LGM climate")
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
