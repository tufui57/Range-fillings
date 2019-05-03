
############################################################################################################
## Visualize varibale imporatance of SAI
############################################################################################################
# 5km resolution, four bioclim
#folder.name = "5km_15Jan19"
# 5km resolution, four bioclim + SAIcc + SAIcl + SAIcc-SAIcl
folder.name = "SAIdiff_4Mar19"

### Load variable importance

aca <- read.csv(paste("Y://averaged_rank_importance_Acaena", folder.name, ".csv", sep = ""))
chi <- read.csv(paste("Y://averaged_rank_importance_Chionochloa", folder.name, ".csv", sep = ""))

### Boxplot 

colnames(aca)[-1] <- c("AMT","MTC","AP","PS","SAIcc","SAIcl", "SAIcc-SAIcl")
colnames(chi)[-1] <- c("AMT","MTC","AP","PS","SAIcc","SAIcl", "SAIcc-SAIcl")

# Reverse rank axis
library(reshape)
meltaca <- melt(aca[,-1])
meltchi <- melt(chi[,-1])

png(paste("Acaena_varibale_importance", folder.name,".png", sep=""))

par(cex.lab=1.5, las=3)
boxplot(meltaca$value ~ meltaca$variable,
        ylim = rev(range(meltaca$value)),
        main = "Acaena",
        ylab = "Variable Importance")

dev.off()

png(paste("Chionochloa_varibale_importance", folder.name,".png", sep=""))

par(cex.lab=1.5, las=3)
boxplot(meltchi$value ~ meltchi$variable,
        ylim = rev(range(meltchi$value)),
        main = "Chionochloa",
        ylab = "Variable Importance")

dev.off()

## Flipped boxplots

library(forcats)
meltaca$variable <- fct_rev(meltaca$variable)
meltchi$variable <- fct_rev(meltchi$variable)

png(paste("Acaena_varibale_importance", folder.name,".png", sep=""))

boxplot(meltaca$value ~ meltaca$variable,
        ylim = rev(range(meltaca$value)),
        horizontal=TRUE, # Flip the fig.
        las=1,
        main = "Acaena",
        ylab = "Variable Importance")

dev.off()

png(paste("Chionochloa_varibale_importance", folder.name,".png", sep=""))
boxplot(meltchi$value ~ meltchi$variable,
        ylim = rev(range(meltchi$value)),
        horizontal=TRUE,
        las=1,
        main = "Chionochloa",
        ylab = "Variable Importance")

dev.off()

############################################################################################################
## Group varibale imporatance by species range size
############################################################################################################

### Acaena

trait <- read.csv("Y://Acaena_data_analyses18sep.csv")
trait$X <- gsub("_", ".", trait$spname)

dat <- merge(aca, trait, by = "X")
dat$size <- ifelse(dat$total < 100, "Small",
                   ifelse(dat$total > 100 & dat$total < 500, "Mid", "Large"
                   )
)

write.csv(dat[, c("X","size", "total","AMT","MTC","AP","PS","SAIcc", "SAIcl", "SAIcc-SAIcl")], 
          "Acaena_var_imp_size.csv")

png("Y://Acaena_varImp_grouped_by_range.png",
    width = 800, height = 550
)

par(mfrow = c(1,3), cex=1)
for(i in c("Small","Mid","Large")){
  dat2 <- dat[dat$size == i,]
  meltaca <- melt(dat2[, c("AMT","MTC","AP","PS","SAIcc", "SAIcl", "SAIcc-SAIcl")])
  
  par(cex.lab=1.5, las=3)
  boxplot(meltaca$value ~ meltaca$variable,
        ylim = rev(range(meltaca$value)),
        main = paste(i, "ranged Acaena species\n", 
                     ifelse(i=="Small", "n < 50",
                            ifelse(i=="Mid", "50 < n < 100", "n > 100")
                                                        )),
        ylab = "Variable Importance"
        )
}
dev.off()

### Chionochloa
trait <- read.csv("Y://Analysis_chion.csv")
trait$X <- gsub("_", ".", trait$spname)

dat <- merge(chi, trait, by = "X")
dat$size <- ifelse(dat$occ < 100, "Small",
                   ifelse(dat$occ > 100 & dat$occ < 400, "Mid", "Large"
                   )
)

write.csv(dat[, c("X","size", "occ","AMT","MTC","AP","PS","SAIcc", "SAIcl", "SAIcc-SAIcl")], 
          "Chionochloa_var_imp_size.csv")

png("Y://Chionochloa_varImp_grouped_by_range.png",
    width = 800, height = 550
    )

par(mfrow = c(1,3), cex=1)
for(i in c("Small","Mid","Large")){
  dat2 <- dat[dat$size == i,]
  meltaca <- melt(dat2[, c("AMT","MTC","AP","PS","SAIcc", "SAIcl", "SAIcc-SAIcl")])
  
  par(cex.lab=1.5, las=3)
  boxplot(meltaca$value ~ meltaca$variable,
          ylim = rev(range(meltaca$value)),
          main = paste(i, "ranged Chionochloa species\n", 
                       ifelse(i=="Small", "n < 100",
                              ifelse(i=="Mid", "100 < n < 400", "n > 400")
                       )),
          ylab = "Variable Importance"
  )
}
dev.off()

############################################################################################################
## Linear models for range filling and variable importances
############################################################################################################

### Load range filling data
aca.range <- read.csv(paste("Y://rangefilling_Acaena", folder.name, ".csv", sep=""))
chi.range <- read.csv(paste("Y://rangefilling_Chionochloa", folder.name,".csv", sep=""))

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


