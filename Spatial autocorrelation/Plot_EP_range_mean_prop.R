#################################################################################################################
### Draw plots; EP vs. species range
#################################################################################################################
source(".//GitHub//Range-fillings//Spatial autocorrelation//F_randomClusterSampling.R")
library(dplyr)
library(ggplot2)

genus_name <- "Chionochloa"

a <- load(paste("Y://", genus_name, "_randomClusterSamples.data", sep = ""))
ran.ep <- get(a)

data2 <- create_dataframe_EP_error(ran.ep)

dat <- read.csv(paste("Y://", genus_name, "EPclimatedata.csv", sep = ""))

dat$spname <- gsub("_", ".", dat$spname)

data2$spname <- gsub("_", ".", data2$spname)

dat2 <- merge(dat, data2, by = "spname")

# Default line plot
p <- ggplot(dat2, aes(x = sp.occ, y = ep.range)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.range, ymax = error.max.range), width = .2, col = "red",
                position = position_dodge(0.05)
  )+
  geom_errorbar(aes(ymin = error.2.5.range, ymax = error.97.5.range), width = .2, col = "yellow",
                position = position_dodge(0.05)
  )

png(paste("Y://", genus_name, "_eprange_cluster.png", sep=""))
# Finished line plot
p + labs(title = genus_name, y = "EP range", x = "Species range") +
  theme_classic()
dev.off()

# Default line plot
p <- ggplot(dat2, aes(x = sp.occ, y = ep.mean)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin = error.min.mean, ymax = error.max.mean), width = .2, col = "red",
                position=position_dodge(0.05)
  ) +
  geom_errorbar(aes(ymin = error.2.5.mean, ymax = error.97.5.mean), width = .2, col = "yellow",
                position = position_dodge(0.05)
  )

png(paste("Y://", genus_name, "_epmean_cluster.png", sep = ""))
# Finished line plot
p + labs(title = genus_name, y = "EP mean", x = "Species range") +
  theme_classic()
dev.off()


# Default line plot
p <- ggplot(dat2, aes(x = sp.occ, y = epcccl.prop)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error.min.prop, ymax = error.max.prop), col = "red") +
  geom_errorbar(aes(ymin = error.2.5.prop, ymax = error.97.5.prop), width = .2, col = "yellow",
                position = position_dodge(0.05)
  )


png(paste("Y://", genus_name, "speciesRange_Prop_potiveEPcc_cl_cluster.png", sep = ""))
# Finished line plot
p + labs(title = genus_name, y = "Proportion of positive EPcc-cl", x = "Species range") +
  theme_classic()
dev.off()
