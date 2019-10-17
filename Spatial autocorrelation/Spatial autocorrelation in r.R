library(devtools)
# devtools::install_github('rspatial/rspatial')
# Install the downloaded package
# install.packages('rspatial_1.0-0.tar.gz', repos = NULL)
library(raster)
library(rspatial)
library(latticeExtra)


################################################################################
### Spatial autocorrelation models, examples
################################################################################

### Data preparation

# Load polygon data
h <- sp_data('houses2000')
# Merge the polygons by county
hh <- aggregate(h, "County")
# Make dataframe and extract some columns
d1 <- data.frame(h)[, c("nhousingUn", "recHouses", "nMobileHom", "nBadPlumbi",
                        "nBadKitche", "Population", "Males", "Females", "Under5", "White",
                        "Black", "AmericanIn", "Asian", "Hispanic", "PopInHouse", "nHousehold", "Families")]
d1a <- aggregate(d1, list(County=h$County), sum, na.rm=TRUE)
d2 <- data.frame(h)[, c("houseValue", "yearBuilt", "nRooms", "nBedrooms",
                        "medHHinc", "MedianAge", "householdS",  "familySize")]
d2 <- cbind(d2 * h$nHousehold, hh=h$nHousehold)
d2a <- aggregate(d2, list(County=h$County), sum, na.rm=TRUE)
d2a[, 2:ncol(d2a)] <- d2a[, 2:ncol(d2a)] / d2a$hh
d12 <- merge(d1a, d2a, by='County')
hh <- merge(hh, d12, by='County')
grps <- 10
brks <- quantile(h$houseValue, 0:(grps-1)/(grps-1), na.rm=TRUE)
p <- spplot(h, "houseValue", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent" )
p + layer(sp.polygons(hh))
brks <- quantile(h$medHHinc, 0:(grps-1)/(grps-1), na.rm=TRUE)
p <- spplot(h, "medHHinc", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")
p + layer(sp.polygons(hh))

### LM
hh$fBadP <- pmax(hh$nBadPlumbi, hh$nBadKitche) / hh$nhousingUn
hh$fWhite <- hh$White / hh$Population
hh$age <- 2000 - hh$yearBuilt
f1 <- houseValue ~ age +  nBedrooms
m1 <- lm(f1, data=hh)
y <- matrix(hh$houseValue)
X <- cbind(1, hh$age, hh$nBedrooms)
ols <- solve(t(X) %*% X) %*% t(X) %*% y
rownames(ols) <- c('intercept', 'age', 'nBedroom')
hh$residuals <- residuals(m1)
brks <- quantile(hh$residuals, 0:(grps-1)/(grps-1), na.rm=TRUE)
spplot(hh, "residuals", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="black")

### Neighbourhood structure
library(spdep)
nb <- poly2nb(hh)
nb[[21]] <- sort(as.integer(c(nb[[21]], 38)))
nb[[38]] <- sort(as.integer(c(21, nb[[38]])))
par(mai=c(0,0,0,0))
plot(hh)
plot(nb, coordinates(hh), col='red', lwd=2, add=TRUE)

resnb <- sapply(nb, function(x) mean(hh$residuals[x]))
cor(hh$residuals, resnb)
plot(hh$residuals, resnb, xlab='Residuals', ylab='Mean adjacent residuals')
lw <- nb2listw(nb)
moran.mc(hh$residuals, lw, 999)

### Spatial lag model
m1s = lagsarlm(f1, data=hh, lw, tol.solve=1.0e-30)
summary(m1s)
brks <- quantile(hh$residuals, 0:(grps-1)/(grps-1), na.rm=TRUE)
p <- spplot(hh, "residuals", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")
print( p + layer(sp.polygons(hh)) )

m1e = errorsarlm(f1, data=hh, lw, tol.solve=1.0e-30)
brks <- quantile(hh$residuals, 0:(grps-1)/(grps-1), na.rm=TRUE)
p <- spplot(hh, "residuals", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")),
            col="transparent")
print( p + layer(sp.polygons(hh)) )
