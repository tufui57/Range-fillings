############################################################################################################
## SDMs considering spatial autocorrelation
############################################################################################################

### Autocovariate regression
# The validity of this approach strongly hinges on the correct choice of the neighbourhood scheme!
#   Using 'style="B"' ensures symmetry of the neighbourhood matrix (i.e. w_nm = w_mn). Please
#   see Bardos et al. (2015) for details.

library(spdep)

autocov_dist(z, xy, nbs = 1, type = "inverse", zero.policy = NULL,
             style = "B", longlat=NULL)

if (require(rgdal, quietly=TRUE)) {
  example(columbus, package="spData")

  xy <- cbind(columbus$X, columbus$Y)
  # Weighting scheme = equal
  ac1a <- autocov_dist(columbus$CRIME, xy, nbs=10, style="B",
                       type="one")
  # Weighting scheme = inverse distance
  acinva <- autocov_dist(columbus$CRIME, xy, nbs=10, style="B",
                         type="inverse")
  # Weighting scheme = (inverse distance)^2
  acinv2a <- autocov_dist(columbus$CRIME, xy, nbs=10, style="B",
                          type="inverse.squared")


  plot(ac1a ~ columbus$CRIME, pch=16, asp=1)
  points(acinva ~ columbus$CRIME, pch=16, col="red")
  points(acinv2a ~ columbus$CRIME, pch=16, col="blue")
  abline(0,1)

  ### Compare autocovariate values and actual values
  # neighbourhood structure by distance
  nb <- dnearneigh(xy, 0, 10)
  # Convert the above to list objest with weighting
  lw <- nb2listw(nb, style="B")
  # multiply actual values by the neighbourhood distance
  ac1b <- lag(lw, columbus$CRIME)
  all.equal(ac1b, ac1a)

  # measure distance between neighbourhood cells
  nbd <- nbdists(nb, xy)
  # Invert the distances
  gl <- lapply(nbd, function(x) 1/x)
  lw <- nb2listw(nb, glist=gl) # glist = list of general weights corresponding to neighbours
  acinvb <- lag(lw, columbus$CRIME)
  all.equal(acinvb, acinva)

  gl2 <- lapply(nbd, function(x) 1/(x^2))
  lw <- nb2listw(nb, glist=gl2)
  acinv2b <- lag(lw, columbus$CRIME)
  all.equal(acinv2b, acinv2a)

  ### GLM
  summary(glm(CRIME ~ HOVAL + ac1b, data=columbus, family="gaussian"))
  summary(glm(CRIME ~ HOVAL, data=columbus, family="gaussian"))
  ### SAR with another neighbourhood scheme from "B"
  summary(
    spautolm(columbus$CRIME ~ HOVAL, data=columbus,
           listw=nb2listw(nb, style="W"))
  )

}

### Genaralised estimation equation
library(gee)

data(warpbreaks)
## marginal analysis of random effects model for wool (what's difference from GLMM?)
summary(gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="exchangeable", family = poisson))

### Comparison
library(glmmML)
summary(glmmML(breaks ~ tension, cluster = wool, data = warpbreaks, family = poisson))

## test for serial correlation in blocks
summary(gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="AR-M", Mv=1))
if(require(MASS)) {
  data(OME)
  ## not fully appropriate link for these data.
  (fm <- gee(cbind(Correct, Trials-Correct) ~ Loud + Age + OME, id = ID,
             data = OME, family = binomial, corstr = "exchangeable"))
  summary(fm)
}



