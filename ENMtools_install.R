###################################################################################################
### Source all functions of ENMtools manually
###################################################################################################
# I never be able to install ENMtools through Github and local zip (12.Dec.2018).

library(R.utils)
sourceDirectory("C:\\Users\\nomur\\Downloads\\ENMTools-master\\ENMTools-master\\R")

library(lhs)
library(ggplot2)
library(knitr)
library(viridis)


###################################################################################################
### Install ecospat from a local file to solve the installation problem of ENMTools
###################################################################################################

library(devtools)
install_local("C:\\Users\\nomur\\Downloads\\ecospat_3.0.tar.gz")

install_local("C:\\Users\\nomur\\Downloads\\ENMTools-master.zip")
