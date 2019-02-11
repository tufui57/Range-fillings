###################################################################################
### Range filling analyses
###################################################################################

########################################################################################
# Compare range filling between Acaena and Chionochloa on 5km resolution
########################################################################################

aca <- read.csv("Y://rangefilling_AcaenaSAI_cinl8Feb19_ensamble.csv")
chi <- read.csv("Y://rangefilling_ChionochloaSAI_cinl8Feb19_ensamble.csv")

### Compare range filling between genera
var.test(aca$rangefilling, chi$rangefilling)

t.test(aca$rangefilling, chi$rangefilling, var.equal = FALSE)

########################################################################################
# on 1km resolution
########################################################################################
aca <- read.csv("Y://rangefilling_comparison_Acaena.csv")
chi <- read.csv("Y://rangefilling_comparison_Chionochloa.csv")

### Compare range filling between genera
var.test(aca$rangefilling, chi$rangefilling)

t.test(aca$rangefilling, chi$rangefilling, var.equal = FALSE)

boxplot(aca$rangefilling, chi$rangefilling)
