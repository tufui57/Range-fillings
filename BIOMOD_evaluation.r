data(Sp.Env)

Initial.State(Response=Sp.Env[,12:13], Explanatory=Sp.Env[,4:10], 
              IndependentResponse=NULL, IndependentExplanatory=NULL)

Models(GAM = TRUE, NbRunEval = 1, DataSplit = 80,
       Yweights=NULL, Roc=TRUE, Optimized.Threshold.Roc=TRUE, Kappa=F, TSS=F, KeepPredIndependent = FALSE, VarImport=0,
       NbRepPA=0, strategy="circles", coor=CoorXY, distance=2, nb.absences=1000)


load("pred/Pred_Sp277")

data=cbind(Sp.Env[,1], Sp.Env[,13], Pred_Sp277[,3,1,1]/1000)

plotroc <- roc.plot.calculate(data)

### Plot the change in sensitivity in function of the threshold
plot(plotroc$threshold, plotroc$sensitivity, type="l", col="blue ")
### Plot the change in specificity in function of the threshold
lines(plotroc$threshold, plotroc$specificity)
### Plot the change in one criteria to select the threshold: (SE+SP)/2 in function of the threshold
lines(plotroc$threshold, (plotroc$specificity+plotroc$sensitivity)/2, col="red")
### Add a red circle when maximising one criteria to select the threshold: (SE+SP)/2 
points(plotroc$threshold[which((plotroc$specificity+plotroc$sensitivity)/2 ==max((plotroc$specificity+plotroc$sensitivity)/2))], max((plotroc$specificity+plotroc$sensitivity)/2), col="red")
### Plot the change in one criteria to select the threshold: min(abs((SE-SP)) in function of the threshold. This is the one in BIOMOD
lines(plotroc$threshold, abs(plotroc$specificity-plotroc$sensitivity), col="orange")
### Add a orange circle when minimising to select the threshold: min(abs((SE-SP)). This is the one in BIOMOD
points(plotroc$threshold[which(abs(plotroc$specificity-plotroc$sensitivity) ==min(abs(plotroc$specificity-plotroc$sensitivity)))], max((plotroc$specificity+plotroc$sensitivity)/2), col="orange")

