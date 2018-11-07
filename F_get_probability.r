####################################################################
### Add occurrence probability to the data frame of PCA scores
####################################################################

get_occurrenceProbability_to_scores <- function(i, # spname
                                                pred, # data frame of predicted probability
                                                scores # PCA data
){
  
  # Get probability of the species
  prob1 <- pred[names(pred) == gsub("_",".", i)]
  
  prob.d <- data.frame(cbind(coordinates(prob1[[1]]), values(prob1[[1]])))
  colnames(prob.d)[3] <- paste("prob", i, sep = "_") 
  
  scores2 <- merge(prob.d, scores, by = c("x","y"))
  
  return(scores2)
  
}