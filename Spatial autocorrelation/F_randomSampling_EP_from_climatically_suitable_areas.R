##############################################################
# function for Random sampling from predicted presence
############################################################

randomsampling <- function(spname,
  data1, # data with climate values 
  data2 = NULL, # predicted presence data. Get samples from this data
  valueName, # a colname of value to sample, e.g., "EPcc"
  iteration,
  coordinateNames
){
  ran.ep <- list()

  for(i in 1:length(spname)){
    
    ### If data2 isn't given, just sample randomly from data1
    if (is.null(data2)){
      scores.ep.binary <-  data1
    }else{
      scores.ep.binary <- merge(data1, data2[[i]], by = coordinateNames)
    }
    
    test  <- list()
    
    for(j in 1:iteration){
      
      test.row <- sample(1:nrow(scores.ep.binary), sum(scores.ep.binary[, spname[i]]))
      test[[j]] <- scores.ep[test.row, valueName]
      
    }
    ran.ep[[i]] <- test
  }
  
  random.ep <- lapply(ran.ep, function(x){
    do.call(rbind, x)
  }
  )
  
  return(random.ep)
  
}
