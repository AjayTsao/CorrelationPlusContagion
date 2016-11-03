CAViaRQuantiles <- function(CAViaRBeta, Returns, TauCount){
  quantiles <- matrix(0:0, nrow(Returns), (nrow(CAViaRBeta)+1))
  #
  pb <- txtProgressBar(min = 0, max = ncol(quantiles), style = 3)
  #
  quantiles[ ,1] <- Returns[ ,1]
  #
  for (i in 2:ncol(quantiles)){
    quantiles[2,i] <- quantile(Returns[ ,2], probs =  (i/(TauCount+1)))
    for (j in 3:nrow(quantiles)){
      quantiles[j,i] <- (CAViaRBeta[(i-1),"Beta0"]) + (CAViaRBeta[(i-1),"Beta1"] * Returns[(j-1),2]) + (CAViaRBeta[(i-1),"Beta2"] * quantiles[(j-1),i]) + ((-1) * CAViaRBeta[(i-1),"Beta1"] * CAViaRBeta[(i-1),"Beta2"] * Returns[(j-2),2]) + (CAViaRBeta[(i-1),"Beta3"] * abs(Returns[(j-1),2])) 
      
    }
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  return(quantiles)
}