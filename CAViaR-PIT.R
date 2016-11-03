CAViaR.PIT <- function(Quantiles, Returns){
  ProbTransform <- matrix(0:0, (nrow(Quantiles)-1), 2)
  ProbTransform[,1] <- Quantiles[2:nrow(Quantiles),1]
  #
  for (i in 1:nrow(ProbTransform)){
    for (j in 2:ncol(Quantiles)){
      ProbTransform[i,2] <- ProbTransform[i,2] + (as.numeric(Quantiles[(i+1),j] <= Returns[(i+1),2]) / (ncol(Quantiles)-1))
    }
  }
  return(ProbTransform)
}