CAViaRLoss <- function(beta, Returns, Tau, initq) {
  #Size vectors
  n <- nrow(Returns)
  qReturn <- rep(0, n)
  Loss <- rep(0, n)
  RetC <- Returns[,2]
  
  # Estimate unconditional quantile of Tau, sets initial value
  qReturn[2] <-  initq
  
  for (i in 3:n) {
    qReturn[i] <- ((1 * beta[1]) + (RetC[(i-1)] * beta[2]) + 
                     (qReturn[(i-1)] * beta[3]) + ((-1) * RetC[(i-2)] * (beta[2] * beta[3])) + (abs(RetC[(i-1)]) * beta[4]))
    #
    
  }
  Loss[3:n] <-  (RetC[3:n] - qReturn[3:n]) * (Tau - as.numeric(RetC[3:n] < qReturn[3:n]))
  sumLoss <- sum(Loss)
  return(sumLoss)
}