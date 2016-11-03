CAViaRnlmCheckLoss <- function(ReturnsSeries, TauCount){
  EstimateOutput <- matrix(0:0, TauCount, 4)
  colnames(EstimateOutput) <- c("Beta0", "Beta1", "Beta2", "Beta3")
  pb <- txtProgressBar(min = 0, max = (TauCount+1), style = 3)
  #
  estimate <- optim(par = c(0,0,0,0), CAViaRLoss, Returns = ReturnsSeries, Tau = (1/(TauCount + 1)))
  EstimateOutput[1,1] <- estimate$par[1]
  EstimateOutput[1,2] <- estimate$par[2]
  EstimateOutput[1,3] <- estimate$par[3]
  EstimateOutput[1,4] <- estimate$par[4]
  for (i in 2:TauCount){
    estimate <- optim(par = c(EstimateOutput[(i-1),1],EstimateOutput[(i-1),2],EstimateOutput[(i-1),3],EstimateOutput[(i-1),4]),
                      CAViaRLoss, Returns = ReturnsSeries, Tau = (i/(TauCount + 1)))
    EstimateOutput[i,1] <- estimate$par[1]
    EstimateOutput[i,2] <- estimate$par[2]
    EstimateOutput[i,3] <- estimate$par[3]
    EstimateOutput[i,4] <- estimate$par[4]
    
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(EstimateOutput)
}