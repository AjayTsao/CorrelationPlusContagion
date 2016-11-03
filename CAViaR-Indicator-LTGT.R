CAViaRIndicatorLTGT <- function(Quantiles, Returns) {
  indicator <- matrix(0:0, nrow(Quantiles), ncol(Quantiles))
  #
  indicator[1:(nrow(Returns) - 1) , 1] <- Returns[2:nrow(Returns) , 1]
  
  pb <- txtProgressBar(min = 0, max = ncol(Quantiles), style = 3)
  #
  for (i in 2:ncol(indicator)) {
    for (j in 2:nrow(indicator)) {
      if (i <= (0.5 * ncol(Quantiles) + 1)) {
        indicator[j,i] <- as.numeric(Returns[j,2] <= Quantiles[j,i])
      } else {
        indicator[j,i] <- as.numeric(Returns[j,2] > Quantiles[j,i])
      }
    }
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(indicator)
}