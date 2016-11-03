cappgarch.sigma <- function(param, data, alpha){
  n <- nrow(data)
  #
  resids <- matrix(0:0, n, 1)
  sigma <- matrix(0:0, n, 1)
  #
  for (i in 2:n){
    resids[i,1] <- data[i,2] - ((param[1] / 100000) + ((param[2] / 100000) * data[(i-1),2]))
  }
  #
  for (i in 2:n){
    sigma[i,1] <- ((param[3] / 100000) + ((param[4] / 100000) * abs(data[(i-1),2])) + ((param[5] / 100000) * (sigma[(i-1),1] ^ 0.5))) ^ 2 
  }
  #
  VAR <- matrix(0:0, n-1, 1)
  for (i in 2:n){
  VAR[(i-1),1] <- ((param[1] / 100000) + ((param[2] / 100000) * data[(i-1),2])) + qnorm(p = alpha, mean = 0, sd = (sigma[i,1] ^ 0.5))
  }
  #
  return(VAR)
}