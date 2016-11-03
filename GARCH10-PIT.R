garch10.PIT <- function(param, data){
  n <- nrow(data)
  datavec <- rep(0,n)
  datavec <- data[,2]
  #
  resids <- rep(0, n)
  sigma <- rep(0, n)
  #
  resids[2:n] <- datavec[2:n] - ((param[1]/ 100000) + ((param[2]/ 100000) * datavec[1:(n-1)]))
  #
  sigma[2:n] <- ((param[3]/ 100000) + ((param[4]/ 100000) * (resids[1:(n-1)] ^ 2)))
  #
  U <- matrix(0:0, n, 2)
  U[,1] <- data[1:n,1]
  Uvec <- rep(0, n)
  Uvec[2:n] <- pnorm(resids[2:n], mean = 0, sd = (sigma[2:n] ^ 0.5))
  U[,2] <- Uvec
  #
  return(U[-1,])
}