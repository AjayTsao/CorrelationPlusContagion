arma10.PIT <- function(param, data){
  n <- nrow(data)
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  resids <- rep(0, n)
  resids[2:n] <- datavec[2:n] - ((param[1]/ 100000) + ((param[2]/ 100000) * datavec[1:(n-1)]))
  #
  SSE.matrix <- rep(0, n)
  SSE.matrix[2:n] <- (datavec[2:n] - ((param[1]/ 100000) + ((param[2]/ 100000) * datavec[1:(n-1)]))) ^ 2
  #
  sigma2 <- (1 / (n - 1)) * sum(SSE.matrix)
  sigma <- (sigma2 ^ 0.5)
  #
  U <- matrix(0:0, n, 2)
  Uvec <- rep(0, n)
  Uvec[1] <- pnorm(datavec[1], mean = ((param[1]/ 100000) / (1 - (param[2]/ 100000))), sd = ((sigma2 / (1 - ((param[2]/ 100000) ^ 2))) ^ 0.5) )
  U[,1] <- data[,1]
  #
  #
  Uvec[2:n] <- (pnorm(resids[2:n], mean = 0, sd = sigma))
  U[,2] <- Uvec
  #
  return(U[-1,])
}
