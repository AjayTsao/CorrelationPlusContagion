arma11.PIT <- function(param, data){
  n <- nrow(data)
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  resids <- rep(0, n)
  for (i in 2:n){
    resids[i] <- datavec[i] - ((param[1]/ 100000) + ((param[2]/ 100000) * datavec[(i-1)]) + ((param[3]/ 100000) * resids[(i-1)]))
  }
  #
  SSE.matrix <- rep(0, n)
  SSE.matrix[2:n] <- resids[2:n]^2
  #
  sigma2 <- (1 / (n - 1)) * sum(SSE.matrix)
  sigma <- (sigma2 ^ 0.5)
  #
  U <- matrix(0:0, n, 2)
  U[,1] <- data[1:n,1]
  #
  Uvec <- rep(0,n)
  Uvec[2:n] <- pnorm(resids[2:n], mean = 0, sd = sigma)
  #
  U[,2] <- Uvec
   return(U[-1,])
}
