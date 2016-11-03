cappgarchLL <- function(param, data){
  n <- nrow(data)
  #
  resids <- rep(0, n)
  sigma <- rep(0, n)
  #

  resids[1:(n-1)] <- data[2:n,2] - ((param[1] / 100000) + ((param[2] / 100000) * data[1:(n-1),2]))

  #
  for (i in 2:n){
    sigma[i] <- ((param[3] / 100000) + ((param[4] / 100000) * abs(data[(i-1),2])) + ((param[5] / 100000) * (sigma[(i-1)] ^ 0.5))) ^ 2 
  }
  #
  density.matrix <- rep(0, n)

  density.matrix[2:n] <- log(dnorm(resids[2:n], mean = 0, sd = (sigma[2:n] ^ 0.5)))

  #
  LL <- sum(density.matrix)
  return(LL)
}