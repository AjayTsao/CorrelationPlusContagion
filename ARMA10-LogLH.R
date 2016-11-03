arma10LL <- function(param, data){
    n <- nrow(data)
    datavec <- rep(0, n)
    datavec <- data[,2]
    #
    resids <- rep(0, n)
    resids[2:n] <- datavec[2:n] - ((param[1] / 100000) + ((param[2] / 100000) * datavec[1:(n-1)]))
    #
    SSE.matrix <- rep(0, n)
    SSE.matrix <- resids^2
    #
    sigma2 <- (1 / (n - 1)) * sum(SSE.matrix)
    sigma <- (sigma2 ^ 0.5)
    #
    density.matrix <- rep(0, n)
    density.matrix[1] <- log(dnorm(datavec[1], mean = ((param[1]/ 100000) / (1 - (param[2]/ 100000))), 
                                   sd = ((sigma2 / (1 - ((param[2]/ 100000) ^ 2))) ^ 0.5) ))
    density.matrix[2:n] <- log(dnorm(resids[2:n], mean = 0, sd = sigma))
    #
    #
    LL <- sum(density.matrix)
    return(LL)
}
