################################################################################################################################
################################################################################################################################
################ STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - QUANTILE REGRESSION SPECIFICATIONS ###############
################################################################################################################################
################################################################################################################################
a10CheckLoss <- function(p, data, tau){
  n <- nrow(data)
  loss <- rep(0, n)
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  qhat <- rep(0,n)
  qhat[2:n] <- (p[1] + p[2] * datavec[1:(n-1)])
  #
  loss[2:n] <- (tau - as.numeric(datavec[2:n] < qhat[2:n])) * (datavec[2:n] - qhat[2:n])
  #
  return(sum(loss))
}
############################################################
############################################################
############################################################
a11CheckLoss <- function(p, ts, data, tau){
  n <- nrow(data)
  loss <- rep(0, n)
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  resids <- rep(0,n)
  for (i in 2:n) {
    resids[i] <- datavec[i] - ((ts[1] / 100000) + ((ts[2] / 100000) * datavec[(i-1)])  + ((ts[3] / 100000) * resids[(i - 1)]))
  }
  #
  qhat <- rep(0,n)
  qhat[2:n] <- (p[1] + (p[2] * datavec[1:(n-1)]) + (p[3] * resids[1:(n-1)]))
  #
  loss[2:n] <- (tau - as.numeric(datavec[2:n] < qhat[2:n])) * (datavec[2:n] - qhat[2:n])
  #
  return(sum(loss)) 
}
############################################################
############################################################
############################################################
g10sigma <- function(ts, data){
  n <- nrow(data)
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  resids <- rep(0,n)
  resids[2:n] <- datavec[2:n] - ((ts[1] / 100000) + ((ts[2] / 100000) * datavec[1:(n-1)]))
  #
  sigma <- rep(0,n)
  sigma[2:n] <- ((ts[3] / 100000) + ((ts[4] / 100000) * (resids[1:(n-1)]^2)))
  #
  return(sigma)
}

g10CheckLoss <- function(p, data, tau, sigma){
  n <- nrow(data)
  loss <- rep(0, n)
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  qhat <- rep(0,n)
  qhat[2:n] <- (p[1] + (p[2] * datavec[1:(n-1)]) + (p[3] * sqrt(sigma[2:n])))
  #
  loss[2:n] <- (tau - as.numeric(datavec[2:n] < qhat[2:n])) * (datavec[2:n] - qhat[2:n])
  #
  return(sum(loss)) 
}
############################################################
############################################################
############################################################
g11sigma <- function(ts, data){
  n <- nrow(data)
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  resids <- rep(0,n)
  resids[2:n] <- datavec[2:n] - ((ts[1] / 100000) + ((ts[2] / 100000) * datavec[1:(n-1)]))
  #
  sigma <- rep(0,n)
  for (i in 2:n){
    sigma[i] <- (ts[3] / 100000) + ((ts[4] / 100000) * (resids[(i-1)]^2)) + ((ts[5] / 100000) * sigma[(i-1)])
  }
  return(sigma)
}

g11CheckLoss <- function(p, data, tau, sigma){
  n <- nrow(data)
  loss <- rep(0, n)
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  qhat <- rep(0,n)
  qhat[2:n] <- (p[1] + (p[2] * datavec[1:(n-1)]) + (p[3] * sqrt(sigma[2:n])))
  #
  loss[2:n] <- (tau - as.numeric(datavec[2:n] < qhat[2:n])) * (datavec[2:n] - qhat[2:n])
  #
  return(sum(loss)) 
}
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
a10quantile <- function(p, data){
  n <- nrow(data)
  quantiles <- matrix(0:0, nrow(data), nrow(p))
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  for (j in 1:nrow(p)){
    for (i in 2:nrow(quantiles)){
      quantiles[i,j] <- (p[j,1] + (p[j,2] * datavec[(i-1)]))
    }    
  }
  return(quantiles[c(-1,-2),])
}
############################################################
############################################################
############################################################
a11quantile <- function(p, ts, data){
  n <- nrow(data)
  quantiles <- matrix(0:0, nrow(data), nrow(p))
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  resids <- rep(0,n)
  for (i in 2:n) {
    resids[i] <- datavec[i] - ((ts[1] / 100000) + ((ts[2] / 100000) * datavec[(i-1)])  + ((ts[3] / 100000) * resids[(i - 1)]))
  }
  #
  for (j in 1:nrow(p)){
    for (i in 2:nrow(quantiles)){
      quantiles[i,j] <- (p[j,1] + (p[j,2] * datavec[(i-1)]) + (p[j,3] * resids[(i-1)]))
    }    
  }
  return(quantiles[c(-1,-2),])
}
############################################################
############################################################
############################################################
g10quantiles <- function(p, ts, data){
  n <- nrow(data)
  quantiles <- matrix(0:0, nrow(data), nrow(p))
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  resids <- rep(0,n)
  resids[2:n] <- datavec[2:n] - ((ts[1] / 100000) + ((ts[2] / 100000) * datavec[1:(n-1)]))
  #
  sigma <- rep(0,n)
  sigma[2:n] <- ((ts[3] / 100000) + ((ts[4] / 100000) * (resids[1:(n-1)]^2)))
  #
  for (j in 1:nrow(p)){
    for (i in 2:nrow(quantiles)){
      quantiles[i,j] <- (p[j,1] + (p[j,2] * datavec[(i-1)]) + (p[j,3] * sqrt(sigma[(i-1)])))
    }    
  }
  #
  return(quantiles[c(-1,-2),])
}
############################################################
############################################################
############################################################
g11quantiles <- function(p, data, tau, sigma){
  n <- nrow(data)
  quantiles <- matrix(0:0, nrow(data), nrow(p))
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  for (j in 1:nrow(p)){
    for (i in 2:nrow(quantiles)){
      quantiles[i,j] <- (p[j,1] + (p[j,2] * datavec[(i-1)]) + (p[j,3] * sqrt(sigma[(i-1)])))
    }    
  }
  #
  return(quantiles[c(-1,-2),])
}
############################################################
############################################################
############################################################
SAVQuantiles <- function(p, data){
  n <- nrow(data)
  quantiles <- matrix(0:0, nrow(data), nrow(p))
  datavec <- rep(0, n)
  datavec <- data[,2]
  #
  for (j in 1:nrow(p)) {
    quantiles[2,j] <- quantile(datavec, probs =  (j/(nrow(p)+1)))
    for (i in 3:nrow(quantiles)) {
      quantiles[i, j]  <- p[j,1] + (p[j,2] * datavec[(i-1)]) + (p[j,3] * quantiles[(i-1),j]) + 
        ((-1) * p[j,2] * p[j,3] * datavec[(i-2)]) + (p[j,4] * abs(datavec[(i-1)])) 
    }
  }
  return(quantiles[c(-1,-2),])
}
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################

















































