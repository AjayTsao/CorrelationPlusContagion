################################################################################################################################
################################################################################################################################
################### STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - ASX 100 Portfolio and Returns #################
################################################################################################################################
################################################################################################################################
PriceRead <- read.csv("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/MLE and CAViaR/ASXPortfolio/ASX200.csv",
                    stringsAsFactors=FALSE, header = T)
#######################################
#######################################
#######################################
#######################################
ASXReturns <- matrix(0:0, (nrow(PriceRead)-1), (ncol(PriceRead)-1))
colnames(ASXReturns) <- colnames(PriceRead[,-1])
rownames(ASXReturns) <- PriceRead[-1,1]
########################################
########################################
for (i in 1:ncol(ASXReturns)){
  ASXReturns[,i] <- diff(log(PriceRead[,(i+1)]))
}
########################################
########################################
ARMA10Param <- matrix(0:0, ncol(ASXReturns), 2)
rownames(ARMA10Param) <- colnames(ASXReturns)
#
ARMA11Param <- matrix(0:0, ncol(ASXReturns), 3)
rownames(ARMA11Param) <- colnames(ASXReturns)
#
GARCH10Param <- matrix(0:0, ncol(ASXReturns), 4)
rownames(GARCH10Param) <- colnames(ASXReturns)
#
GARCH11Param <- matrix(0:0, ncol(ASXReturns), 5)
rownames(GARCH11Param) <- colnames(ASXReturns)
#
SAVParam <- matrix(0:0, ncol(ASXReturns), 5)
rownames(SAVParam) <- colnames(ASXReturns)
########################################
########################################
ASXAIC <- matrix(0:0, ncol(ASXReturns), 6)
rownames(ASXAIC) <- colnames(ASXReturns)
colnames(ASXAIC) <- c("ARMA10", "ARMA11", "GARCH10", "GARCH11", "SAV", "Best")
#
ASXBIC <- matrix(0:0, ncol(ASXReturns), 6)
rownames(ASXBIC) <- colnames(ASXReturns)
colnames(ASXBIC) <- c("ARMA10", "ARMA11", "GARCH10", "GARCH11", "SAV", "Best")
########################################
########################################
N <- nrow(ASXReturns)
for (i in 1:ncol(ASXReturns)){
  pb <- txtProgressBar(min = 1, max = 57, style = 3)
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  ###############
  A10temp <- optim(par = rep(500,2), fn = arma10LL, data = ASXReturns[,c(1,i)], control = list(maxit = 15000, fnscale = -1))
  ARMA10Param[i,] <- A10temp$par
  ASXAIC[i,1] <- AICBIC(A10temp$value, n = N, k = 2)$AIC
  ASXBIC[i,1] <- AICBIC(A10temp$value, n = N, k = 2)$BIC
  #########################
  A11temp <- optim(par = rep(500,3), fn = arma11LL, data = ASXReturns[,c(1,i)], control = list(maxit = 15000, fnscale = -1))
  ARMA11Param[i,] <- A11temp$par
  ASXAIC[i,2] <- AICBIC(A11temp$value, n = N, k = 3)$AIC
  ASXBIC[i,2] <- AICBIC(A11temp$value, n = N, k = 3)$BIC
  #########################
  G10temp <- optim(par = rep(5000,4), fn = garch10LL, data = ASXReturns[,c(1,i)], control = list(maxit = 15000, fnscale = -1))
  GARCH10Param[i,] <- G10temp$par
  ASXAIC[i,3] <- AICBIC(G10temp$value, n = N, k = 4)$AIC
  ASXBIC[i,3] <- AICBIC(G10temp$value, n = N, k = 4)$BIC
  #########################
  G11temp <- optim(par = rep(5000,5), fn = garch11LL, data = ASXReturns[,c(1,i)], control = list(maxit = 15000, fnscale = -1))
  GARCH11Param[i,] <- G11temp$par
  ASXAIC[i,4] <- AICBIC(G11temp$value, n = N, k = 5)$AIC
  ASXBIC[i,4] <- AICBIC(G11temp$value, n = N, k = 5)$BIC
  #########################
  SAVtemp <- optim(par = rep(5000,5), fn = cappgarchLL, data = ASXReturns[,c(1,i)], control = list(maxit = 15000, fnscale = -1))
  SAVParam[i,] <- SAVtemp$par
  ASXAIC[i,5] <- AICBIC(SAVtemp$value, n = N, k = 5)$AIC
  ASXBIC[i,5] <- AICBIC(SAVtemp$value, n = N, k = 5)$BIC
  #
  ASXAIC[i,6] <- which.min(ASXAIC[i,])
  ASXBIC[i,6] <- which.min(ASXBIC[i,])
  #############
}
########################################
########################################
########################################
########################################
########################################
########################################
ASXtimeseriesU <- matrix(0:0, (nrow(ASXReturns)-1), ncol(ASXReturns))
rownames(ASXtimeseriesU) <- rownames(ASXReturns[-1,])
colnames(ASXtimeseriesU) <- colnames(ASXReturns)
########################################
for (i in 1:ncol(ASXtimeseriesU)){
  Pick <- ASXAIC[i,6]
  #
  if (Pick == 1){
    Utemp <- arma10.PIT(ARMA10Param[i,], ASXReturns[,c(1,i)])
  }
  if (Pick == 2){
    Utemp <- arma11.PIT(ARMA11Param[i,], ASXReturns[,c(1,i)])
  }
  if (Pick == 3){
    Utemp <- garch10.PIT(GARCH10Param[i,], ASXReturns[,c(1,i)])
  }
  if (Pick == 4){
    Utemp <- garch11.PIT(GARCH11Param[i,], ASXReturns[,c(1,i)])
  }
  if (Pick == 5){
    Utemp <- cappgarch.PIT(SAVParam[i,], ASXReturns[,c(1,i)])
  }
  #
  ASXtimeseriesU[,i] <- Utemp[,2]
}
########################################
########################################
########################################
########################################
########################################
########################################
for (i in 1:nrow(ASXtimeseriesU)){
  for (j in 1:ncol(ASXtimeseriesU)){
    if (ASXtimeseriesU[i,j] < 0.0001) ASXtimeseriesU[i,j] <- 0.0001 
    if (ASXtimeseriesU[i,j] > 0.9999) ASXtimeseriesU[i,j] <- 0.9999
  }
}
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
#ASX QUANTILE REGRESSION PIT
ASXqrU <- matrix(0:0, (nrow(ASXReturns)-2), ncol(ASXReturns))
colnames(ASXqrU) <- colnames(ASXReturns)
rownames(ASXqrU) <- rownames(ASXqrU)
#
ASXtau <- (1:199)/200
#

for (i in 1:ncol(ASXqrU)){
  pb <- txtProgressBar(min = 1, max = 57, style = 3)
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  ############
  ############
  if (ASXAIC[i,6] == 1){
    temppar <- matrix(0:0, length(ASXtau), 2)
    #####
    for (j in 1:length(ASXtau)){
      temppar[j,] <- optim(par = c(0.1,0.1), fn = a10CheckLoss, data = ASXReturns[,c(1,i)], tau = ASXtau[j])$par
    }
    #####
    tempq <- a10quantiles(temppar, ASXReturns[,c(1,i)])
    tempU <- rep(0, nrow(tempq))
    for (k in 1:length(tempU)){
      tempU[k] <- (sum(tempq[k,] <= ASXReturns[(k+2),i]) / (length(ASXtau)+1))
    }
    ASXqrU[,i] <- tempU
  }
  ############
  ############
  if (ASXAIC[i,6] == 2){
    temppar <- matrix(0:0, length(ASXtau), 3)
    #####
    for (j in 1:length(ASXtau)){
      temppar[j,] <- optim(par = c(0.1,0.1,0.1), fn = a11CheckLoss, data = ASXReturns[,c(1,i)], 
                           tau = ASXtau[j], ts = c(ARMA11Param[i,1], ARMA11Param[i,2], ARMA11Param[i,3]))$par
    }
    #####
    tempq <- a11quantiles(temppar, ASXReturns[,c(1,i)], ts = c(ARMA11Param[i,1], ARMA11Param[i,2], ARMA11Param[i,3]))
    tempU <- rep(0, nrow(tempq))
    for (k in 1:length(tempU)){
      tempU[k] <- (sum(tempq[k,] <= ASXReturns[(k+2),i]) / (length(ASXtau)+1))
    }
    ASXqrU[,i] <- tempU
  }
  ############
  ############
  if (ASXAIC[i,6] == 3){
    temppar <- matrix(0:0, length(ASXtau), 3)
    sigmatemp <- g10sigma(ts = c(GARCH10Param[i,1], GARCH10Param[i,2], GARCH10Param[i,3], GARCH10Param[i,4]),
                          data = ASXReturns[,c(1,i)])
    #####
    for (j in 1:length(ASXtau)){
      temppar[j,] <- optim(par = c(0.1,0.1,0.1), fn = g10CheckLoss, data = ASXReturns[,c(1,i)], 
                           tau = ASXtau[j], sigma = sigmatemp)$par
    }
    #####
    tempq <- g10quantiles(temppar, ASXReturns[,c(1,i)], 
                         ts = c(GARCH10Param[i,1], GARCH10Param[i,2], GARCH10Param[i,3], GARCH10Param[i,4]))
    tempU <- rep(0, nrow(tempq))
    for (k in 1:length(tempU)){
      tempU[k] <- (sum(tempq[k,] <= ASXReturns[(k+2),i]) / (length(ASXtau)+1))
    }
    ASXqrU[,i] <- tempU
  }
  ############
  ############
  if (ASXAIC[i,6] == 4){
    temppar <- matrix(0:0, length(ASXtau), 3)
    #####
    sigmatemp <- g11sigma(ts = c(GARCH11Param[i,1], GARCH11Param[i,2], GARCH11Param[i,3], GARCH11Param[i,4], GARCH11Param[i,5]), 
                          data = ASXReturns[,c(1,i)])
    for (j in 1:length(ASXtau)){
      temppar[j,] <- optim(par = c(0.5,0.5,0.5), fn = g11CheckLoss, data = ASXReturns[,c(1,i)], 
                           tau = ASXtau[j], sigma = sigmatemp)$par
    }
    #####
    tempq <- g11quantiles(temppar, ASXReturns[,c(1,i)], sigma = sigmatemp)
    tempU <- rep(0, nrow(tempq))
    for (k in 1:length(tempU)){
      tempU[k] <- (sum(tempq[k,] <= ASXReturns[(k+2),i]) / (length(ASXtau)+1))
    }
    ASXqrU[,i] <- tempU
  }
  ############
  ############
  if (ASXAIC[i,6] == 5){
    temppar <- matrix(0:0, length(ASXtau), 4)
    #####
    for (j in 1:length(ASXtau)){
      temppar[j,] <- optim(par = c(0.1,-0.1,-0.1,0.1), fn = CAViaRLoss, Returns = ASXReturns[,c(1,i)], 
                           Tau = ASXtau[j], initq = quantile(ASXReturns[,i], probs = ASXtau[j], na.rm = TRUE))$par
    }
    #####
    tempq <- SAVQuantiles(temppar, ASXReturns[,c(1,i)])
    tempU <- rep(0, nrow(tempq))
    for (k in 1:length(tempU)){
      tempU[k] <- (sum(tempq[k,] <= ASXReturns[(k+2),i]) / (length(ASXtau)+1))
    }
    ASXqrU[,i] <- tempU 
  }
}
save.image()
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
ASXAICRankMatrix <- matrix(0:0, nrow(ASXAIC), (ncol(ASXAIC)-1))
colnames(ASXAICRankMatrix) <- colnames(ASXAIC[,-6])
rownames(ASXAICRankMatrix) <- rownames(ASXAIC)
for (i in 1:nrow(ASXAICRankMatrix)){
  for (j in 1:ncol(ASXAICRankMatrix)){
    ASXAICRankMatrix[i,j] <- sum(ASXAIC[i,1:5] <= ASXAIC[i,j])
  }
}
######################
######################
ASXBICRankMatrix <- matrix(0:0, nrow(ASXBIC), (ncol(ASXBIC)-1))
colnames(ASXBICRankMatrix) <- colnames(ASXBIC[,-6])
rownames(ASXBICRankMatrix) <- rownames(ASXBIC)
for (i in 1:nrow(ASXBICRankMatrix)){
  for (j in 1:ncol(ASXBICRankMatrix)){
    ASXBICRankMatrix[i,j] <- sum(ASXBIC[i,1:5] <= ASXBIC[i,j])
  }
}
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
library(copula)
corrvect <- rep(0, 1596)
for (i in 1:56){
    corrvect[firstseq[(i+1)]:secseq[(i+1)]] <- cor(ASXReturns[,i], ASXReturns[,(i+1):57])
}

tcopLL <- function(par){
  LL <- loglikCopula(param = par, u = ASXtimeseriesU, copula = ellipCopula(family = "t", dim = 57, dispstr = "un"))
  return(LL)
}
ASXtCopula <- optim(par = partialcorr, tcopLL, method = "CG", control = list(maxit = 5, trace = 3, fnscale = -1))
partialcorr <- ASXtCopula$par
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
library(plotrix)
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/AICBIC/ASXAIC.pdf", width = 7, height = 11)
color2D.matplot(x = ASXAICRankMatrix, 
                extremes = c("chartreuse2", "firebrick2"),
                show.legend = T, nslices = 5, border = "gray30", 
                axes = F, ylab = "Stock Code", xlab = "Model Specification", main = "AIC Rankings for Different Specifications")
axis(1, at = c(1:5-0.5), labels = c("ARMA(1,0)", "ARMA(1,1)", "GARCH(1,0)", "GARCH(1,1)", "SAV-GARCH"))
axis(2, at = c(57:1-0.5), labels = c(rownames(ASXAICRankMatrix)), las = 1)
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/AICBIC/ASXBIC.pdf", width = 7, height = 11)
color2D.matplot(x = ASXBICRankMatrix, 
                extremes = c("chartreuse2", "firebrick2"),
                show.legend = T, nslices = 5, border = "gray30", 
                axes = F, ylab = "Stock Code", xlab = "Model Specification", main = "BIC Rankings for Different Specifications")
axis(1, at = c(1:5-0.5), labels = c("ARMA(1,0)", "ARMA(1,1)", "GARCH(1,0)", "GARCH(1,1)", "SAV-GARCH"))
axis(2, at = c(57:1-0.5), labels = c(rownames(ASXAICRankMatrix)), las = 1)
dev.off()
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
ASXtCorrMatrix <- matrix(0:0, 57, 57)
for (i in 1:nrow(ASXtCorrMatrix)){
  ASXtCorrMatrix[i,i] <- 1
}
########################################
uptri <- 56:0
firstseq <- rep(0, 57)
firstseq[57] <- 1596
secseq <- rep(0,56)
secseq[57] <- 1596
for (i in (length(firstseq)-1):1){
  secseq[i] <- firstseq[(i+1)] - 1
  firstseq[i] <- secseq[i] - uptri[i]
}
#########################################
for (i in 1:(nrow(ASXtCorrMatrix)-1)){
  ASXtCorrMatrix[i,(i+1):ncol(ASXtCorrMatrix)] <- ASXtCopula$par[firstseq[(i+1)]:secseq[(i+1)]]
  ASXtCorrMatrix[(i+1):nrow(ASXtCorrMatrix),i] <- ASXtCopula$par[firstseq[(i+1)]:secseq[(i+1)]]
}
#########################################
#########################################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/AICBIC/ASXtCorr.pdf", width = 11, height = 11)
color2D.matplot(x = ASXtCorrMatrix, 
                c(-1,0),c(0,0), c(0,1),
                extremes = c("royalblue1", "coral1"),
                xlab = " ", ylab = " ",
                show.legend = T, nslices = 10, border = "gray30", 
                axes = F)
axis(3, at = c(1:57-0.5), labels = c(rownames(ASXAICRankMatrix)), las = 2)
axis(2, at = c(57:1-0.5), labels = c(rownames(ASXAICRankMatrix)), las = 1)
dev.off()
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
# SUMMARY STATISTICS
asxmean <- rep(0, 57)
asxskew <- rep(0, 57)
asxkurt <- rep(0, 57)
library(moments)
#
for (i in 1:ncol(ASXReturns)){
  asxmean[i] <- mean(ASXReturns[,i])
  asxskew[i] <- skewness(ASXReturns[,i])
  asxkurt[i] <- kurtosis(ASXReturns[,i])
}
#
TablePrint(c(
  "\\hline ASX Code", "\\hline \\hline ABC", "ALL", "ALQ", "AMC", "AMP", "ANN", "ANZ", "ASX", "AWC", "BEN", "BHP", "BKL", "BOQ", "CBA", "CCL", "CGF",
  "CIM", "COH", "CPU", "CSL", "CSR", "CTX", "DOW", "FLT", "FXJ", "GNC", "GPT", "HVN", "ILU",
  "Mean", asxmean[1:29], 
  "Skewness", asxskew[1:29],
  "Kurtosis", asxkurt[1:29], 
  "ASX Code", "IOF", "JHX", "LLC", "MGR", "MQG", "NAB", "NCM", "ORG", "ORI", "OSH", "PPT", "PRY", "QAN", "QBE", "RHC", 
  "RIO", "RMD", "SGP", "SHL",
  "STO", "SUN", "TAH", "TCL", "TLS", "WBC", "WES", "WOW", "WPL", " ", 
  "Mean", asxmean[30:57], " ",
  "Skewness", asxskew[30:57], " ",
  "Kurtosis", asxkurt[30:57], " " 
), 30, 8)
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
TablePrint(c(
"\\hline ASX Code", "\\hline \\hline ABC", "ALL", "ALQ", "AMC", "AMP", "ANN", "ANZ", "ASX", "AWC", "BEN", "BHP", "BKL", "BOQ", "CBA", "CCL", "CGF",
"CIM", "COH", "CPU", "CSL", "CSR", "CTX", "DOW", "FLT", "FXJ", "GNC", "GPT", "HVN", "ILU","IOF", "JHX", "LLC", "MGR", "MQG", "NAB", "NCM", "ORG", "ORI", "OSH", "PPT", "PRY", "QAN", "QBE", "RHC", 
"RIO", "RMD", "SGP", "SHL",
"STO", "SUN", "TAH", "TCL", "TLS", "WBC", "WES", "WOW", "WPL",
"ARMA(1,0)", ASXAIC[,1],
"ARMA(1,1)", ASXAIC[,2],
"ARMA(1,0)GARCH(1,0)", ASXAIC[,3],
"ARMA(1,0)GARCH(1,1)", ASXAIC[,4],
"SAV-GARCH", ASXAIC[,5]
), 58, 6)
TablePrint(c(
  "\\hline ASX Code", "\\hline \\hline ABC", "ALL", "ALQ", "AMC", "AMP", "ANN", "ANZ", "ASX", "AWC", "BEN", "BHP", "BKL", "BOQ", "CBA", "CCL", "CGF",
  "CIM", "COH", "CPU", "CSL", "CSR", "CTX", "DOW", "FLT", "FXJ", "GNC", "GPT", "HVN", "ILU","IOF", "JHX", "LLC", "MGR", "MQG", "NAB", "NCM", "ORG", "ORI", "OSH", "PPT", "PRY", "QAN", "QBE", "RHC", 
  "RIO", "RMD", "SGP", "SHL",
  "STO", "SUN", "TAH", "TCL", "TLS", "WBC", "WES", "WOW", "WPL",
  "ARMA(1,0)", ASXBIC[,1],
  "ARMA(1,1)", ASXBIC[,2],
  "ARMA(1,0)GARCH(1,0)", ASXBIC[,3],
  "ARMA(1,0)GARCH(1,1)", ASXBIC[,4],
  "SAV-GARCH", ASXBIC[,5]
), 58, 6)















































