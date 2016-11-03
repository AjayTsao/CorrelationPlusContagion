################################################################################################################################
################################################################################################################################
################ STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - ASX 100 LOGIT MLE AND COEXCEEDANCES ##############
################################################################################################################################
################################################################################################################################
ASXLogitMLE <- function(Beta, Y, X){
  FXB <- rep(0, nrow(X))
  LL <- rep(0, nrow(X))
  onesones <- matrix(1:1, nrow(X), 1)
  
  # Ugly as sin but significant speed boost
    FXB <- exp(Beta[1] * X[,1] + Beta[2] * X[,2] + Beta[3] * X[,3] + Beta[4] * X[,4] + Beta[5] * X[,5] +
                     Beta[6] * X[,6] + Beta[7] * X[,7] + Beta[8] * X[,8] + Beta[9] * X[,9] + Beta[10] * X[,10] +
                     Beta[11] * X[,11] + Beta[12] * X[,12] + Beta[13] * X[,13] + Beta[14] * X[,14] + Beta[15] * X[,15] + 
                     Beta[16] * X[,16] +
                     Beta[17] * X[,17] + Beta[18] * X[,18] + Beta[19] * X[,19] + Beta[20] * X[,20] + Beta[21] * X[,21] + 
                 Beta[22] * X[,22] +  Beta[23] * X[,23] + Beta[24] * X[,24] + Beta[25] * X[,25] + Beta[26] * X[,26] + 
                 Beta[27] * X[,27] + 
                 Beta[28] * X[,28] + Beta[29] * X[,29] + Beta[30] * X[,30] + Beta[31] * X[,31] + 
                 Beta[32] * X[,32] + Beta[33] * X[,33] + 
                 Beta[34] * X[,34] + Beta[35] * X[,35] + 
                 Beta[36] * X[,36] + Beta[37] * X[,37] + Beta[38] * X[,38] + Beta[39] * X[,39] + Beta[40] * X[,40] + 
                 Beta[41] * X[,41] + Beta[42] * X[,42] + Beta[43] * X[,43] + Beta[44] * X[,44] +
                 Beta[45] * X[,45] + Beta[46] * X[,46] + Beta[47] * X[,47] + Beta[48] * X[,48] +
                 Beta[49] * X[,49] + Beta[50] * X[,50] + Beta[51] * X[,51] + Beta[52] * X[,52] +
                 Beta[53] * X[,53] + Beta[54] * X[,54] + Beta[55] * X[,55] + Beta[56] * onesones[,1]) /
      (1 + exp(Beta[1] * X[,1] + Beta[2] * X[,2] + Beta[3] * X[,3] + Beta[4] * X[,4] + Beta[5] * X[,5] +
                 Beta[6] * X[,6] + Beta[7] * X[,7] + Beta[8] * X[,8] + Beta[9] * X[,9] + Beta[10] * X[,10] +
                 Beta[11] * X[,11] + Beta[12] * X[,12] + Beta[13] * X[,13] + Beta[14] * X[,14] + Beta[15] * X[,15] + 
                 Beta[16] * X[,16] +
                 Beta[17] * X[,17] + Beta[18] * X[,18] + Beta[19] * X[,19] + Beta[20] * X[,20] + Beta[21] * X[,21] + 
                 Beta[22] * X[,22] +  Beta[23] * X[,23] + Beta[24] * X[,24] + Beta[25] * X[,25] + Beta[26] * X[,26] + 
                 Beta[27] * X[,27] + 
                 Beta[28] * X[,28] + Beta[29] * X[,29] + Beta[30] * X[,30] + Beta[31] * X[,31] + 
                 Beta[32] * X[,32] + Beta[33] * X[,33] + 
                 Beta[34] * X[,34] + Beta[35] * X[,35] + 
                 Beta[36] * X[,36] + Beta[37] * X[,37] + Beta[38] * X[,38] + Beta[39] * X[,39] + Beta[40] * X[,40] + 
                 Beta[41] * X[,41] + Beta[42] * X[,42] + Beta[43] * X[,43] + Beta[44] * X[,44] +
                 Beta[45] * X[,45] + Beta[46] * X[,46] + Beta[47] * X[,47] + Beta[48] * X[,48] +
                 Beta[49] * X[,49] + Beta[50] * X[,50] + Beta[51] * X[,51] + Beta[52] * X[,52] +
                 Beta[53] * X[,53] + Beta[54] * X[,54] + Beta[55] * X[,55] + Beta[56] * onesones[,1]))

  LL <- (Y * log(FXB)) + ((1 - Y) * log(1 - FXB))
  #
  retLL <- sum(LL)
  #
  return(retLL)
}
####################
####################
####################
####################
ASXLogitOptim <- function(PIT, CX1, CX2, tau){
  Y1 <- as.numeric((PIT[,CX1] < tau) & (PIT[,CX2] < tau))
  Y2 <- as.numeric((PIT[,CX2] < tau))
  #
  X <- matrix(0:0, nrow(PIT), (ncol(ASXqrU)-2))
  #
  X <- ASXqrU[,c(-CX1, -CX2)]
  #
  EstimJ <- optim(par = rep(-1.5,56), fn = ASXLogitMLE, Y = Y1, X = X, control = list(maxit = 350000, fnscale = (-1)), hessian = T)
  EstimM <- optim(par = rep(-1,56), fn = ASXLogitMLE, Y = Y2, X = X, control = list(maxit = 350000, fnscale = (-1)), hessian = T)
  #
  return(list(Joint = EstimJ, Marg = EstimM))
}
####################
####################
####################
####################
ASXLogitMSE <- function(PIT, BetaJ, BetaM, CX1, CX2, tau){
    Coex <- matrix(0:0, nrow(PIT), 5)
    rownames(Coex) <- rownames(PIT)
    #
    for (i in 1:nrow(PIT)) {
      if (PIT[i, CX2] < tau) {
        Temp1 <- exp(sum(BetaJ * PIT[i, c(-CX1,-CX2)])) /
          (1 + exp(sum(BetaJ * PIT[i, c(-CX1,-CX2)])))
        Temp2 <- exp(sum(BetaM * PIT[i, c(-CX1,-CX2)])) /
          (1 + exp(sum(BetaM * PIT[i, c(-CX1,-CX2)])))
        Coex[i, 2] <- (Temp1 / Temp2)
        if (Coex[i,2] > 1) Coex[i,2] <- 1
        Coex[i, 3] <- as.numeric((PIT[i, CX1] < tau) & (PIT[i, CX2] < tau))
      }
    }
    Coex[, 4] <- ((Coex[, 2] - Coex[, 3]) ^ 2)
    Coex[1, 5] <- sum(Coex[, 4]) / sum((PIT[, CX2] < tau))
    #
    return(Coex)
}
####################
####################
####################
####################
# BHPCBA.Logit.05.Est <- ASXLogitOptim(PIT = ASXqrU, CX1 = 11, CX2 = 14, tau = 0.05)
# # # Mining
# ANZCBA.Logit.05.Est <- ASXLogitOptim(PIT = ASXqrU, CX1 = 7, CX2 = 14, tau = 0.05)
# # Financials
# CPUCBA.Logit.05.Est <- ASXLogitOptim(PIT = ASXqrU, CX1 = 19, CX2 = 14, tau = 0.05)
# # IT
# ####################
####################
BHPCBA.Logit.05.MSE <- ASXLogitMSE(PIT = ASXtimeseriesU, 
                                   BetaJ = BHPCBA.Logit.05.Est$Joint$par,
                                   BetaM = BHPCBA.Logit.05.Est$Marg$par,
                                   CX1 = 11, CX2 = 14, tau = 0.05)
#
ANZCBA.Logit.05.MSE <- ASXLogitMSE(PIT = ASXtimeseriesU,
                                   BetaJ = ANZCBA.Logit.05.Est$Joint$par,
                                   BetaM = ANZCBA.Logit.05.Est$Marg$par,
                                   CX1 = 7, CX2 = 14, tau = 0.05)
#
CPUCBA.Logit.05.MSE <- ASXLogitMSE(PIT = ASXtimeseriesU, 
                                   BetaJ = CPUCBA.Logit.05.Est$Joint$par,
                                   BetaM = CPUCBA.Logit.05.Est$Marg$par,
                                   CX1 = 19, CX2 = 14, tau = 0.05)
####################
####################
####################
####################
# BHPCBA.Logit.10.Est <- ASXLogitOptim(PIT = ASXqrU, CX1 = 11, CX2 = 14, tau = 0.10)
# # Mining
# ANZCBA.Logit.10.Est <- ASXLogitOptim(PIT = ASXqrU, CX1 = 7, CX2 = 14, tau = 0.10)
# # Financials
# CPUCBA.Logit.10.Est <- ASXLogitOptim(PIT = ASXqrU, CX1 = 19, CX2 = 14, tau = 0.10)
# # IT
# ####################
####################
BHPCBA.Logit.10.MSE <- ASXLogitMSE(PIT = ASXtimeseriesU, 
                                   BetaJ = BHPCBA.Logit.10.Est$Joint$par,
                                   BetaM = BHPCBA.Logit.10.Est$Marg$par,
                                   CX1 = 11, CX2 = 14, tau = 0.10)
#
ANZCBA.Logit.10.MSE <- ASXLogitMSE(PIT = ASXtimeseriesU, 
                                   BetaJ = ANZCBA.Logit.10.Est$Joint$par,
                                   BetaM = ANZCBA.Logit.10.Est$Marg$par,
                                   CX1 = 7, CX2 = 14, tau = 0.10)
#
CPUCBA.Logit.10.MSE <- ASXLogitMSE(PIT = ASXtimeseriesU, 
                                   BetaJ = CPUCBA.Logit.10.Est$Joint$par,
                                   BetaM = CPUCBA.Logit.10.Est$Marg$par,
                                   CX1 = 19, CX2 = 14, tau = 0.10)
####################
####################
TablePrint(c(
  "\\hline \\textbf{MSE of Prediction}", "\\textbf{of Coexceedances}", "\\hline BHP-CBA", " ",
  "\\hline ANZ-CBA", " ", "\\hline CPU-CBA", " ",
  " ", " ", "Logit", "Student-t", "Logit", "Student-t", "Logit", "Student-t",
  "$\\tau = 0.05$", " ", BHPCBA.Logit.05.MSE[1,5], BHPCBA.t.05.MSE[1,5], 
  ANZCBA.Logit.05.MSE[1,5], ANZCBA.t.05.MSE[1,5], 
  CPUCBA.Logit.05.MSE[1,5], CPUCBA.t.05.MSE[1,5],
  "$\\tau = 0.10$", " ", BHPCBA.Logit.10.MSE[1,5], BHPCBA.t.10.MSE[1,5], 
  ANZCBA.Logit.10.MSE[1,5], ANZCBA.t.10.MSE[1,5], 
  CPUCBA.Logit.10.MSE[1,5], CPUCBA.t.10.MSE[1,5]
), 8, 4)
####################
####################
ANZCBACovVarJ <- solve(-ANZCBA.Logit.05.Est$Joint$hessian)
ANZCBASEJ <- rep(0, nrow(ANZCBA.Logit.05.Est$Joint$hessian))
for (i in 1:length(ANZCBASEJ)){
  ANZCBASEJ[i] <- ANZCBACovVarJ[i,i] ^ 0.5
}
####################
ANZCBACovVarM <- solve(-ANZCBA.Logit.05.Est$Marg$hessian)
ANZCBASEM <- rep(0, nrow(ANZCBA.Logit.05.Est$Marg$hessian))
for (i in 1:length(ANZCBASEM)){
  ANZCBASEM[i] <- ANZCBACovVarM[i,i] ^ 0.5
}
####################
####################
TablePrint(c(
  "\\hline ASX Code", "\\hline \\hline Intercept", "ABC", "ALL", "ALQ", "AMC", "AMP", "ANN", "ASX", "AWC", "BEN", "BHP", "BKL", "BOQ",  "CCL", "CGF",
  "CIM", "COH", "CPU", "CSL", "CSR", "CTX", "DOW", "FLT", "FXJ", "GNC", "GPT", "HVN", "ILU",
  "$\\hat{\\nu}$", ANZCBA.Logit.05.Est$Joint$par[56], ANZCBA.Logit.05.Est$Joint$par[1:27],
  " ",ANZCBASEJ[56], ANZCBASEJ[1:27],
  "ASX Code", "IOF", "JHX", "LLC", "MGR", "MQG", "NAB", "NCM", "ORG", "ORI", "OSH", "PPT", "PRY", "QAN", "QBE", "RHC", 
  "RIO", "RMD", "SGP", "SHL",
  "STO", "SUN", "TAH", "TCL", "TLS", "WBC", "WES", "WOW", "WPL",
  "$\\hat{\\nu}$", ANZCBA.Logit.05.Est$Joint$par[28:55],
  " ", ANZCBASEJ[28:55]
), 29, 6)
####################
####################
TablePrint(c(
  "\\hline ASX Code", "\\hline \\hline Intercept", "ABC", "ALL", "ALQ", "AMC", "AMP", "ANN", "ASX", "AWC", "BEN", "BHP", "BKL", "BOQ",  "CCL", "CGF",
  "CIM", "COH", "CPU", "CSL", "CSR", "CTX", "DOW", "FLT", "FXJ", "GNC", "GPT", "HVN", "ILU",
  "$\\hat{\\nu}$", ANZCBA.Logit.05.Est$Marg$par[1:28],
  " ", ANZCBASEM[1:28],
  "ASX Code", "IOF", "JHX", "LLC", "MGR", "MQG", "NAB", "NCM", "ORG", "ORI", "OSH", "PPT", "PRY", "QAN", "QBE", "RHC", 
  "RIO", "RMD", "SGP", "SHL",
  "STO", "SUN", "TAH", "TCL", "TLS", "WBC", "WES", "WOW", "WPL",
  "$\\hat{\\nu}$", ANZCBA.Logit.05.Est$Marg$par[29:56],
  " ", ANZCBASEM[29:56]
), 29, 6)

































































