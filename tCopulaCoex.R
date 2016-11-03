################################################################################################################################
################################################################################################################################
################## STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - ASX 100 t-Copula COEXCEEDANCES #################
################################################################################################################################
################################################################################################################################
friendlyCop <- ellipCopula(family = "t", param = ASXtCopula$par[1:1596],
                           dim = 57, dispstr = "un", df = round(ASXtCopula$par[1597],0))
library(mvtnorm)

ASXtCopMSE <- function(PIT, CX1, CX2, tau) {
  Coex <- matrix(0:0, nrow(PIT), 5)
  rownames(Coex) <- rownames(PIT)

  #
  for (i in 1:nrow(PIT)) {
    if (PIT[i, CX2] < tau) {
      Xt1 <- rep(0, ncol(PIT))
      Xt2 <- rep(0, ncol(PIT))
      Xt1 <- PIT[i,]
      Xt2 <- PIT[i,]
      Xt1 <- qt(Xt1, df = ASXtCopula$par[1597])
      Xt2 <- qt(Xt2, df = ASXtCopula$par[1597])
      Xt1[CX1] <- qt(tau,  df = ASXtCopula$par[1597])
      Xt1[CX2] <- qt(tau,  df = ASXtCopula$par[1597])
      Xt2[CX2] <- qt(tau,  df = ASXtCopula$par[1597])
      Xt2 <- Xt2[-CX1]
      Temp1 <- pmvt(lower = rep(-Inf,57), upper = Xt1, df = round(ASXtCopula$par[1597],0), corr = ASXtCorrMatrix, abseps =0.005)
      Temp2 <- pmvt(lower = rep(-Inf,56), upper = Xt2, df = round(ASXtCopula$par[1597],0), corr = ASXtCorrMatrix[-CX1,-CX1], abseps =0.005)
      Coex[i, 2] <- (Temp1 / Temp2)
      if (Coex[i,2] > 1) Coex[i,2] <- 1
      Coex[i, 3] <- as.numeric((PIT[i, CX1] < tau) & (PIT[i, CX2] < tau))
    }
  }
  Coex[, 4] <- ((Coex[, 2] - Coex[, 3]) ^ 2)
  Coex[1, 5] <- sum(Coex[, 4]) / sum((PIT[, CX2 ] < tau))
  #
  return(Coex)
}
####################
####################
####################
####################
BHPCBA.t.05.MSE <- ASXtCopMSE(PIT = ASXtimeseriesU, CX1 = 11, CX2 = 14, tau = 0.05)
#
ANZCBA.t.05.MSE <- ASXtCopMSE(PIT = ASXtimeseriesU, CX1 = 7, CX2 = 14, tau = 0.05)
#
CPUCBA.t.05.MSE <- ASXtCopMSE(PIT = ASXtimeseriesU, CX1 = 19, CX2 = 14, tau = 0.05)
####################
####################
####################
####################
BHPCBA.t.10.MSE <- ASXtCopMSE(PIT = ASXtimeseriesU, CX1 = 11, CX2 = 14, tau = 0.10)
#
ANZCBA.t.10.MSE <- ASXtCopMSE(PIT = ASXtimeseriesU, CX1 = 7, CX2 = 14, tau = 0.10)
#
CPUCBA.t.10.MSE <- ASXtCopMSE(PIT = ASXtimeseriesU, CX1 = 19, CX2 = 14, tau = 0.10)


















































