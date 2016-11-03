###############################################################################################################################
###############################################################################################################################
################## STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - CONDITIONAL CAVIAR REGRESSION #################
###############################################################################################################################
###############################################################################################################################
ConditionalEmpiricalMSE <- function(PIT, CX1, CX2, Cond1, Cond2, tau){
  Coex <- matrix(0:0, nrow(PIT), 5)
  Coex[,1] <- PIT[,1]
  # 
  Emp1 <- as.numeric((PIT[,(CX1+1)] < tau) & (PIT[,(CX2+1)] < tau)) 
  Emp2 <- as.numeric((PIT[,(CX2+1)] < tau)) 
  #
  H1 <- bw.nrd0(PIT[,(Cond1+1)])
  H2 <- bw.nrd0(PIT[,(Cond2+1)])
  #
  for (i in 1:nrow(PIT)){
    if (PIT[i,(CX2+1)] < tau){
      Temp1 <- sum(Emp1 * sqrt(2 * pi) * exp((-1/2) * ((PIT[,(Cond1+1)] - PIT[i,(Cond1+1)]) / H1)^2) * sqrt(2 * pi) * exp((-1/2) * ((PIT[,(Cond2+1)] - PIT[i,(Cond2+1)])/ H2)^2))
      Temp2 <- sum(Emp2 * sqrt(2 * pi) * exp((-1/2) * ((PIT[,(Cond1+1)] - PIT[i,(Cond1+1)]) / H1)^2) * sqrt(2 * pi) * exp((-1/2) * ((PIT[,(Cond2+1)] - PIT[i,(Cond2+1)]) / H2)^2))
      Coex[i,2] <- (Temp1 / Temp2)
      Coex[i,3] <- as.numeric((PIT[i,(CX1+1)] < tau) & (PIT[i,(CX2+1)] < tau))
    }
  }
  Coex[,4] <- (Coex[,2] - Coex[,3]) ^ 2
  Coex[1,5] <- sum(Coex[,4]) / sum((PIT[,(CX2+1)] < tau))
  #
  return(Coex)
}
#################################################
#################################################
#################################################
#################################################
BrazilChile.CondEmp.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 1, CX2 = 2, Cond1 = 3, Cond2 = 4, tau = 0.01)
#
BrazilArgentina.CondEmp.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 1, CX2 = 3, Cond1 = 2, Cond2 = 4, tau = 0.01)
#
BrazilMexico.CondEmp.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 1, CX2 = 4, Cond1 = 2, Cond2 = 3, tau = 0.01)
#
ChileArgentina.CondEmp.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 2, CX2 = 3, Cond1 = 1, Cond2 = 4, tau = 0.01)
#
ChileMexico.CondEmp.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 2, CX2 = 4, Cond1 = 1, Cond2 = 3, tau = 0.01)
#
ArgentinaMexico.CondEmp.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 3, CX2 = 4, Cond1 = 1, Cond2 = 2, tau = 0.01)
#################################################
#################################################
BrazilChile.CondEmp.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 1, CX2 = 2, Cond1 = 3, Cond2 = 4, tau = 0.05)
#
BrazilArgentina.CondEmp.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 1, CX2 = 3, Cond1 = 2, Cond2 = 4, tau = 0.05)
#
BrazilMexico.CondEmp.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 1, CX2 = 4, Cond1 = 2, Cond2 = 3, tau = 0.05)
#
ChileArgentina.CondEmp.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 2, CX2 = 3, Cond1 = 1, Cond2 = 4, tau = 0.05)
#
ChileMexico.CondEmp.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 2, CX2 = 4, Cond1 = 1, Cond2 = 3, tau = 0.05)
#
ArgentinaMexico.CondEmp.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 3, CX2 = 4, Cond1 = 1, Cond2 = 2, tau = 0.05)
#################################################
#################################################
BrazilChile.CondEmp.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 1, CX2 = 2, Cond1 = 3, Cond2 = 4, tau = 0.10)
#
BrazilArgentina.CondEmp.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 1, CX2 = 3, Cond1 = 2, Cond2 = 4, tau = 0.10)
#
BrazilMexico.CondEmp.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 1, CX2 = 4, Cond1 = 2, Cond2 = 3, tau = 0.10)
#
ChileArgentina.CondEmp.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 2, CX2 = 3, Cond1 = 1, Cond2 = 4, tau = 0.10)
#
ChileMexico.CondEmp.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 2, CX2 = 4, Cond1 = 1, Cond2 = 3, tau = 0.10)
#
ArgentinaMexico.CondEmp.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.CAViaRU, CX1 = 3, CX2 = 4, Cond1 = 1, Cond2 = 2, tau = 0.10)
#################################################
#################################################










































































