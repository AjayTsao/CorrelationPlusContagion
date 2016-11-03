###############################################################################################################################
###############################################################################################################################
################## STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - CONDITIONAL CAVIAR REGRESSION #################
###############################################################################################################################
###############################################################################################################################
All.CAViaRU <- merge(BrazilChile.caviarU[-1,], ArgentinaMexico.caviarU[-1,], by = 1)
####################
LogitMLE <- function(Beta, Y, X){
  FXB <- rep(0, nrow(X))
  LL <- rep(0, nrow(X))

    FXB <- exp(Beta[1] + Beta[2] * X[,2] + Beta[3] * X[,3]) / 
      (1 + exp(Beta[1]  + Beta[2] * X[,2] + Beta[3] * X[,3]))
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
CAViaRConditionalRegression <- function(PIT, CX1, CX2, Cond1, Cond2, tau){
  Y1 <- as.numeric((PIT[,(CX1+1)] < tau) & (PIT[,(CX2+1)] < tau))
  Y2 <- as.numeric((PIT[,(CX2+1)] < tau))
  #
  X <- matrix(0:0, nrow(PIT), 3)
  #
  X[,1] <- rep(1, nrow(PIT))
  X[,2] <- PIT[,(Cond1 + 1)]
  X[,3] <- PIT[,(Cond2 + 1)]
  #
  EstimJ <- optim(par = c(10,-10,-10), fn = LogitMLE, Y = Y1, X = X, control = list(maxit = 2500, fnscale = (-1)), hessian = T)
  EstimM <- optim(par = c(10,-10,-10), fn = LogitMLE, Y = Y2, X = X, control = list(maxit = 2500, fnscale = (-1)), hessian = T)
  #
  return(list(Joint = EstimJ, Marg = EstimM))
}
####################
####################
####################
####################
ConditionalEmpiricalMSE <- function(PIT, BetaJ, BetaM, CX1, CX2, Cond1, Cond2, tau){
  Coex <- matrix(0:0, nrow(PIT), 5)
  Coex[,1] <- PIT[,1]
  #
  for (i in 1:nrow(PIT)){
    if (PIT[i,(CX2+1)] < tau){
      Temp1 <- exp(BetaJ[1] + (BetaJ[2] * PIT[i,(Cond1+1)]) + (BetaJ[3] * PIT[i,(Cond2+1)])) /
        (1 + exp(BetaJ[1] + (BetaJ[2] * PIT[i,(Cond1+1)]) + (BetaJ[3] * PIT[i,(Cond2+1)])))
      Temp2 <- exp(BetaM[1] + (BetaM[2] * PIT[i,(Cond1+1)]) + (BetaM[3] * PIT[i,(Cond2+1)])) /
        (1 + exp(BetaM[1] + (BetaM[2] * PIT[i,(Cond1+1)]) + (BetaM[3] * PIT[i,(Cond2+1)]))) 
      Coex[i,2] <- (Temp1 / Temp2)
      Coex[i,3] <- as.numeric((PIT[i,(CX1+1)] < tau) & (PIT[i,(CX2+1)] < tau))
    }
  }
  Coex[,4] <- ((Coex[,2] - Coex[,3]) ^ 2) 
  Coex[1,5] <- sum(Coex[,4]) / sum((PIT[,(CX2+1)] < tau))
  #
  return(Coex)
}
##########
ConditionalEmpiricalMSEAllDates <- function(PIT, BetaJ, BetaM, CX1, CX2, Cond1, Cond2, tau){
  Coex <- matrix(0:0, nrow(PIT), 5)
  Coex[,1] <- PIT[,1]
  #
  for (i in 1:nrow(PIT)){

      Temp1 <- exp(BetaJ[1] + (BetaJ[2] * PIT[i,(Cond1+1)]) + (BetaJ[3] * PIT[i,(Cond2+1)])) /
        (1 + exp(BetaJ[1] + (BetaJ[2] * PIT[i,(Cond1+1)]) + (BetaJ[3] * PIT[i,(Cond2+1)])))
      Temp2 <- exp(BetaM[1] + (BetaM[2] * PIT[i,(Cond1+1)]) + (BetaM[3] * PIT[i,(Cond2+1)])) /
        (1 + exp(BetaM[1] + (BetaM[2] * PIT[i,(Cond1+1)]) + (BetaM[3] * PIT[i,(Cond2+1)]))) 
      Coex[i,2] <- (Temp1 / Temp2)
      Coex[i,3] <- as.numeric((PIT[i,(CX1+1)] < tau) & (PIT[i,(CX2+1)] < tau))
    
  }
  Coex[,4] <- ((Coex[,2] - Coex[,3]) ^ 2) 
  Coex[1,5] <- sum(Coex[,4]) / sum((PIT[,(CX2+1)] < tau))
  #
  return(Coex)
}
####################
####################
####################
####################
BrazilChile.CondCAViaR.01.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 1, CX2 = 2, Cond1 = 3, Cond2 = 4, tau = 0.01)
#
BrazilArgentina.CondCAViaR.01.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 1, CX2 = 3, Cond1 = 2, Cond2 = 4, tau = 0.01)
#
BrazilMexico.CondCAViaR.01.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 1, CX2 = 4, Cond1 = 2, Cond2 = 3, tau = 0.01)
#
ChileArgentina.CondCAViaR.01.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 2, CX2 = 3, Cond1 = 1, Cond2 = 4, tau = 0.01)
#
ChileMexico.CondCAViaR.01.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 2, CX2 = 4, Cond1 = 1, Cond2 = 3, tau = 0.01)
#
ArgentinaMexico.CondCAViaR.01.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 3, CX2 = 4, Cond1 = 1, Cond2 = 2, tau = 0.01)
####################
####################
BrazilChile.CondCAViaR.05.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 1, CX2 = 2, Cond1 = 3, Cond2 = 4, tau = 0.05)
#
BrazilArgentina.CondCAViaR.05.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 1, CX2 = 3, Cond1 = 2, Cond2 = 4, tau = 0.05)
#
BrazilMexico.CondCAViaR.05.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 1, CX2 = 4, Cond1 = 2, Cond2 = 3, tau = 0.05)
#
ChileArgentina.CondCAViaR.05.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 2, CX2 = 3, Cond1 = 1, Cond2 = 4, tau = 0.05)
#
ChileMexico.CondCAViaR.05.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 2, CX2 = 4, Cond1 = 1, Cond2 = 3, tau = 0.05)
#
ArgentinaMexico.CondCAViaR.05.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 3, CX2 = 4, Cond1 = 1, Cond2 = 2, tau = 0.05)
####################
####################
BrazilChile.CondCAViaR.10.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 1, CX2 = 2, Cond1 = 3, Cond2 = 4, tau = 0.10)
#
BrazilArgentina.CondCAViaR.10.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 1, CX2 = 3, Cond1 = 2, Cond2 = 4, tau = 0.10)
#
BrazilMexico.CondCAViaR.10.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 1, CX2 = 4, Cond1 = 2, Cond2 = 3, tau = 0.10)
#
ChileArgentina.CondCAViaR.10.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 2, CX2 = 3, Cond1 = 1, Cond2 = 4, tau = 0.10)
#
ChileMexico.CondCAViaR.10.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 2, CX2 = 4, Cond1 = 1, Cond2 = 3, tau = 0.10)
#
ArgentinaMexico.CondCAViaR.10.Est <- CAViaRConditionalRegression(PIT = All.CAViaRU, CX1 = 3, CX2 = 4, Cond1 = 1, Cond2 = 2, tau = 0.10)
####################
####################
####################
####################
BrazilChile.CondCAViaR.01.EstSE <- matrix(0:0, 3, 2)
BrazilChile.CC.01.JVC <- solve(-BrazilChile.CondCAViaR.01.Est$Joint$hessian)
BrazilChile.CC.01.MVC <- solve(-BrazilChile.CondCAViaR.01.Est$Marg$hessian)
for (i in 1:3){
  BrazilChile.CondCAViaR.01.EstSE[i,1] <- sqrt(BrazilChile.CC.01.JVC[i,i])
  BrazilChile.CondCAViaR.01.EstSE[i,2] <- sqrt(BrazilChile.CC.01.MVC[i,i])
}
#
BrazilArgentina.CondCAViaR.01.EstSE <- matrix(0:0, 3, 2)
BrazilArgentina.CC.01.JVC <- solve(-BrazilArgentina.CondCAViaR.01.Est$Joint$hessian)
BrazilArgentina.CC.01.MVC <- solve(-BrazilArgentina.CondCAViaR.01.Est$Marg$hessian)
for (i in 1:3){
  BrazilArgentina.CondCAViaR.01.EstSE[i,1] <- sqrt(BrazilArgentina.CC.01.JVC[i,i])
  BrazilArgentina.CondCAViaR.01.EstSE[i,2] <- sqrt(BrazilArgentina.CC.01.MVC[i,i])
}
#
BrazilMexico.CondCAViaR.01.EstSE <- matrix(0:0, 3, 2)
BrazilMexico.CC.01.JVC <- solve(-BrazilMexico.CondCAViaR.01.Est$Joint$hessian)
BrazilMexico.CC.01.MVC <- solve(-BrazilMexico.CondCAViaR.01.Est$Marg$hessian)
for (i in 1:3){
  BrazilMexico.CondCAViaR.01.EstSE[i,1] <- sqrt(BrazilMexico.CC.01.JVC[i,i])
  BrazilMexico.CondCAViaR.01.EstSE[i,2] <- sqrt(BrazilMexico.CC.01.MVC[i,i])
}
#
ChileArgentina.CondCAViaR.01.EstSE <- matrix(0:0, 3, 2)
ChileArgentina.CC.01.JVC <- solve(-ChileArgentina.CondCAViaR.01.Est$Joint$hessian)
ChileArgentina.CC.01.MVC <- solve(-ChileArgentina.CondCAViaR.01.Est$Marg$hessian)
for (i in 1:3){
  ChileArgentina.CondCAViaR.01.EstSE[i,1] <- sqrt(ChileArgentina.CC.01.JVC[i,i])
  ChileArgentina.CondCAViaR.01.EstSE[i,2] <- sqrt(ChileArgentina.CC.01.MVC[i,i])
}
#
ChileMexico.CondCAViaR.01.EstSE <- matrix(0:0, 3, 2)
ChileMexico.CC.01.JVC <- solve(-ChileMexico.CondCAViaR.01.Est$Joint$hessian)
ChileMexico.CC.01.MVC <- solve(-ChileMexico.CondCAViaR.01.Est$Marg$hessian)
for (i in 1:3){
  ChileMexico.CondCAViaR.01.EstSE[i,1] <- sqrt(ChileMexico.CC.01.JVC[i,i])
  ChileMexico.CondCAViaR.01.EstSE[i,2] <- sqrt(ChileMexico.CC.01.MVC[i,i])
}
#
ArgentinaMexico.CondCAViaR.01.EstSE <- matrix(0:0, 3, 2)
ArgentinaMexico.CC.01.JVC <- solve(-ArgentinaMexico.CondCAViaR.01.Est$Joint$hessian)
ArgentinaMexico.CC.01.MVC <- solve(-ArgentinaMexico.CondCAViaR.01.Est$Marg$hessian)
for (i in 1:3){
  ArgentinaMexico.CondCAViaR.01.EstSE[i,1] <- sqrt(ArgentinaMexico.CC.01.JVC[i,i])
  ArgentinaMexico.CondCAViaR.01.EstSE[i,2] <- sqrt(ArgentinaMexico.CC.01.MVC[i,i])
}
####################
####################
BrazilChile.CondCAViaR.05.EstSE <- matrix(0:0, 3, 2)
BrazilChile.CC.05.JVC <- solve(-BrazilChile.CondCAViaR.05.Est$Joint$hessian)
BrazilChile.CC.05.MVC <- solve(-BrazilChile.CondCAViaR.05.Est$Marg$hessian)
for (i in 1:3){
  BrazilChile.CondCAViaR.05.EstSE[i,1] <- sqrt(BrazilChile.CC.05.JVC[i,i])
  BrazilChile.CondCAViaR.05.EstSE[i,2] <- sqrt(BrazilChile.CC.05.MVC[i,i])
}
#
BrazilArgentina.CondCAViaR.05.EstSE <- matrix(0:0, 3, 2)
BrazilArgentina.CC.05.JVC <- solve(-BrazilArgentina.CondCAViaR.05.Est$Joint$hessian)
BrazilArgentina.CC.05.MVC <- solve(-BrazilArgentina.CondCAViaR.05.Est$Marg$hessian)
for (i in 1:3){
  BrazilArgentina.CondCAViaR.05.EstSE[i,1] <- sqrt(BrazilArgentina.CC.05.JVC[i,i])
  BrazilArgentina.CondCAViaR.05.EstSE[i,2] <- sqrt(BrazilArgentina.CC.05.MVC[i,i])
}
#
BrazilMexico.CondCAViaR.05.EstSE <- matrix(0:0, 3, 2)
BrazilMexico.CC.05.JVC <- solve(-BrazilMexico.CondCAViaR.05.Est$Joint$hessian)
BrazilMexico.CC.05.MVC <- solve(-BrazilMexico.CondCAViaR.05.Est$Marg$hessian)
for (i in 1:3){
  BrazilMexico.CondCAViaR.05.EstSE[i,1] <- sqrt(BrazilMexico.CC.05.JVC[i,i])
  BrazilMexico.CondCAViaR.05.EstSE[i,2] <- sqrt(BrazilMexico.CC.05.MVC[i,i])
}
#
ChileArgentina.CondCAViaR.05.EstSE <- matrix(0:0, 3, 2)
ChileArgentina.CC.05.JVC <- solve(-ChileArgentina.CondCAViaR.05.Est$Joint$hessian)
ChileArgentina.CC.05.MVC <- solve(-ChileArgentina.CondCAViaR.05.Est$Marg$hessian)
for (i in 1:3){
  ChileArgentina.CondCAViaR.05.EstSE[i,1] <- sqrt(ChileArgentina.CC.05.JVC[i,i])
  ChileArgentina.CondCAViaR.05.EstSE[i,2] <- sqrt(ChileArgentina.CC.05.MVC[i,i])
}
#
ChileMexico.CondCAViaR.05.EstSE <- matrix(0:0, 3, 2)
ChileMexico.CC.05.JVC <- solve(-ChileMexico.CondCAViaR.05.Est$Joint$hessian)
ChileMexico.CC.05.MVC <- solve(-ChileMexico.CondCAViaR.05.Est$Marg$hessian)
for (i in 1:3){
  ChileMexico.CondCAViaR.05.EstSE[i,1] <- sqrt(ChileMexico.CC.05.JVC[i,i])
  ChileMexico.CondCAViaR.05.EstSE[i,2] <- sqrt(ChileMexico.CC.05.MVC[i,i])
}
#
ArgentinaMexico.CondCAViaR.05.EstSE <- matrix(0:0, 3, 2)
ArgentinaMexico.CC.05.JVC <- solve(-ArgentinaMexico.CondCAViaR.05.Est$Joint$hessian)
ArgentinaMexico.CC.05.MVC <- solve(-ArgentinaMexico.CondCAViaR.05.Est$Marg$hessian)
for (i in 1:3){
  ArgentinaMexico.CondCAViaR.05.EstSE[i,1] <- sqrt(ArgentinaMexico.CC.05.JVC[i,i])
  ArgentinaMexico.CondCAViaR.05.EstSE[i,2] <- sqrt(ArgentinaMexico.CC.05.MVC[i,i])
}
####################
####################
BrazilChile.CondCAViaR.10.EstSE <- matrix(0:0, 3, 2)
BrazilChile.CC.10.JVC <- solve(-BrazilChile.CondCAViaR.10.Est$Joint$hessian)
BrazilChile.CC.10.MVC <- solve(-BrazilChile.CondCAViaR.10.Est$Marg$hessian)
for (i in 1:3){
  BrazilChile.CondCAViaR.10.EstSE[i,1] <- sqrt(BrazilChile.CC.10.JVC[i,i])
  BrazilChile.CondCAViaR.10.EstSE[i,2] <- sqrt(BrazilChile.CC.10.MVC[i,i])
}
#
BrazilArgentina.CondCAViaR.10.EstSE <- matrix(0:0, 3, 2)
BrazilArgentina.CC.10.JVC <- solve(-BrazilArgentina.CondCAViaR.10.Est$Joint$hessian)
BrazilArgentina.CC.10.MVC <- solve(-BrazilArgentina.CondCAViaR.10.Est$Marg$hessian)
for (i in 1:3){
  BrazilArgentina.CondCAViaR.10.EstSE[i,1] <- sqrt(BrazilArgentina.CC.10.JVC[i,i])
  BrazilArgentina.CondCAViaR.10.EstSE[i,2] <- sqrt(BrazilArgentina.CC.10.MVC[i,i])
}
#
BrazilMexico.CondCAViaR.10.EstSE <- matrix(0:0, 3, 2)
BrazilMexico.CC.10.JVC <- solve(-BrazilMexico.CondCAViaR.10.Est$Joint$hessian)
BrazilMexico.CC.10.MVC <- solve(-BrazilMexico.CondCAViaR.10.Est$Marg$hessian)
for (i in 1:3){
  BrazilMexico.CondCAViaR.10.EstSE[i,1] <- sqrt(BrazilMexico.CC.10.JVC[i,i])
  BrazilMexico.CondCAViaR.10.EstSE[i,2] <- sqrt(BrazilMexico.CC.10.MVC[i,i])
}
#
ChileArgentina.CondCAViaR.10.EstSE <- matrix(0:0, 3, 2)
ChileArgentina.CC.10.JVC <- solve(-ChileArgentina.CondCAViaR.10.Est$Joint$hessian)
ChileArgentina.CC.10.MVC <- solve(-ChileArgentina.CondCAViaR.10.Est$Marg$hessian)
for (i in 1:3){
  ChileArgentina.CondCAViaR.10.EstSE[i,1] <- sqrt(ChileArgentina.CC.10.JVC[i,i])
  ChileArgentina.CondCAViaR.10.EstSE[i,2] <- sqrt(ChileArgentina.CC.10.MVC[i,i])
}
#
ChileMexico.CondCAViaR.10.EstSE <- matrix(0:0, 3, 2)
ChileMexico.CC.10.JVC <- solve(-ChileMexico.CondCAViaR.10.Est$Joint$hessian)
ChileMexico.CC.10.MVC <- solve(-ChileMexico.CondCAViaR.10.Est$Marg$hessian)
for (i in 1:3){
  ChileMexico.CondCAViaR.10.EstSE[i,1] <- sqrt(ChileMexico.CC.10.JVC[i,i])
  ChileMexico.CondCAViaR.10.EstSE[i,2] <- sqrt(ChileMexico.CC.10.MVC[i,i])
}
#
ArgentinaMexico.CondCAViaR.10.EstSE <- matrix(0:0, 3, 2)
ArgentinaMexico.CC.10.JVC <- solve(-ArgentinaMexico.CondCAViaR.10.Est$Joint$hessian)
ArgentinaMexico.CC.10.MVC <- solve(-ArgentinaMexico.CondCAViaR.10.Est$Marg$hessian)
for (i in 1:3){
  ArgentinaMexico.CondCAViaR.10.EstSE[i,1] <- sqrt(ArgentinaMexico.CC.10.JVC[i,i])
  ArgentinaMexico.CondCAViaR.10.EstSE[i,2] <- sqrt(ArgentinaMexico.CC.10.MVC[i,i])
}
####################
####################
####################
####################
####################
####################
BrazilChile.CondCAViaR.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = BrazilChile.CondCAViaR.01.Est$Joint$par,
                                                         BetaM = BrazilChile.CondCAViaR.01.Est$Marg$par, CX1 = 1, CX2 = 2,
                                                         Cond1 = 3, Cond2 = 4, tau = 0.01)
###
BrazilArgentina.CondCAViaR.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = BrazilArgentina.CondCAViaR.01.Est$Joint$par,
                                                             BetaM = BrazilArgentina.CondCAViaR.01.Est$Marg$par, CX1 = 1, CX2 = 3,
                                                             Cond1 = 2, Cond2 = 4, tau = 0.01)
###

BrazilMexico.CondCAViaR.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = BrazilMexico.CondCAViaR.01.Est$Joint$par,
                                                          BetaM = BrazilMexico.CondCAViaR.01.Est$Marg$par, CX1 = 1, CX2 = 4,
                                                          Cond1 = 2, Cond2 = 3, tau = 0.01)
###

ChileArgentina.CondCAViaR.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = ChileArgentina.CondCAViaR.01.Est$Joint$par,
                                                            BetaM = ChileArgentina.CondCAViaR.01.Est$Marg$par, CX1 = 2, CX2 = 3,
                                                            Cond1 = 1, Cond2 = 4, tau = 0.01)
###

ChileMexico.CondCAViaR.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = ChileMexico.CondCAViaR.01.Est$Joint$par,
                                                         BetaM = ChileMexico.CondCAViaR.01.Est$Marg$par, CX1 = 2, CX2 = 4,
                                                         Cond1 = 1, Cond2 = 3, tau = 0.01)
###

ArgentinaMexico.CondCAViaR.MSE.01 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = ArgentinaMexico.CondCAViaR.01.Est$Joint$par,
                                                             BetaM = ArgentinaMexico.CondCAViaR.01.Est$Marg$par, CX1 = 3, CX2 = 4,
                                                             Cond1 = 1, Cond2 = 2, tau = 0.01)
####################
####################
####################
####################
BrazilChile.CondCAViaR.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = BrazilChile.CondCAViaR.05.Est$Joint$par,
                                                         BetaM = BrazilChile.CondCAViaR.05.Est$Marg$par, CX1 = 1, CX2 = 2,
                                                         Cond1 = 3, Cond2 = 4, tau = 0.05)
BrazilChile.CondCAViaR.MSE.05AllDates <- ConditionalEmpiricalMSEAllDates(PIT = All.cappU, BetaJ = BrazilChile.CondCAViaR.05.Est$Joint$par,
                                                                         BetaM = BrazilChile.CondCAViaR.05.Est$Marg$par, CX1 = 1, CX2 = 2,
                                                                         Cond1 = 3, Cond2 = 4, tau = 0.05)
###
BrazilArgentina.CondCAViaR.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = BrazilArgentina.CondCAViaR.05.Est$Joint$par,
                                                             BetaM = BrazilArgentina.CondCAViaR.05.Est$Marg$par, CX1 = 1, CX2 = 3,
                                                             Cond1 = 2, Cond2 = 4, tau = 0.05)
###

BrazilMexico.CondCAViaR.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = BrazilMexico.CondCAViaR.05.Est$Joint$par,
                                                          BetaM = BrazilMexico.CondCAViaR.05.Est$Marg$par, CX1 = 1, CX2 = 4,
                                                          Cond1 = 2, Cond2 = 3, tau = 0.05)
###

ChileArgentina.CondCAViaR.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = ChileArgentina.CondCAViaR.05.Est$Joint$par,
                                                            BetaM = ChileArgentina.CondCAViaR.05.Est$Marg$par, CX1 = 2, CX2 = 3,
                                                            Cond1 = 1, Cond2 = 4, tau = 0.05)
###

ChileMexico.CondCAViaR.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = ChileMexico.CondCAViaR.05.Est$Joint$par,
                                                         BetaM = ChileMexico.CondCAViaR.05.Est$Marg$par, CX1 = 2, CX2 = 4,
                                                         Cond1 = 1, Cond2 = 3, tau = 0.05)
###

ArgentinaMexico.CondCAViaR.MSE.05 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = ArgentinaMexico.CondCAViaR.05.Est$Joint$par,
                                                             BetaM = ArgentinaMexico.CondCAViaR.05.Est$Marg$par, CX1 = 3, CX2 = 4,
                                                             Cond1 = 1, Cond2 = 2, tau = 0.05)
####################
####################
####################
####################
BrazilChile.CondCAViaR.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = BrazilChile.CondCAViaR.10.Est$Joint$par,
                                                         BetaM = BrazilChile.CondCAViaR.10.Est$Marg$par, CX1 = 1, CX2 = 2,
                                                         Cond1 = 3, Cond2 = 4, tau = 0.10)
###
BrazilArgentina.CondCAViaR.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = BrazilArgentina.CondCAViaR.10.Est$Joint$par,
                                                             BetaM = BrazilArgentina.CondCAViaR.10.Est$Marg$par, CX1 = 1, CX2 = 3,
                                                             Cond1 = 2, Cond2 = 4, tau = 0.10)
###

BrazilMexico.CondCAViaR.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = BrazilMexico.CondCAViaR.10.Est$Joint$par,
                                                          BetaM = BrazilMexico.CondCAViaR.10.Est$Marg$par, CX1 = 1, CX2 = 4,
                                                          Cond1 = 2, Cond2 = 3, tau = 0.10)
###

ChileArgentina.CondCAViaR.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = ChileArgentina.CondCAViaR.10.Est$Joint$par,
                                                            BetaM = ChileArgentina.CondCAViaR.10.Est$Marg$par, CX1 = 2, CX2 = 3,
                                                            Cond1 = 1, Cond2 = 4, tau = 0.10)
###

ChileMexico.CondCAViaR.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = ChileMexico.CondCAViaR.10.Est$Joint$par,
                                                         BetaM = ChileMexico.CondCAViaR.10.Est$Marg$par, CX1 = 2, CX2 = 4,
                                                         Cond1 = 1, Cond2 = 3, tau = 0.10)
###

ArgentinaMexico.CondCAViaR.MSE.10 <- ConditionalEmpiricalMSE(PIT = All.cappU, BetaJ = ArgentinaMexico.CondCAViaR.10.Est$Joint$par,
                                                             BetaM = ArgentinaMexico.CondCAViaR.10.Est$Marg$par, CX1 = 3, CX2 = 4,
                                                             Cond1 = 1, Cond2 = 2, tau = 0.10)
####################
####################
TablePrint(c(
  "\\hline $\\tau = 0.01$", " ",   "\\hline Brazil-Chile", "se",  "\\hline  Brazil-Argentina", "se",  
  "\\hline  Brazil-Mexico","se",  "\\hline  Chile-Argentina", "se", 
  "\\hline Chile-Mexico", "se",  "\\hline Argentina-Mexico", "se", 
  "\\hline \\hline $\\tau = 0.05$", " ",    "\\hline Brazil-Chile", "se",  "\\hline  Brazil-Argentina", "se",  
  "\\hline  Brazil-Mexico","se",  "\\hline  Chile-Argentina", "se", 
  "\\hline Chile-Mexico", "se",  "\\hline Argentina-Mexico", "se", 
  "\\hline \\hline $\\tau = 0.10$"," ",   "\\hline Brazil-Chile", "se",  "\\hline  Brazil-Argentina", "se",  
  "\\hline  Brazil-Mexico","se",  "\\hline  Chile-Argentina", "se", 
  "\\hline Chile-Mexico", "se",  "\\hline Argentina-Mexico", "se", 
  
  " ", "$\\beta_{0}$", BrazilChile.CondCAViaR.01.Est$Joint$par[1], BrazilChile.CondCAViaR.01.EstSE[1,1],
  BrazilArgentina.CondCAViaR.01.Est$Joint$par[1], BrazilArgentina.CondCAViaR.01.EstSE[1,1],
  BrazilMexico.CondCAViaR.01.Est$Joint$par[1], BrazilMexico.CondCAViaR.01.EstSE[1,1],
  ChileArgentina.CondCAViaR.01.Est$Joint$par[1], ChileArgentina.CondCAViaR.01.EstSE[1,1],
  ChileMexico.CondCAViaR.01.Est$Joint$par[1], ChileMexico.CondCAViaR.01.EstSE[1,1],
  ArgentinaMexico.CondCAViaR.01.Est$Joint$par[1], ArgentinaMexico.CondCAViaR.01.EstSE[1,1], 
  
  " ", "$\\beta_{0}$", BrazilChile.CondCAViaR.05.Est$Joint$par[1], BrazilChile.CondCAViaR.05.EstSE[1,1],
  BrazilArgentina.CondCAViaR.05.Est$Joint$par[1], BrazilArgentina.CondCAViaR.05.EstSE[1,1],
  BrazilMexico.CondCAViaR.05.Est$Joint$par[1], BrazilMexico.CondCAViaR.05.EstSE[1,1],
  ChileArgentina.CondCAViaR.05.Est$Joint$par[1], ChileArgentina.CondCAViaR.05.EstSE[1,1],
  ChileMexico.CondCAViaR.05.Est$Joint$par[1], ChileMexico.CondCAViaR.05.EstSE[1,1],
  ArgentinaMexico.CondCAViaR.05.Est$Joint$par[1], ArgentinaMexico.CondCAViaR.05.EstSE[1,1], 
  
  " ", "$\\beta_{0}$", BrazilChile.CondCAViaR.10.Est$Joint$par[1], BrazilChile.CondCAViaR.10.EstSE[1,1],
  BrazilArgentina.CondCAViaR.10.Est$Joint$par[1], BrazilArgentina.CondCAViaR.10.EstSE[1,1],
  BrazilMexico.CondCAViaR.10.Est$Joint$par[1], BrazilMexico.CondCAViaR.10.EstSE[1,1],
  ChileArgentina.CondCAViaR.10.Est$Joint$par[1], ChileArgentina.CondCAViaR.10.EstSE[1,1],
  ChileMexico.CondCAViaR.10.Est$Joint$par[1], ChileMexico.CondCAViaR.10.EstSE[1,1],
  ArgentinaMexico.CondCAViaR.10.Est$Joint$par[1], ArgentinaMexico.CondCAViaR.10.EstSE[1,1], 
  
  " ", "$\\beta_{1}$", BrazilChile.CondCAViaR.01.Est$Joint$par[2], BrazilChile.CondCAViaR.01.EstSE[2,1],
  BrazilArgentina.CondCAViaR.01.Est$Joint$par[2], BrazilArgentina.CondCAViaR.01.EstSE[2,1],
  BrazilMexico.CondCAViaR.01.Est$Joint$par[2], BrazilMexico.CondCAViaR.01.EstSE[2,1],
  ChileArgentina.CondCAViaR.01.Est$Joint$par[2], ChileArgentina.CondCAViaR.01.EstSE[2,1],
  ChileMexico.CondCAViaR.01.Est$Joint$par[2], ChileMexico.CondCAViaR.01.EstSE[2,1],
  ArgentinaMexico.CondCAViaR.01.Est$Joint$par[2], ArgentinaMexico.CondCAViaR.01.EstSE[2,1],
  
  " ", "$\\beta_{1}$", BrazilChile.CondCAViaR.05.Est$Joint$par[2], BrazilChile.CondCAViaR.05.EstSE[2,1],
  BrazilArgentina.CondCAViaR.05.Est$Joint$par[2], BrazilArgentina.CondCAViaR.05.EstSE[2,1],
  BrazilMexico.CondCAViaR.05.Est$Joint$par[2], BrazilMexico.CondCAViaR.05.EstSE[2,1],
  ChileArgentina.CondCAViaR.05.Est$Joint$par[2], ChileArgentina.CondCAViaR.05.EstSE[2,1],
  ChileMexico.CondCAViaR.05.Est$Joint$par[2], ChileMexico.CondCAViaR.05.EstSE[2,1],
  ArgentinaMexico.CondCAViaR.05.Est$Joint$par[2], ArgentinaMexico.CondCAViaR.05.EstSE[2,1], 
  
  " ", "$\\beta_{1}$", BrazilChile.CondCAViaR.10.Est$Joint$par[2], BrazilChile.CondCAViaR.10.EstSE[2,1],
  BrazilArgentina.CondCAViaR.10.Est$Joint$par[2], BrazilArgentina.CondCAViaR.10.EstSE[2,1],
  BrazilMexico.CondCAViaR.10.Est$Joint$par[2], BrazilMexico.CondCAViaR.10.EstSE[2,1],
  ChileArgentina.CondCAViaR.10.Est$Joint$par[2], ChileArgentina.CondCAViaR.10.EstSE[2,1],
  ChileMexico.CondCAViaR.10.Est$Joint$par[2], ChileMexico.CondCAViaR.10.EstSE[2,1],
  ArgentinaMexico.CondCAViaR.10.Est$Joint$par[2], ArgentinaMexico.CondCAViaR.10.EstSE[2,1], 
  
  " ", "$\\beta_{2}$", BrazilChile.CondCAViaR.01.Est$Joint$par[3], BrazilChile.CondCAViaR.01.EstSE[3,1],
  BrazilArgentina.CondCAViaR.01.Est$Joint$par[3], BrazilArgentina.CondCAViaR.01.EstSE[3,1],
  BrazilMexico.CondCAViaR.01.Est$Joint$par[3], BrazilMexico.CondCAViaR.01.EstSE[3,1],
  ChileArgentina.CondCAViaR.01.Est$Joint$par[3], ChileArgentina.CondCAViaR.01.EstSE[3,1],
  ChileMexico.CondCAViaR.01.Est$Joint$par[3], ChileMexico.CondCAViaR.01.EstSE[3,1],
  ArgentinaMexico.CondCAViaR.01.Est$Joint$par[3], ArgentinaMexico.CondCAViaR.01.EstSE[3,1], 
  
  " ", "$\\beta_{2}$", BrazilChile.CondCAViaR.05.Est$Joint$par[3], BrazilChile.CondCAViaR.05.EstSE[3,1],
  BrazilArgentina.CondCAViaR.05.Est$Joint$par[3], BrazilArgentina.CondCAViaR.05.EstSE[3,1],
  BrazilMexico.CondCAViaR.05.Est$Joint$par[3], BrazilMexico.CondCAViaR.05.EstSE[3,1],
  ChileArgentina.CondCAViaR.05.Est$Joint$par[3], ChileArgentina.CondCAViaR.05.EstSE[3,1],
  ChileMexico.CondCAViaR.05.Est$Joint$par[3], ChileMexico.CondCAViaR.05.EstSE[3,1],
  ArgentinaMexico.CondCAViaR.05.Est$Joint$par[3], ArgentinaMexico.CondCAViaR.05.EstSE[3,1],
  
  " ", "$\\beta_{2}$", BrazilChile.CondCAViaR.10.Est$Joint$par[3], BrazilChile.CondCAViaR.10.EstSE[3,1],
  BrazilArgentina.CondCAViaR.10.Est$Joint$par[3], BrazilArgentina.CondCAViaR.10.EstSE[3,1],
  BrazilMexico.CondCAViaR.10.Est$Joint$par[3], BrazilMexico.CondCAViaR.10.EstSE[3,1],
  ChileArgentina.CondCAViaR.10.Est$Joint$par[3], ChileArgentina.CondCAViaR.10.EstSE[3,1],
  ChileMexico.CondCAViaR.10.Est$Joint$par[3], ChileMexico.CondCAViaR.10.EstSE[3,1],
  ArgentinaMexico.CondCAViaR.10.Est$Joint$par[3], ArgentinaMexico.CondCAViaR.10.EstSE[3,1]
), 42, 4)

####################
####################
TablePrint(c(
  "\\hline $\\tau = 0.01$", " ",   "\\hline Brazil-Chile", "se",  "\\hline  Brazil-Argentina", "se",  
  "\\hline  Brazil-Mexico","se",  "\\hline  Chile-Argentina", "se", 
  "\\hline Chile-Mexico", "se",  "\\hline Argentina-Mexico", "se", 
  "\\hline \\hline $\\tau = 0.05$", " ",    "\\hline Brazil-Chile", "se",  "\\hline  Brazil-Argentina", "se",  
  "\\hline  Brazil-Mexico","se",  "\\hline  Chile-Argentina", "se", 
  "\\hline Chile-Mexico", "se",  "\\hline Argentina-Mexico", "se", 
  "\\hline \\hline $\\tau = 0.10$"," ",   "\\hline Brazil-Chile", "se",  "\\hline  Brazil-Argentina", "se",  
  "\\hline  Brazil-Mexico","se",  "\\hline  Chile-Argentina", "se", 
  "\\hline Chile-Mexico", "se",  "\\hline Argentina-Mexico", "se", 
  
  " ", "$\\beta_{0}$", BrazilChile.CondCAViaR.01.Est$Marg$par[1], BrazilChile.CondCAViaR.01.EstSE[1,1],
  BrazilArgentina.CondCAViaR.01.Est$Marg$par[1], BrazilArgentina.CondCAViaR.01.EstSE[1,1],
  BrazilMexico.CondCAViaR.01.Est$Marg$par[1], BrazilMexico.CondCAViaR.01.EstSE[1,1],
  ChileArgentina.CondCAViaR.01.Est$Marg$par[1], ChileArgentina.CondCAViaR.01.EstSE[1,1],
  ChileMexico.CondCAViaR.01.Est$Marg$par[1], ChileMexico.CondCAViaR.01.EstSE[1,1],
  ArgentinaMexico.CondCAViaR.01.Est$Marg$par[1], ArgentinaMexico.CondCAViaR.01.EstSE[1,1], 
  
  " ", "$\\beta_{0}$", BrazilChile.CondCAViaR.05.Est$Marg$par[1], BrazilChile.CondCAViaR.05.EstSE[1,1],
  BrazilArgentina.CondCAViaR.05.Est$Marg$par[1], BrazilArgentina.CondCAViaR.05.EstSE[1,1],
  BrazilMexico.CondCAViaR.05.Est$Marg$par[1], BrazilMexico.CondCAViaR.05.EstSE[1,1],
  ChileArgentina.CondCAViaR.05.Est$Marg$par[1], ChileArgentina.CondCAViaR.05.EstSE[1,1],
  ChileMexico.CondCAViaR.05.Est$Marg$par[1], ChileMexico.CondCAViaR.05.EstSE[1,1],
  ArgentinaMexico.CondCAViaR.05.Est$Marg$par[1], ArgentinaMexico.CondCAViaR.05.EstSE[1,1], 
  
  " ", "$\\beta_{0}$", BrazilChile.CondCAViaR.10.Est$Marg$par[1], BrazilChile.CondCAViaR.10.EstSE[1,1],
  BrazilArgentina.CondCAViaR.10.Est$Marg$par[1], BrazilArgentina.CondCAViaR.10.EstSE[1,1],
  BrazilMexico.CondCAViaR.10.Est$Marg$par[1], BrazilMexico.CondCAViaR.10.EstSE[1,1],
  ChileArgentina.CondCAViaR.10.Est$Marg$par[1], ChileArgentina.CondCAViaR.10.EstSE[1,1],
  ChileMexico.CondCAViaR.10.Est$Marg$par[1], ChileMexico.CondCAViaR.10.EstSE[1,1],
  ArgentinaMexico.CondCAViaR.10.Est$Marg$par[1], ArgentinaMexico.CondCAViaR.10.EstSE[1,1], 
  
  " ", "$\\beta_{1}$", BrazilChile.CondCAViaR.01.Est$Marg$par[2], BrazilChile.CondCAViaR.01.EstSE[2,1],
  BrazilArgentina.CondCAViaR.01.Est$Marg$par[2], BrazilArgentina.CondCAViaR.01.EstSE[2,1],
  BrazilMexico.CondCAViaR.01.Est$Marg$par[2], BrazilMexico.CondCAViaR.01.EstSE[2,1],
  ChileArgentina.CondCAViaR.01.Est$Marg$par[2], ChileArgentina.CondCAViaR.01.EstSE[2,1],
  ChileMexico.CondCAViaR.01.Est$Marg$par[2], ChileMexico.CondCAViaR.01.EstSE[2,1],
  ArgentinaMexico.CondCAViaR.01.Est$Marg$par[2], ArgentinaMexico.CondCAViaR.01.EstSE[2,1],
  
  " ", "$\\beta_{1}$", BrazilChile.CondCAViaR.05.Est$Marg$par[2], BrazilChile.CondCAViaR.05.EstSE[2,1],
  BrazilArgentina.CondCAViaR.05.Est$Marg$par[2], BrazilArgentina.CondCAViaR.05.EstSE[2,1],
  BrazilMexico.CondCAViaR.05.Est$Marg$par[2], BrazilMexico.CondCAViaR.05.EstSE[2,1],
  ChileArgentina.CondCAViaR.05.Est$Marg$par[2], ChileArgentina.CondCAViaR.05.EstSE[2,1],
  ChileMexico.CondCAViaR.05.Est$Marg$par[2], ChileMexico.CondCAViaR.05.EstSE[2,1],
  ArgentinaMexico.CondCAViaR.05.Est$Marg$par[2], ArgentinaMexico.CondCAViaR.05.EstSE[2,1], 
  
  " ", "$\\beta_{1}$", BrazilChile.CondCAViaR.10.Est$Marg$par[2], BrazilChile.CondCAViaR.10.EstSE[2,1],
  BrazilArgentina.CondCAViaR.10.Est$Marg$par[2], BrazilArgentina.CondCAViaR.10.EstSE[2,1],
  BrazilMexico.CondCAViaR.10.Est$Marg$par[2], BrazilMexico.CondCAViaR.10.EstSE[2,1],
  ChileArgentina.CondCAViaR.10.Est$Marg$par[2], ChileArgentina.CondCAViaR.10.EstSE[2,1],
  ChileMexico.CondCAViaR.10.Est$Marg$par[2], ChileMexico.CondCAViaR.10.EstSE[2,1],
  ArgentinaMexico.CondCAViaR.10.Est$Marg$par[2], ArgentinaMexico.CondCAViaR.10.EstSE[2,1], 
  
  " ", "$\\beta_{2}$", BrazilChile.CondCAViaR.01.Est$Marg$par[3], BrazilChile.CondCAViaR.01.EstSE[3,1],
  BrazilArgentina.CondCAViaR.01.Est$Marg$par[3], BrazilArgentina.CondCAViaR.01.EstSE[3,1],
  BrazilMexico.CondCAViaR.01.Est$Marg$par[3], BrazilMexico.CondCAViaR.01.EstSE[3,1],
  ChileArgentina.CondCAViaR.01.Est$Marg$par[3], ChileArgentina.CondCAViaR.01.EstSE[3,1],
  ChileMexico.CondCAViaR.01.Est$Marg$par[3], ChileMexico.CondCAViaR.01.EstSE[3,1],
  ArgentinaMexico.CondCAViaR.01.Est$Marg$par[3], ArgentinaMexico.CondCAViaR.01.EstSE[3,1], 
  
  " ", "$\\beta_{2}$", BrazilChile.CondCAViaR.05.Est$Marg$par[3], BrazilChile.CondCAViaR.05.EstSE[3,1],
  BrazilArgentina.CondCAViaR.05.Est$Marg$par[3], BrazilArgentina.CondCAViaR.05.EstSE[3,1],
  BrazilMexico.CondCAViaR.05.Est$Marg$par[3], BrazilMexico.CondCAViaR.05.EstSE[3,1],
  ChileArgentina.CondCAViaR.05.Est$Marg$par[3], ChileArgentina.CondCAViaR.05.EstSE[3,1],
  ChileMexico.CondCAViaR.05.Est$Marg$par[3], ChileMexico.CondCAViaR.05.EstSE[3,1],
  ArgentinaMexico.CondCAViaR.05.Est$Marg$par[3], ArgentinaMexico.CondCAViaR.05.EstSE[3,1],
  
  " ", "$\\beta_{2}$", BrazilChile.CondCAViaR.10.Est$Marg$par[3], BrazilChile.CondCAViaR.10.EstSE[3,1],
  BrazilArgentina.CondCAViaR.10.Est$Marg$par[3], BrazilArgentina.CondCAViaR.10.EstSE[3,1],
  BrazilMexico.CondCAViaR.10.Est$Marg$par[3], BrazilMexico.CondCAViaR.10.EstSE[3,1],
  ChileArgentina.CondCAViaR.10.Est$Marg$par[3], ChileArgentina.CondCAViaR.10.EstSE[3,1],
  ChileMexico.CondCAViaR.10.Est$Marg$par[3], ChileMexico.CondCAViaR.10.EstSE[3,1],
  ArgentinaMexico.CondCAViaR.10.Est$Marg$par[3], ArgentinaMexico.CondCAViaR.10.EstSE[3,1]
), 42, 4)


TablePrint(c(
  "\\hline \\textbf{MSE of Prediction}", "\\textbf{of Coexceedences}",  "\\hline Brazil Chile", " ", " ", " ", "\\hline Brazil Argentina", " ", " ", " ", 
  "\\hline Brazil Mexico", " ", " ", " ", "\\hline Chile Argentina", " ", " ", " ", "\\hline Chile Mexico", " ", " ", " ", 
  "\\hline Argentina Mexico", " ", " ", " ", 
  " ", " ", "Unconditional CAViaR", "Unconditional Empirical", "Conditional CAViaR", "Conditional Empirical",
  "Unconditional CAViaR", "Unconditional Empirical", "Conditional CAViaR", "Conditional Empirical",
  "Unconditional CAViaR", "Unconditional Empirical", "Conditional CAViaR", "Conditional Empirical",
  "Unconditional CAViaR", "Unconditional Empirical", "Conditional CAViaR", "Conditional Empirical",
  "Unconditional CAViaR", "Unconditional Empirical", "Conditional CAViaR", "Conditional Empirical",
  "Unconditional CAViaR", "Unconditional Empirical", "Conditional CAViaR", "Conditional Empirical",
  " ", "$\\tau = 0.05$", BrazilChile.PW.CAViaR.05[1,5], BrazilChile.PW.Emp.05[1,5], BrazilChile.CondCAViaR.MSE.05[1,5], BrazilChile.CondEmp.MSE.05[1,5],
  BrazilArgentina.PW.CAViaR.05[1,5], BrazilArgentina.PW.Emp.05[1,5], BrazilArgentina.CondCAViaR.MSE.05[1,5], BrazilArgentina.CondEmp.MSE.05[1,5],
  BrazilMexico.PW.CAViaR.05[1,5], BrazilMexico.PW.Emp.05[1,5], BrazilMexico.CondCAViaR.MSE.05[1,5], BrazilMexico.CondEmp.MSE.05[1,5],
  ChileArgentina.PW.CAViaR.05[1,5], ChileArgentina.PW.Emp.05[1,5], ChileArgentina.CondCAViaR.MSE.05[1,5], ChileArgentina.CondEmp.MSE.05[1,5],
  ChileMexico.PW.CAViaR.05[1,5], ChileMexico.PW.Emp.05[1,5], ChileMexico.CondCAViaR.MSE.05[1,5], ChileMexico.CondEmp.MSE.05[1,5],
  ArgentinaMexico.PW.CAViaR.05[1,5], ArgentinaMexico.PW.Emp.05[1,5], ArgentinaMexico.CondCAViaR.MSE.05[1,5], ArgentinaMexico.CondEmp.MSE.05[1,5],
  
  " ", "$\\tau = 0.10$", BrazilChile.PW.CAViaR.10[1,5], BrazilChile.PW.Emp.10[1,5], BrazilChile.CondCAViaR.MSE.10[1,5], BrazilChile.CondEmp.MSE.10[1,5],
  BrazilArgentina.PW.CAViaR.10[1,5], BrazilArgentina.PW.Emp.10[1,5], BrazilArgentina.CondCAViaR.MSE.10[1,5], BrazilArgentina.CondEmp.MSE.10[1,5],
  BrazilMexico.PW.CAViaR.10[1,5], BrazilMexico.PW.Emp.10[1,5], BrazilMexico.CondCAViaR.MSE.10[1,5], BrazilMexico.CondEmp.MSE.10[1,5],
  ChileArgentina.PW.CAViaR.10[1,5], ChileArgentina.PW.Emp.10[1,5], ChileArgentina.CondCAViaR.MSE.10[1,5], ChileArgentina.CondEmp.MSE.10[1,5],
  ChileMexico.PW.CAViaR.10[1,5], ChileMexico.PW.Emp.10[1,5], ChileMexico.CondCAViaR.MSE.10[1,5], ChileMexico.CondEmp.MSE.10[1,5],
  ArgentinaMexico.PW.CAViaR.10[1,5], ArgentinaMexico.PW.Emp.10[1,5], ArgentinaMexico.CondCAViaR.MSE.10[1,5], ArgentinaMexico.CondEmp.MSE.10[1,5]
  
), 26, 4)

####################
####################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BCCondEmp01.pdf")
plot(BrazilChile.CondEmp.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Chile \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional Empirical Model")
points(BrazilChile.CondEmp.MSE.01[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BACondEmp01.pdf")
plot(BrazilArgentina.CondEmp.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Argentina \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional Empirical Model")
points(BrazilArgentina.CondEmp.MSE.01[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BMCondEmp01.pdf")
plot(BrazilMexico.CondEmp.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Mexico \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional Empirical Model")
points(BrazilMexico.CondEmp.MSE.01[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CACondEmp01.pdf")
plot(ChileArgentina.CondEmp.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Argentina \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional Empirical Model")
points(ChileArgentina.CondEmp.MSE.01[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CMCondEmp01.pdf")
plot(ChileMexico.CondEmp.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Mexico \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional Empirical Model")
points(ChileMexico.CondEmp.MSE.01[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/AMCondEmp01.pdf")
plot(ArgentinaMexico.CondEmp.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Argentina-Mexico \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional Empirical Model")
points(ArgentinaMexico.CondEmp.MSE.01[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
####################
####################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BCCondEmp05.pdf")
plot(BrazilChile.CondEmp.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Chile \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional Empirical Model")
points(BrazilChile.CondEmp.MSE.05[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BACondEmp05.pdf")
plot(BrazilArgentina.CondEmp.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Argentina \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional Empirical Model")
points(BrazilArgentina.CondEmp.MSE.05[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BMCondEmp05.pdf")
plot(BrazilMexico.CondEmp.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Mexico \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional Empirical Model")
points(BrazilMexico.CondEmp.MSE.05[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CACondEmp05.pdf")
plot(ChileArgentina.CondEmp.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Argentina \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional Empirical Model")
points(ChileArgentina.CondEmp.MSE.05[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CMCondEmp05.pdf")
plot(ChileMexico.CondEmp.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Mexico \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional Empirical Model")
points(ChileMexico.CondEmp.MSE.05[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/AMCondEmp05.pdf")
plot(ArgentinaMexico.CondEmp.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Argentina-Mexico \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional Empirical Model")
points(ArgentinaMexico.CondEmp.MSE.05[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
####################
####################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BCCondEmp10.pdf")
plot(BrazilChile.CondEmp.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Chile \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional Empirical Model")
points(BrazilChile.CondEmp.MSE.10[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BACondEmp10.pdf")
plot(BrazilArgentina.CondEmp.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Argentina \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional Empirical Model")
points(BrazilArgentina.CondEmp.MSE.10[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BMCondEmp10.pdf")
plot(BrazilMexico.CondEmp.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Mexico \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional Empirical Model")
points(BrazilMexico.CondEmp.MSE.10[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CACondEmp10.pdf")
plot(ChileArgentina.CondEmp.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Argentina \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional Empirical Model")
points(ChileArgentina.CondEmp.MSE.10[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CMCondEmp10.pdf")
plot(ChileMexico.CondEmp.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Mexico \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional Empirical Model")
points(ChileMexico.CondEmp.MSE.10[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/AMCondEmp10.pdf")
plot(ArgentinaMexico.CondEmp.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Argentina-Mexico \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional Empirical Model")
points(ArgentinaMexico.CondEmp.MSE.10[,c(1,2)], type = "l", col = "red")
legend("topleft", col = c("black", "red"), c("Instances of Coexceedences", "Expected Probability - Conditional Empirical Model"), 
       lwd = c(1,1))
dev.off()


############################################
############################################
############################################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BCCondt01.pdf")
plot(BrazilChile.All.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Chile \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional t-Copula")
points(BrazilChile.All.MSE.01[,c(1,2)], type = "l", col = "blue")
legend("topleft", col = c("black", "blue"), c("Instances of Coexceedences", "Expected Probability - Conditional t-Copula"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BACondt01.pdf")
plot(BrazilArgentina.All.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Argentina \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional t-Copula")
points(BrazilArgentina.All.MSE.01[,c(1,2)], type = "l", col = "blue")
legend("topleft", col = c("black", "blue"), c("Instances of Coexceedences", "Expected Probability - Conditional t-Copula"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BMCondt01.pdf")
plot(BrazilMexico.All.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Mexico \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional t-Copula")
points(BrazilMexico.All.MSE.01[,c(1,2)], type = "l", col = "blue")
legend("topleft", col = c("black", "blue"), c("Instances of Coexceedences", "Expected Probability - Conditional t-Copula"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CACondt01.pdf")
plot(ChileArgentina.All.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Argentina \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional t-Copula")
points(ChileArgentina.All.MSE.01[,c(1,2)], type = "l", col = "blue")
legend("topleft", col = c("black", "blue"), c("Instances of Coexceedences", "Expected Probability - Conditional t-Copula"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CMCondt01.pdf")
plot(ChileMexico.All.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Mexico \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional t-Copula")
points(ChileMexico.All.MSE.01[,c(1,2)], type = "l", col = "blue")
legend("topleft", col = c("black", "blue"), c("Instances of Coexceedences", "Expected Probability - Conditional t-Copula"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/AMCondt01.pdf")
plot(ArgentinaMexico.All.MSE.01[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Argentina-Mexico \n Expected Probability of Coexceedence at tau = 0.01 \n Conditional t-Copula")
points(ArgentinaMexico.All.MSE.01[,c(1,2)], type = "l", col = "blue")
legend("topleft", col = c("black", "blue"), c("Instances of Coexceedences", "Expected Probability - Conditional t-Copula"), 
       lwd = c(1,1))
dev.off()
#

##########################################################
###########################################################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BCCondCAViaR05.pdf")
plot(BrazilChile.CondCAViaR.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Chile \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional CAViaR Logit Model")
points(BrazilChile.CondCAViaR.MSE.05[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BACondCAViaR05.pdf")
plot(BrazilArgentina.CondCAViaR.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Argentina \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional CAViaR Logit Model")
points(BrazilArgentina.CondCAViaR.MSE.05[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BMCondCAViaR05.pdf")
plot(BrazilMexico.CondCAViaR.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Mexico \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional CAViaR Logit Model")
points(BrazilMexico.CondCAViaR.MSE.05[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CACondCAViaR05.pdf")
plot(ChileArgentina.CondCAViaR.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Argentina \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional CAViaR Logit Model")
points(ChileArgentina.CondCAViaR.MSE.05[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CMCondCAViaR05.pdf")
plot(ChileMexico.CondCAViaR.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Mexico \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional CAViaR Logit Model")
points(ChileMexico.CondCAViaR.MSE.05[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/AMCondCAViaR05.pdf")
plot(ArgentinaMexico.CondCAViaR.MSE.05[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Argentina-Mexico \n Expected Probability of Coexceedence at tau = 0.05 \n Conditional CAViaR Logit Model")
points(ArgentinaMexico.CondCAViaR.MSE.05[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
##########################################################
###########################################################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BCCondCAViaR10.pdf")
plot(BrazilChile.CondCAViaR.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Chile \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional CAViaR Logit Model")
points(BrazilChile.CondCAViaR.MSE.10[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BACondCAViaR10.pdf")
plot(BrazilArgentina.CondCAViaR.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Argentina \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional CAViaR Logit Model")
points(BrazilArgentina.CondCAViaR.MSE.10[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/BMCondCAViaR10.pdf")
plot(BrazilMexico.CondCAViaR.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Brazil-Mexico \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional CAViaR Logit Model")
points(BrazilMexico.CondCAViaR.MSE.10[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CACondCAViaR10.pdf")
plot(ChileArgentina.CondCAViaR.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Argentina \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional CAViaR Logit Model")
points(ChileArgentina.CondCAViaR.MSE.10[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/CMCondCAViaR10.pdf")
plot(ChileMexico.CondCAViaR.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Chile-Mexico \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional CAViaR Logit Model")
points(ChileMexico.CondCAViaR.MSE.10[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()
#
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/CondMSE/AMCondCAViaR10.pdf")
plot(ArgentinaMexico.CondCAViaR.MSE.10[,c(1,3)], type = "l", col = "black", ylim = c(0,1.2), xlab = "Date", ylab = "Expected Probability",
     main = "Argentina-Mexico \n Expected Probability of Coexceedence at tau = 0.10 \n Conditional CAViaR Logit Model")
points(ArgentinaMexico.CondCAViaR.MSE.10[,c(1,2)], type = "l", col = "green")
legend("topleft", col = c("black", "green"), c("Instances of Coexceedences", "Expected Probability - Conditional CAViaR Logit Model"), 
       lwd = c(1,1))
dev.off()

TablePrint(c(
  "\\hline $\\tau = 0.05$", "\\hline $\\hat{\\beta}_{0}$", "se", "\\hline $\\hat{\\beta}_{1}$", "se", "\\hline $\\hat{\\beta}_{2}$", "se",
  "Joint Logit Model", BrazilChile.CondCAViaR.05.Est$Joint$par[1], BrazilChile.CondCAViaR.05.EstSE[1,1], 
  BrazilChile.CondCAViaR.05.Est$Joint$par[2], BrazilChile.CondCAViaR.05.EstSE[2,1], 
  BrazilChile.CondCAViaR.05.Est$Joint$par[3], BrazilChile.CondCAViaR.05.EstSE[3,1], 
  
  "Marginal Logit Model", BrazilChile.CondCAViaR.05.Est$Marg$par[1], BrazilChile.CondCAViaR.05.EstSE[1,2], 
  BrazilChile.CondCAViaR.05.Est$Marg$par[2], BrazilChile.CondCAViaR.05.EstSE[2,2], 
  BrazilChile.CondCAViaR.05.Est$Marg$par[3], BrazilChile.CondCAViaR.05.EstSE[3,2]
), 7,3)

TablePrint(c(
  "\\hline $\\tau = 0.10$", "\\hline $\\hat{\\beta}_{0}$", "se", "\\hline $\\hat{\\beta}_{1}$", "se", "\\hline $\\hat{\\beta}_{2}$", "se",
  "Joint Logit Model", BrazilChile.CondCAViaR.10.Est$Joint$par[1], BrazilChile.CondCAViaR.10.EstSE[1,1], 
  BrazilChile.CondCAViaR.10.Est$Joint$par[2], BrazilChile.CondCAViaR.10.EstSE[2,1], 
  BrazilChile.CondCAViaR.10.Est$Joint$par[3], BrazilChile.CondCAViaR.10.EstSE[3,1], 
  
  "Marginal Logit Model", BrazilChile.CondCAViaR.10.Est$Marg$par[1], BrazilChile.CondCAViaR.10.EstSE[1,2], 
  BrazilChile.CondCAViaR.10.Est$Marg$par[2], BrazilChile.CondCAViaR.10.EstSE[2,2], 
  BrazilChile.CondCAViaR.10.Est$Marg$par[3], BrazilChile.CondCAViaR.10.EstSE[3,2]
), 7,3)


TablePrint(c(
  "\\hline $\\tau = 0.05$", "$\\nu_{J0}$", " ", "$\\nu_{J1}$", " ", "$\\nu_{J2}$", " ",
  "Brazil-Chile", BrazilChile.CondCAViaR.05.Est$Joint$par[1], BrazilChile.CondCAViaR.05.EstSE[1,1], 
  BrazilChile.CondCAViaR.05.Est$Joint$par[2], BrazilChile.CondCAViaR.05.EstSE[2,1], 
  BrazilChile.CondCAViaR.05.Est$Joint$par[3], BrazilChile.CondCAViaR.05.EstSE[3,1], 
  "Brazil-Argentina", BrazilArgentina.CondCAViaR.05.Est$Joint$par[1], BrazilArgentina.CondCAViaR.05.EstSE[1,1], 
  BrazilArgentina.CondCAViaR.05.Est$Joint$par[2], BrazilArgentina.CondCAViaR.05.EstSE[2,1], 
  BrazilArgentina.CondCAViaR.05.Est$Joint$par[3], BrazilArgentina.CondCAViaR.05.EstSE[3,1],
  "Brazil-Mexico", BrazilMexico.CondCAViaR.05.Est$Joint$par[1], BrazilMexico.CondCAViaR.05.EstSE[1,1], 
  BrazilMexico.CondCAViaR.05.Est$Joint$par[2], BrazilMexico.CondCAViaR.05.EstSE[2,1], 
  BrazilMexico.CondCAViaR.05.Est$Joint$par[3], BrazilMexico.CondCAViaR.05.EstSE[3,1], 
  "Chile-Argentina", ChileArgentina.CondCAViaR.05.Est$Joint$par[1], ChileArgentina.CondCAViaR.05.EstSE[1,1], 
  ChileArgentina.CondCAViaR.05.Est$Joint$par[2], ChileArgentina.CondCAViaR.05.EstSE[2,1], 
  ChileArgentina.CondCAViaR.05.Est$Joint$par[3], ChileArgentina.CondCAViaR.05.EstSE[3,1],
  "Chile-Mexico", ChileMexico.CondCAViaR.05.Est$Joint$par[1], ChileMexico.CondCAViaR.05.EstSE[1,1], 
  ChileMexico.CondCAViaR.05.Est$Joint$par[2], ChileMexico.CondCAViaR.05.EstSE[2,1], 
  ChileMexico.CondCAViaR.05.Est$Joint$par[3], ChileMexico.CondCAViaR.05.EstSE[3,1], 
  "Argentina-Mexico", ArgentinaMexico.CondCAViaR.05.Est$Joint$par[1], ArgentinaMexico.CondCAViaR.05.EstSE[1,1], 
  ArgentinaMexico.CondCAViaR.05.Est$Joint$par[2], ArgentinaMexico.CondCAViaR.05.EstSE[2,1], 
  ArgentinaMexico.CondCAViaR.05.Est$Joint$par[3], ArgentinaMexico.CondCAViaR.05.EstSE[3,1]
),7,7)

TablePrint(c(
  "\\hline $\\tau = 0.05$", "$\\nu_{M0}$", " ", "$\\nu_{M1}$", " ", "$\\nu_{M2}$", " ",
  "Brazil-Chile", BrazilChile.CondCAViaR.05.Est$Marg$par[1], BrazilChile.CondCAViaR.05.EstSE[1,2], 
  BrazilChile.CondCAViaR.05.Est$Marg$par[2], BrazilChile.CondCAViaR.05.EstSE[2,2], 
  BrazilChile.CondCAViaR.05.Est$Marg$par[3], BrazilChile.CondCAViaR.05.EstSE[3,2], 
  "Brazil-Argentina", BrazilArgentina.CondCAViaR.05.Est$Marg$par[1], BrazilArgentina.CondCAViaR.05.EstSE[1,2], 
  BrazilArgentina.CondCAViaR.05.Est$Marg$par[2], BrazilArgentina.CondCAViaR.05.EstSE[2,2], 
  BrazilArgentina.CondCAViaR.05.Est$Marg$par[3], BrazilArgentina.CondCAViaR.05.EstSE[3,2],
  "Brazil-Mexico", BrazilMexico.CondCAViaR.05.Est$Marg$par[1], BrazilMexico.CondCAViaR.05.EstSE[1,2], 
  BrazilMexico.CondCAViaR.05.Est$Marg$par[2], BrazilMexico.CondCAViaR.05.EstSE[2,2], 
  BrazilMexico.CondCAViaR.05.Est$Marg$par[3], BrazilMexico.CondCAViaR.05.EstSE[3,2], 
  "Chile-Argentina", ChileArgentina.CondCAViaR.05.Est$Marg$par[1], ChileArgentina.CondCAViaR.05.EstSE[1,2], 
  ChileArgentina.CondCAViaR.05.Est$Marg$par[2], ChileArgentina.CondCAViaR.05.EstSE[2,2], 
  ChileArgentina.CondCAViaR.05.Est$Marg$par[3], ChileArgentina.CondCAViaR.05.EstSE[3,2],
  "Chile-Mexico", ChileMexico.CondCAViaR.05.Est$Marg$par[1], ChileMexico.CondCAViaR.05.EstSE[1,2], 
  ChileMexico.CondCAViaR.05.Est$Marg$par[2], ChileMexico.CondCAViaR.05.EstSE[2,2], 
  ChileMexico.CondCAViaR.05.Est$Marg$par[3], ChileMexico.CondCAViaR.05.EstSE[3,2], 
  "Argentina-Mexico", ArgentinaMexico.CondCAViaR.05.Est$Marg$par[1], ArgentinaMexico.CondCAViaR.05.EstSE[1,2], 
  ArgentinaMexico.CondCAViaR.05.Est$Marg$par[2], ArgentinaMexico.CondCAViaR.05.EstSE[2,2], 
  ArgentinaMexico.CondCAViaR.05.Est$Marg$par[3], ArgentinaMexico.CondCAViaR.05.EstSE[3,2]
),7,7)


TablePrint(c(
  "\\hline $\\tau = 0.10$", "$\\nu_{J0}$", " ", "$\\nu_{J1}$", " ", "$\\nu_{J2}$", " ",
  "Brazil-Chile", BrazilChile.CondCAViaR.10.Est$Joint$par[1], BrazilChile.CondCAViaR.10.EstSE[1,1], 
  BrazilChile.CondCAViaR.10.Est$Joint$par[2], BrazilChile.CondCAViaR.10.EstSE[2,1], 
  BrazilChile.CondCAViaR.10.Est$Joint$par[3], BrazilChile.CondCAViaR.10.EstSE[3,1], 
  "Brazil-Argentina", BrazilArgentina.CondCAViaR.10.Est$Joint$par[1], BrazilArgentina.CondCAViaR.10.EstSE[1,1], 
  BrazilArgentina.CondCAViaR.10.Est$Joint$par[2], BrazilArgentina.CondCAViaR.10.EstSE[2,1], 
  BrazilArgentina.CondCAViaR.10.Est$Joint$par[3], BrazilArgentina.CondCAViaR.10.EstSE[3,1],
  "Brazil-Mexico", BrazilMexico.CondCAViaR.10.Est$Joint$par[1], BrazilMexico.CondCAViaR.10.EstSE[1,1], 
  BrazilMexico.CondCAViaR.10.Est$Joint$par[2], BrazilMexico.CondCAViaR.10.EstSE[2,1], 
  BrazilMexico.CondCAViaR.10.Est$Joint$par[3], BrazilMexico.CondCAViaR.10.EstSE[3,1], 
  "Chile-Argentina", ChileArgentina.CondCAViaR.10.Est$Joint$par[1], ChileArgentina.CondCAViaR.10.EstSE[1,1], 
  ChileArgentina.CondCAViaR.10.Est$Joint$par[2], ChileArgentina.CondCAViaR.10.EstSE[2,1], 
  ChileArgentina.CondCAViaR.10.Est$Joint$par[3], ChileArgentina.CondCAViaR.10.EstSE[3,1],
  "Chile-Mexico", ChileMexico.CondCAViaR.10.Est$Joint$par[1], ChileMexico.CondCAViaR.10.EstSE[1,1], 
  ChileMexico.CondCAViaR.10.Est$Joint$par[2], ChileMexico.CondCAViaR.10.EstSE[2,1], 
  ChileMexico.CondCAViaR.10.Est$Joint$par[3], ChileMexico.CondCAViaR.10.EstSE[3,1], 
  "Argentina-Mexico", ArgentinaMexico.CondCAViaR.10.Est$Joint$par[1], ArgentinaMexico.CondCAViaR.10.EstSE[1,1], 
  ArgentinaMexico.CondCAViaR.10.Est$Joint$par[2], ArgentinaMexico.CondCAViaR.10.EstSE[2,1], 
  ArgentinaMexico.CondCAViaR.10.Est$Joint$par[3], ArgentinaMexico.CondCAViaR.10.EstSE[3,1]
),7,7)

TablePrint(c(
  "\\hline $\\tau = 0.10$", "$\\nu_{M0}$", " ", "$\\nu_{M1}$", " ", "$\\nu_{M2}$", " ",
  "Brazil-Chile", BrazilChile.CondCAViaR.10.Est$Marg$par[1], BrazilChile.CondCAViaR.10.EstSE[1,2], 
  BrazilChile.CondCAViaR.10.Est$Marg$par[2], BrazilChile.CondCAViaR.10.EstSE[2,2], 
  BrazilChile.CondCAViaR.10.Est$Marg$par[3], BrazilChile.CondCAViaR.10.EstSE[3,2], 
  "Brazil-Argentina", BrazilArgentina.CondCAViaR.10.Est$Marg$par[1], BrazilArgentina.CondCAViaR.10.EstSE[1,2], 
  BrazilArgentina.CondCAViaR.10.Est$Marg$par[2], BrazilArgentina.CondCAViaR.10.EstSE[2,2], 
  BrazilArgentina.CondCAViaR.10.Est$Marg$par[3], BrazilArgentina.CondCAViaR.10.EstSE[3,2],
  "Brazil-Mexico", BrazilMexico.CondCAViaR.10.Est$Marg$par[1], BrazilMexico.CondCAViaR.10.EstSE[1,2], 
  BrazilMexico.CondCAViaR.10.Est$Marg$par[2], BrazilMexico.CondCAViaR.10.EstSE[2,2], 
  BrazilMexico.CondCAViaR.10.Est$Marg$par[3], BrazilMexico.CondCAViaR.10.EstSE[3,2], 
  "Chile-Argentina", ChileArgentina.CondCAViaR.10.Est$Marg$par[1], ChileArgentina.CondCAViaR.10.EstSE[1,2], 
  ChileArgentina.CondCAViaR.10.Est$Marg$par[2], ChileArgentina.CondCAViaR.10.EstSE[2,2], 
  ChileArgentina.CondCAViaR.10.Est$Marg$par[3], ChileArgentina.CondCAViaR.10.EstSE[3,2],
  "Chile-Mexico", ChileMexico.CondCAViaR.10.Est$Marg$par[1], ChileMexico.CondCAViaR.10.EstSE[1,2], 
  ChileMexico.CondCAViaR.10.Est$Marg$par[2], ChileMexico.CondCAViaR.10.EstSE[2,2], 
  ChileMexico.CondCAViaR.10.Est$Marg$par[3], ChileMexico.CondCAViaR.10.EstSE[3,2], 
  "Argentina-Mexico", ArgentinaMexico.CondCAViaR.10.Est$Marg$par[1], ArgentinaMexico.CondCAViaR.10.EstSE[1,2], 
  ArgentinaMexico.CondCAViaR.10.Est$Marg$par[2], ArgentinaMexico.CondCAViaR.10.EstSE[2,2], 
  ArgentinaMexico.CondCAViaR.10.Est$Marg$par[3], ArgentinaMexico.CondCAViaR.10.EstSE[3,2]
),7,7)
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
TablePrint(c(
  "Brazil-Chile", " ", " ", "Brazil-Argentina", " ", " ", "Brazil-Mexico", " ", " ",
  "Chile-Argentina", " ", " ", "Chile-Mexico", " ", " ", "Argentina-Mexico", " ", " ",
  "Logit", "Empirical Copula", "Student-t","Logit", "Empirical Copula", "Student-t",
  "Logit", "Empirical Copula", "Student-t","Logit", "Empirical Copula", "Student-t",
  "Logit", "Empirical Copula", "Student-t","Logit", "Empirical Copula", "Student-t",
  BrazilChile.CondCAViaR.MSE.05[1,5], BrazilChile.CondEmp.MSE.05[1,5], BrazilChile.All.MSE.05[1,5],
  BrazilArgentina.CondCAViaR.MSE.05[1,5], BrazilArgentina.CondEmp.MSE.05[1,5], BrazilArgentina.All.MSE.05[1,5],
  BrazilMexico.CondCAViaR.MSE.05[1,5], BrazilMexico.CondEmp.MSE.05[1,5], BrazilMexico.All.MSE.05[1,5],
  ChileArgentina.CondCAViaR.MSE.05[1,5], ChileArgentina.CondEmp.MSE.05[1,5], ChileArgentina.All.MSE.05[1,5],
  ChileMexico.CondCAViaR.MSE.05[1,5], ChileMexico.CondEmp.MSE.05[1,5], ChileMexico.All.MSE.05[1,5],
  ArgentinaMexico.CondCAViaR.MSE.05[1,5], ArgentinaMexico.CondEmp.MSE.05[1,5], ArgentinaMexico.All.MSE.05[1,5],
  
  BrazilChile.CondCAViaR.MSE.10[1,5], BrazilChile.CondEmp.MSE.10[1,5], BrazilChile.All.MSE.10[1,5],
  BrazilArgentina.CondCAViaR.MSE.10[1,5], BrazilArgentina.CondEmp.MSE.10[1,5], BrazilArgentina.All.MSE.10[1,5],
  BrazilMexico.CondCAViaR.MSE.10[1,5], BrazilMexico.CondEmp.MSE.10[1,5], BrazilMexico.All.MSE.10[1,5],
  ChileArgentina.CondCAViaR.MSE.10[1,5], ChileArgentina.CondEmp.MSE.10[1,5], ChileArgentina.All.MSE.10[1,5],
  ChileMexico.CondCAViaR.MSE.10[1,5], ChileMexico.CondEmp.MSE.10[1,5], ChileMexico.All.MSE.10[1,5],
  ArgentinaMexico.CondCAViaR.MSE.10[1,5], ArgentinaMexico.CondEmp.MSE.10[1,5], ArgentinaMexico.All.MSE.10[1,5]
), 18, 4)


















