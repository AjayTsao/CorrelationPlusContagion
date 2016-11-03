################################################################################################################################
################################################################################################################################
######################## STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - t-COEXCEEDENCE MSE #######################
################################################################################################################################
################################################################################################################################

tPW.Int <- function(data, tCopRho, tCopDf){
  prod <- dCopula(data, tCopula(param = (tCopRho/100000), df = (tCopDf/100000), dim = 2)) 
  return(prod)
}
###########

###########
###########
tCop.PairwiseMSE <- function(data, tCopRho, tCopDf, tau){
  MSE <- matrix(0:0, nrow(data), 5)
  MSE[,1] <- data[,1]
  prob <- vegas(ndim = 2, ncomp = 1, integrand = tPW.Int, tCopRho = tCopRho, tCopDf = tCopDf,
                lower = c(0,0), upper = c(tau,tau), nstart = 125000, max.eval = 125000)$value / tau
  for (i in 1:nrow(data)){
    if (data[i,3] < tau){
      MSE[i,2] <- prob
      MSE[i,3] <- as.numeric(data[i,2] < tau)
      MSE[i,4] <- (MSE[i,3] - MSE[i,2]) ^ 2
    }
  }
  MSE[1,5] <- (sum(MSE[,4])) / sum(data[,3] < tau)
  return(MSE)
}
###########
###########
CAViaR.PairwiseMSE <- function(data, prob, tau){
  MSE <- matrix(0:0, nrow(data), 5)
  MSE[,1] <- data[,1]
  for (i in 1:nrow(data)){
    if (data[i,3] < tau){
      MSE[i,2] <- prob
      MSE[i,3] <- as.numeric(data[i,2] < tau)
      MSE[i,4] <- (MSE[i,3] - MSE[i,2]) ^ 2
    }
  }
  MSE[1,5] <- (sum(MSE[,4])) / sum(data[,3] < tau)
  return(MSE)
}
###########
###########
tMulti.Cop <- ellipCopula(family = "t", param = (t.CappJoint$par[21:26]/100000), dim = 4, 
                          dispstr = "un", df = (t.CappJoint$par[27]/100000))
###########
###########
tMulti.Int <- function(data){
  prod <- dCopula(u = data, copula = tMulti.Cop)
}
###########
###########
tCop.Multi.MSE <- function(data, tau, CX1, CX2, Cond1, Cond2){
  MSE <- matrix(0:0, nrow(data), 5)
  Jointdata <- matrix(0:0, nrow(data), 4)
  #
  Jointdata[,CX1] <- rep(tau, nrow(data))
  Jointdata[,CX2] <- rep(tau, nrow(data))
  Jointdata[,Cond1] <- data[,(Cond1+1)]
  Jointdata[,Cond2] <- data[,(Cond2+1)]
  #
  Margdata <- matrix(0:0, nrow(data), 4)
  #
  Margdata[,CX1] <- rep(1, nrow(data))
  Margdata[,CX2] <- rep(tau, nrow(data))
  Margdata[,Cond1] <- data[,(Cond1+1)]
  Margdata[,Cond2] <- data[,(Cond2+1)]
  #
  MSE[,1] <- data[,1]
  for (i in 1:nrow(data)){
    if (data[i,(CX2+1)] < tau){
      temp1 <- vegas(ndim = 4, ncomp = 1, integrand = tMulti.Int, lower = c(0,0,0,0), upper = c(Jointdata[i,]), 
                                  min.eval = 100000, max.eval = 150000, nstart = 50000)$value
      #
      temp2 <- vegas(ndim = 4, ncomp = 1, integrand = tMulti.Int, lower = c(0,0,0,0), upper = c(Margdata[i,]), 
                                   min.eval = 100000, max.eval = 150000, nstart = 50000)$value
      MSE[i,2] <- temp1 / temp2

        
      if (MSE[i,2] > 1){
        temp1 <- vegas(ndim = 4, ncomp = 1, integrand = tMulti.Int, lower = c(0,0,0,0), upper = c(Jointdata[i,]),
                       min.eval = 500000, max.eval = 750000, nstart = 50000)$value
        temp2 <- vegas(ndim = 4, ncomp = 1, integrand = tMulti.Int, lower = c(0,0,0,0), upper = c(Margdata[i,]),
                       min.eval = 500000, max.eval = 750000, nstart = 50000)$value
        MSE[i,2] <- temp1 / temp2
      }
      MSE[i,3] <- as.numeric(data[i,(CX1+1)] < tau)
      MSE[i,4] <- (MSE[i,3] - MSE[i,2]) ^ 2
    }
  }
  MSE[1,5] <- (sum(MSE[,4])) / sum(data[,CX2] < tau)
  return(MSE)
}
###########
###########
###########
###########
Empirical.Pairwise.MSE <- function(data, trainingdata, tau){
  MSE <- matrix(0:0, nrow(data), 5)
  MSE[,1] <- data[,1]
  indictemp1 <- as.numeric(trainingdata[,2] < tau)
  indictemp2 <- as.numeric(trainingdata[,3] < tau)
  indictemp3 <- indictemp1 * indictemp2
  prob <- mean(indictemp3) / mean(indictemp2)
  #
  for (i in 1:nrow(data)){
    if (data[i,3] < tau){
      MSE[i,2] <- prob
      MSE[i,3] <- as.numeric(data[i,2] < tau)
      MSE[i,4] <- (MSE[i,3] - MSE[i,2]) ^ 2
    }
  }
  MSE[1,5] <- (sum(MSE[,4])) / sum(data[,3] < tau)
  return(MSE)
}
###########
###########
###########
###########
BrazilChile.PW.t.01 <- tCop.PairwiseMSE(All.cappU[,c(1,2,3)], t.CappJoint$par[21], t.CappJoint$par[27], tau = 0.01)
#
BrazilArgentina.PW.t.01 <- tCop.PairwiseMSE(All.cappU[,c(1,2,4)], t.CappJoint$par[22], t.CappJoint$par[27], tau = 0.01)
#
BrazilMexico.PW.t.01 <- tCop.PairwiseMSE(All.cappU[,c(1,2,5)], t.CappJoint$par[23], t.CappJoint$par[27], tau = 0.01)
#
ChileArgentina.PW.t.01 <- tCop.PairwiseMSE(All.cappU[,c(1,3,4)], t.CappJoint$par[24], t.CappJoint$par[27], tau = 0.01)
#
ChileMexico.PW.t.01 <- tCop.PairwiseMSE(All.cappU[,c(1,3,5)], t.CappJoint$par[25], t.CappJoint$par[27], tau = 0.01)
#
ArgentinaMexico.PW.t.01 <- tCop.PairwiseMSE(All.cappU[,c(1,4,5)], t.CappJoint$par[26], t.CappJoint$par[27], tau = 0.01)
###########
###########
BrazilChile.PW.t.05 <- tCop.PairwiseMSE(All.cappU[,c(1,2,3)], t.CappJoint$par[21], t.CappJoint$par[27], tau = 0.05)
#
BrazilArgentina.PW.t.05 <- tCop.PairwiseMSE(All.cappU[,c(1,2,4)], t.CappJoint$par[22], t.CappJoint$par[27], tau = 0.05)
#
BrazilMexico.PW.t.05 <- tCop.PairwiseMSE(All.cappU[,c(1,2,5)], t.CappJoint$par[23], t.CappJoint$par[27], tau = 0.05)
#
ChileArgentina.PW.t.05 <- tCop.PairwiseMSE(All.cappU[,c(1,3,4)], t.CappJoint$par[24], t.CappJoint$par[27], tau = 0.05)
#
ChileMexico.PW.t.05 <- tCop.PairwiseMSE(All.cappU[,c(1,3,5)], t.CappJoint$par[25], t.CappJoint$par[27], tau = 0.05)
#
ArgentinaMexico.PW.t.05 <- tCop.PairwiseMSE(All.cappU[,c(1,4,5)], t.CappJoint$par[26], t.CappJoint$par[27], tau = 0.05)
###########
###########
BrazilChile.PW.t.10 <- tCop.PairwiseMSE(All.cappU[,c(1,2,3)], t.CappJoint$par[21], t.CappJoint$par[27], tau = 0.10)
#
BrazilArgentina.PW.t.10 <- tCop.PairwiseMSE(All.cappU[,c(1,2,4)], t.CappJoint$par[22], t.CappJoint$par[27], tau = 0.10)
#
BrazilMexico.PW.t.10 <- tCop.PairwiseMSE(All.cappU[,c(1,2,5)], t.CappJoint$par[23], t.CappJoint$par[27], tau = 0.10)
#
ChileArgentina.PW.t.10 <- tCop.PairwiseMSE(All.cappU[,c(1,3,4)], t.CappJoint$par[24], t.CappJoint$par[27], tau = 0.10)
#
ChileMexico.PW.t.10 <- tCop.PairwiseMSE(All.cappU[,c(1,3,5)], t.CappJoint$par[25], t.CappJoint$par[27], tau = 0.10)
#
ArgentinaMexico.PW.t.10 <- tCop.PairwiseMSE(All.cappU[,c(1,4,5)], t.CappJoint$par[26], t.CappJoint$par[27], tau = 0.10)
###########
###########
###########
###########
BrazilChile.PW.CAViaR.01 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,2,3)], prob = BrazilChile.Coex[1,10], 0.01)
#
BrazilArgentina.PW.CAViaR.01 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,2,4)], prob = BrazilArgentina.Coex[1,10], 0.01)
#
BrazilMexico.PW.CAViaR.01 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,2,5)], prob = BrazilMexico.Coex[1,10], 0.01)
#
ChileArgentina.PW.CAViaR.01 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,3,4)], prob = ChileArgentina.Coex[1,10], 0.01)
#
ChileMexico.PW.CAViaR.01 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,3,5)], prob = ChileMexico.Coex[1,10], 0.01)
#
ArgentinaMexico.PW.CAViaR.01 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,4,5)], prob = ArgentinaMexico.Coex[1,10], 0.01)
###########
###########
BrazilChile.PW.CAViaR.05 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,2,3)], prob = BrazilChile.Coex[1,50], 0.05)
#
BrazilArgentina.PW.CAViaR.05 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,2,4)], prob = BrazilArgentina.Coex[1,50], 0.05)
#
BrazilMexico.PW.CAViaR.05 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,2,5)], prob = BrazilMexico.Coex[1,50], 0.05)
#
ChileArgentina.PW.CAViaR.05 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,3,4)], prob = ChileArgentina.Coex[1,50], 0.05)
#
ChileMexico.PW.CAViaR.05 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,3,5)], prob = ChileMexico.Coex[1,50], 0.05)
#
ArgentinaMexico.PW.CAViaR.05 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,4,5)], prob = ArgentinaMexico.Coex[1,50], 0.05)
###########
###########
BrazilChile.PW.CAViaR.10 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,2,3)], prob = BrazilChile.Coex[1,100], 0.10)
#
BrazilArgentina.PW.CAViaR.10 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,2,4)], prob = BrazilArgentina.Coex[1,100], 0.10)
#
BrazilMexico.PW.CAViaR.10 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,2,5)], prob = BrazilMexico.Coex[1,100], 0.10)
#
ChileArgentina.PW.CAViaR.10 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,3,4)], prob = ChileArgentina.Coex[1,100], 0.10)
#
ChileMexico.PW.CAViaR.10 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,3,5)], prob = ChileMexico.Coex[1,100], 0.10)
#
ArgentinaMexico.PW.CAViaR.10 <- CAViaR.PairwiseMSE(data = All.cappU[,c(1,4,5)], prob = ArgentinaMexico.Coex[1,100], 0.10)
###########
###########
###########
###########
BrazilChile.PW.Emp.01 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,2,3)], trainingdata = BrazilChile.cappU, 0.01)
#
BrazilArgentina.PW.Emp.01 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,2,4)], trainingdata = BrazilArgentina.cappU, 0.01)
#
BrazilMexico.PW.Emp.01 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,2,5)], trainingdata = BrazilMexico.cappU, 0.01)
#
ChileArgentina.PW.Emp.01 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,3,4)], trainingdata = ChileArgentina.cappU, 0.01)
#
ChileMexico.PW.Emp.01 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,3,5)], trainingdata = ChileMexico.cappU, 0.01)
#
ArgentinaMexico.PW.Emp.01 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,4,5)], trainingdata = ArgentinaMexico.cappU, 0.01)
###########
###########
BrazilChile.PW.Emp.05 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,2,3)], trainingdata = BrazilChile.cappU, 0.05)
#
BrazilArgentina.PW.Emp.05 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,2,4)], trainingdata = BrazilArgentina.cappU, 0.05)
#
BrazilMexico.PW.Emp.05 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,2,5)], trainingdata = BrazilMexico.cappU, 0.05)
#
ChileArgentina.PW.Emp.05 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,3,4)], trainingdata = ChileArgentina.cappU, 0.05)
#
ChileMexico.PW.Emp.05 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,3,5)], trainingdata = ChileMexico.cappU, 0.05)
#
ArgentinaMexico.PW.Emp.05 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,4,5)], trainingdata = ArgentinaMexico.cappU, 0.05)
###########
###########
BrazilChile.PW.Emp.10 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,2,3)], trainingdata = BrazilChile.cappU, 0.10)
#
BrazilArgentina.PW.Emp.10 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,2,4)], trainingdata = BrazilArgentina.cappU, 0.10)
#
BrazilMexico.PW.Emp.10 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,2,5)], trainingdata = BrazilMexico.cappU, 0.10)
#
ChileArgentina.PW.Emp.10 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,3,4)], trainingdata = ChileArgentina.cappU, 0.10)
#
ChileMexico.PW.Emp.10 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,3,5)], trainingdata = ChileMexico.cappU, 0.10)
#
ArgentinaMexico.PW.Emp.10 <- Empirical.Pairwise.MSE(data = All.cappU[,c(1,4,5)], trainingdata = ArgentinaMexico.cappU, 0.10)
###########
###########
###########
###########
# BrazilChile.All.MSE.01 <- tCop.Multi.MSE(data = All.cappU, tau = 0.01, CX1 = 1, CX2 = 2, Cond1 = 3, Cond2 = 4)
# #
# BrazilArgentina.All.MSE.01 <- tCop.Multi.MSE(data = All.cappU, tau = 0.01, CX1 = 1, CX2 = 3, Cond1 = 2, Cond2 = 4)
# #
# BrazilMexico.All.MSE.01 <- tCop.Multi.MSE(data = All.cappU, tau = 0.01, CX1 = 1, CX2 = 4, Cond1 = 2, Cond2 = 3)
# #
# ChileArgentina.All.MSE.01 <- tCop.Multi.MSE(data = All.cappU, tau = 0.01, CX1 = 2, CX2 = 3, Cond1 = 1, Cond2 = 4)
# #
# ChileMexico.All.MSE.01 <- tCop.Multi.MSE(data = All.cappU, tau = 0.01, CX1 = 2, CX2 = 4, Cond1 = 1, Cond2 = 3)
# #
# ArgentinaMexico.All.MSE.01 <- tCop.Multi.MSE(data = All.cappU, tau = 0.01, CX1 = 3, CX2 = 4, Cond1 = 1, Cond2 = 2)
# ###########
# ###########
# BrazilChile.All.MSE.05 <- tCop.Multi.MSE(data = All.cappU, tau = 0.05, CX1 = 1, CX2 = 2, Cond1 = 3, Cond2 = 4)
# #
# BrazilArgentina.All.MSE.05 <- tCop.Multi.MSE(data = All.cappU, tau = 0.05, CX1 = 1, CX2 = 3, Cond1 = 2, Cond2 = 4)
# #
# BrazilMexico.All.MSE.05 <- tCop.Multi.MSE(data = All.cappU, tau = 0.05, CX1 = 1, CX2 = 4, Cond1 = 2, Cond2 = 3)
#                                           
# ChileArgentina.All.MSE.05 <- tCop.Multi.MSE(data = All.cappU, tau = 0.05, CX1 = 2, CX2 = 3, Cond1 = 1, Cond2 = 4)
# #
# ChileMexico.All.MSE.05 <- tCop.Multi.MSE(data = All.cappU, tau = 0.05, CX1 = 2, CX2 = 4, Cond1 = 1, Cond2 = 3)
# #
# ArgentinaMexico.All.MSE.05 <- tCop.Multi.MSE(data = All.cappU, tau = 0.05, CX1 = 3, CX2 = 4, Cond1 = 1, Cond2 = 2)
# save.image()
###########
###########
# BrazilChile.All.MSE.10 <- tCop.Multi.MSE(data = All.cappU, tau = 0.10, CX1 = 1, CX2 = 2, Cond1 = 3, Cond2 = 4)
# #
# BrazilArgentina.All.MSE.10 <- tCop.Multi.MSE(data = All.cappU, tau = 0.10, CX1 = 1, CX2 = 3, Cond1 = 2, Cond2 = 4)
# #
# BrazilMexico.All.MSE.10 <- tCop.Multi.MSE(data = All.cappU, tau = 0.10, CX1 = 1, CX2 = 4, Cond1 = 2, Cond2 = 3)
# 
# ChileArgentina.All.MSE.10 <- tCop.Multi.MSE(data = All.cappU, tau = 0.10, CX1 = 2, CX2 = 3, Cond1 = 1, Cond2 = 4)
# #
# ChileMexico.All.MSE.10 <- tCop.Multi.MSE(data = All.cappU, tau = 0.10, CX1 = 2, CX2 = 4, Cond1 = 1, Cond2 = 3)
# #
# ArgentinaMexico.All.MSE.10 <- tCop.Multi.MSE(data = All.cappU, tau = 0.10, CX1 = 3, CX2 = 4, Cond1 = 1, Cond2 = 2)
# save.image()
# ###########
###########
###########
###########
TablePrint(c(
  "\\hline \\textbf{MSE of Prediction}", "\\textbf{of Coexceedences}",  "\\hline Brazil Chile", " ", " ", " ", "\\hline Brazil Argentina", " ", " ", " ",
  "\\hline Brazil Mexico", " ", " ", " ", "\\hline Chile Argentina", " ", " ", " ", "\\hline Chile Mexico", " ", " ", " ", "\\hline Argentina Mexico", " ", " ", " ",
  " ", " ", "t Copula", "CAViaR", "Empirical", "Conditional t Copula", "t Copula", "CAViaR", "Empirical", "Conditional t Copula", 
  "t Copula", "CAViaR", "Empirical", "Conditional t Copula", 
  "t Copula", "CAViaR", "Empirical", "Conditional t Copula",  "t Copula", "CAViaR", "Empirical", "Conditional t Copula", 
  "t Copula", "CAViaR", "Empirical","Conditional t Copula", 
  "Coexceedences", "$\\tau = 0.01$", BrazilChile.PW.t.01[1,5], BrazilChile.PW.CAViaR.01[1,5], BrazilChile.PW.Emp.01[1,5], BrazilChile.All.MSE.01[1,5],
  BrazilArgentina.PW.t.01[1,5], BrazilArgentina.PW.CAViaR.01[1,5], BrazilArgentina.PW.Emp.01[1,5], BrazilArgentina.All.MSE.01[1,5],
  BrazilMexico.PW.t.01[1,5], BrazilMexico.PW.CAViaR.01[1,5], BrazilMexico.PW.Emp.01[1,5], BrazilMexico.All.MSE.01[1,5],
  ChileArgentina.PW.t.01[1,5], ChileArgentina.PW.CAViaR.01[1,5], ChileArgentina.PW.Emp.01[1,5], ChileArgentina.All.MSE.01[1,5],
  ChileMexico.PW.t.01[1,5], ChileMexico.PW.CAViaR.01[1,5], ChileMexico.PW.Emp.01[1,5], ChileMexico.All.MSE.01[1,5],
  ArgentinaMexico.PW.t.01[1,5], ArgentinaMexico.PW.CAViaR.01[1,5], ArgentinaMexico.PW.Emp.01[1,5], ArgentinaMexico.All.MSE.01[1,5],
  
  "Coexceedences", "$\\tau = 0.05$", BrazilChile.PW.t.05[1,5], BrazilChile.PW.CAViaR.05[1,5], BrazilChile.PW.Emp.05[1,5], " ",
  BrazilArgentina.PW.t.05[1,5], BrazilArgentina.PW.CAViaR.05[1,5], BrazilArgentina.PW.Emp.05[1,5], " ",
  BrazilMexico.PW.t.05[1,5], BrazilMexico.PW.CAViaR.05[1,5], BrazilMexico.PW.Emp.05[1,5], " ",
  ChileArgentina.PW.t.05[1,5], ChileArgentina.PW.CAViaR.05[1,5], ChileArgentina.PW.Emp.05[1,5], " ",
  ChileMexico.PW.t.05[1,5], ChileMexico.PW.CAViaR.05[1,5], ChileMexico.PW.Emp.05[1,5], " ",
  ArgentinaMexico.PW.t.05[1,5], ArgentinaMexico.PW.CAViaR.05[1,5], ArgentinaMexico.PW.Emp.05[1,5], " ",
  
  "Coexceedences", "$\\tau = 0.10$", BrazilChile.PW.t.10[1,5], BrazilChile.PW.CAViaR.10[1,5], BrazilChile.PW.Emp.10[1,5], " ",
  BrazilArgentina.PW.t.10[1,5], BrazilArgentina.PW.CAViaR.10[1,5], BrazilArgentina.PW.Emp.10[1,5], " ",
  BrazilMexico.PW.t.10[1,5], BrazilMexico.PW.CAViaR.10[1,5], BrazilMexico.PW.Emp.10[1,5], " ",
  ChileArgentina.PW.t.10[1,5], ChileArgentina.PW.CAViaR.10[1,5], ChileArgentina.PW.Emp.10[1,5], " ",
  ChileMexico.PW.t.10[1,5], ChileMexico.PW.CAViaR.10[1,5], ChileMexico.PW.Emp.10[1,5], " ",
  ArgentinaMexico.PW.t.10[1,5], ArgentinaMexico.PW.CAViaR.10[1,5], ArgentinaMexico.PW.Emp.10[1,5], " "
), 26, 5)


TablePrint(c(
  "\\hline \\textbf{MSE of Prediction}", "\\textbf{of Coexceedences}",  "\\hline Brazil Chile", " ", " ",
  "\\hline Brazil Argentina", " ", " ","\\hline Brazil Mexico", " ", " ","\\hline Chile Argentina", " ", " ",
  "\\hline Chile Mexico", " ", " ","\\hline Argentina Mexico", " ", " ",
  " ", " ", "Logit Regression", " Conditional t-Copula", "Conditional Empirical",
  "Logit Regression", " Conditional t-Copula", "Conditional Empirical",
  "Logit Regression", " Conditional t-Copula", "Conditional Empirical",
  "Logit Regression", " Conditional t-Copula", "Conditional Empirical",
  "Logit Regression", " Conditional t-Copula", "Conditional Empirical","Logit Regression", " Conditional t-Copula", "Conditional Empirical",
  
  "Coexceedences", "$\\tau = 0.05$", BrazilChile.CondCAViaR.MSE.05[1,5], BrazilChile.All.MSE.05[1,5], BrazilChile.CondEmp.MSE.05[1,5],
  BrazilArgentina.CondCAViaR.MSE.05[1,5], BrazilArgentina.All.MSE.05[1,5], BrazilArgentina.CondEmp.MSE.05[1,5],
  BrazilMexico.CondCAViaR.MSE.05[1,5], BrazilMexico.All.MSE.05[1,5], BrazilMexico.CondEmp.MSE.05[1,5],
  ChileArgentina.CondCAViaR.MSE.05[1,5], ChileArgentina.All.MSE.05[1,5], ChileArgentina.CondEmp.MSE.05[1,5],
  ChileMexico.CondCAViaR.MSE.05[1,5], ChileMexico.All.MSE.05[1,5], ChileMexico.CondEmp.MSE.05[1,5],
  ArgentinaMexico.CondCAViaR.MSE.05[1,5], ArgentinaMexico.All.MSE.05[1,5], ArgentinaMexico.CondEmp.MSE.05[1,5],
  
  "Coexceedences", "$\\tau = 0.10$", BrazilChile.CondCAViaR.MSE.10[1,5],  " ", BrazilChile.CondEmp.MSE.10[1,5],
  BrazilArgentina.CondCAViaR.MSE.10[1,5],  " ", BrazilArgentina.CondEmp.MSE.10[1,5],
  BrazilMexico.CondCAViaR.MSE.10[1,5],  " ", BrazilMexico.CondEmp.MSE.10[1,5],
  ChileArgentina.CondCAViaR.MSE.10[1,5],  " ", ChileArgentina.CondEmp.MSE.10[1,5],
  ChileMexico.CondCAViaR.MSE.10[1,5],  " ", ChileMexico.CondEmp.MSE.10[1,5],
  ArgentinaMexico.CondCAViaR.MSE.10[1,5],  " ", ArgentinaMexico.CondEmp.MSE.10[1,5]
),20,4)







































