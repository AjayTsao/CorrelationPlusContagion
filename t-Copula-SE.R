################################################################################################################################
################################################################################################################################
##################### STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - t-Copula Standard Errors ####################
################################################################################################################################
################################################################################################################################
setwd("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/MLE and CAViaR/")
######
######
library(boot)
WeirdSeries <- c((1:10 * 100), (999 + (1:90)*10))
#
tCopStop <- ellipCopula(family = "t", dim = 4, dispstr = "un")
#
tCopulaSE.LL.Function <- function(param, data){
  LL1 <- cappgarchLL(param[1:5], data[,c(1,2)])
  U1 <- cappgarch.PIT(param[1:5], data[,c(1,2)])
  #
  LL2 <- cappgarchLL(param[6:10], data[,c(1,3)])
  U2 <- cappgarch.PIT(param[6:10], data[,c(1,3)])
  #
  LL3 <- cappgarchLL(param[11:15], data[,c(1,4)])
  U3 <- cappgarch.PIT(param[11:15], data[,c(1,4)])
  #
  LL4 <- cappgarchLL(param[16:20], data[,c(1,5)])
  U4 <- cappgarch.PIT(param[16:20], data[,c(1,5)])
  #
  U12 <- merge(U1, U2, by = 1)
  U34 <- merge(U3, U4, by = 1)
  UAll <- merge(U12, U34, by = 1)
  #
  C1 <- UAll[,2]
  C2 <- UAll[,3]
  C3 <- UAll[,4]
  C4 <- UAll[,5]
  #
  for (i in 1:length(C1)){
    if (C1[i] < 0.000001) C1[i] <- 0.000001
    if (C2[i] < 0.000001) C2[i] <- 0.000001
    if (C3[i] < 0.000001) C3[i] <- 0.000001
    if (C4[i] < 0.000001) C4[i] <- 0.000001
    #
    if (C1[i] > 0.999999) C1[i] <- 0.999999
    if (C2[i] > 0.999999) C2[i] <- 0.999999
    if (C3[i] > 0.999999) C3[i] <- 0.999999
    if (C4[i] > 0.999999) C4[i] <- 0.999999
  }
  #
  friendship <- do.call(cbind, list(C1, C2, C3, C4))
  #
  tCop <- ellipCopula()
  #
  tLL <- loglikCopula(param = (param[21:27]/100000), u = friendship, copula = tCopStop)
  #
  retLL <- tLL + LL1 + LL2 + LL3 + LL4
  #
  if(is.infinite(retLL)) retLL <- 0
  #
  return(retLL)
}
#
tCopulaSE.Bootstrap.Function <- function(data, inds, param){
  results <- optim(par = param, fn = tCopulaSE.LL.Function, data = data[inds,], 
                   control = list(maxit = 300, trace = T, fnscale = (-1)))$par
  ########
  tSeq1 <- seq(from = 0, to = 0.5, length = 50)
  tSeq2 <- seq(from = 0.5, to = 0, length = 50)
  tSeq3 <- seq(from = 0, to = 1, length = 100)
  #########
  coex <- rep(0, (length(tSeq1) + length(tSeq2) + length(tSeq3)))
  ##########
  coex[1:50] <- pCopula(u=cbind(tSeq1, tSeq1), copula = ellipCopula(param = (results[21]/100000), df = floor((results[27]/100000)), 
                                                                     dim = 2, dispstr = "un")) / tSeq1
  coex[51:100] <- pCopula(u=cbind(tSeq2, tSeq2), copula = ellipCopula(param = results[21]/100000, df = floor((results[27]/100000)), 
                            dim = 2, dispstr = "un")) / tSeq2
  coex[101:200] <- pCopula(u=cbind(tSeq3, tSeq3), copula = ellipCopula(param = (results[21]/100000), df = floor((results[27]/100000)), 
                                                                         dim = 2, dispstr = "un"))
  #
  coex[201:250] <- pCopula(u=cbind(tSeq1, tSeq1), copula = ellipCopula(param = (results[22]/100000), df = floor((results[27]/100000)), 
                                                                     dim = 2, dispstr = "un")) / tSeq1
  coex[251:300] <- pCopula(u=cbind(tSeq2, tSeq2), copula = ellipCopula(param = results[22]/100000, df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un")) / tSeq2
  coex[301:400] <- pCopula(u=cbind(tSeq3, tSeq3), copula = ellipCopula(param = (results[22]/100000), df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un"))
  #
  coex[401:450] <- pCopula(u=cbind(tSeq1, tSeq1), copula = ellipCopula(param = (results[23]/100000), df = floor((results[27]/100000)), 
                                                                     dim = 2, dispstr = "un")) / tSeq1
  coex[451:500] <- pCopula(u=cbind(tSeq2, tSeq2), copula = ellipCopula(param = results[23]/100000, df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un")) / tSeq2
  coex[501:600] <- pCopula(u=cbind(tSeq3, tSeq3), copula = ellipCopula(param = (results[23]/100000), df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un"))
  #
  coex[601:650] <- pCopula(u=cbind(tSeq1, tSeq1), copula = ellipCopula(param = (results[24]/100000), df = floor((results[27]/100000)), 
                                                                     dim = 2, dispstr = "un")) / tSeq1
  coex[651:700] <- pCopula(u=cbind(tSeq2, tSeq2), copula = ellipCopula(param = results[24]/100000, df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un")) / tSeq2
  coex[701:800] <- pCopula(u=cbind(tSeq3, tSeq3), copula = ellipCopula(param = (results[24]/100000), df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un"))
  #
  coex[801:850] <- pCopula(u=cbind(tSeq1, tSeq1), copula = ellipCopula(param = (results[25]/100000), df = floor((results[27]/100000)), 
                                                                     dim = 2, dispstr = "un")) / tSeq1
  coex[851:900] <- pCopula(u=cbind(tSeq2, tSeq2), copula = ellipCopula(param = results[25]/100000, df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un")) / tSeq2
  coex[901:1000] <- pCopula(u=cbind(tSeq3, tSeq3), copula = ellipCopula(param = (results[25]/100000), df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un"))
  #
  coex[1001:1050] <- pCopula(u=cbind(tSeq1, tSeq1), copula = ellipCopula(param = (results[26]/100000), df = floor((results[27]/100000)), 
                                                                     dim = 2, dispstr = "un")) / tSeq1
  coex[1051:1100] <- pCopula(u=cbind(tSeq2, tSeq2), copula = ellipCopula(param = results[26]/100000, df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un")) / tSeq2
  coex[1101:1200] <- pCopula(u=cbind(tSeq3, tSeq3), copula = ellipCopula(param = (results[26]/100000), df = floor((results[27]/100000)), 
                                                                       dim = 2, dispstr = "un"))
  #
  BrazilU <- cappgarch.PIT(results[1:5], data = BrazilReturns[inds,])
  ChileU <- cappgarch.PIT(results[6:10], data = ChileReturns[inds,])
  ArgentinaU <- cappgarch.PIT(results[11:15], data = ArgentinaReturns[inds,])
  MexicoU <- cappgarch.PIT(results[16:20], data = MexicoReturns[inds,])
  #
  BrazilVhileU <- merge(BrazilU, ChileU, by = 1)
  argentinamexicoU <- merge(ArgentinaU, MexicoU, by = 1)
  MostU <- merge(brazilchileU, argentinamexicoU, by = 1)
  #
  BrazilChileEmpCopBS <- EmpiricalCopulaFunction(MostU[,c(1,2,3)])
  BrazilArgentinaEmpCopBS <- EmpiricalCopulaFunction(MostU[,c(1,2,4)])
  BrazilMexicoEmpCopBS <- EmpiricalCopulaFunction(MostU[,c(1,2,5)])
  ChileArgentinaEmpCopBS <- EmpiricalCopulaFunction(MostU[,c(1,3,4)])
  ChileMexicoEmpCopBS <- EmpiricalCopulaFunction(MostU[,c(1,3,5)])
  ArgentinaMexicoEmpCopBS <- EmpiricalCopulaFunction(MostU[,c(1,4,5)])
  #
  BrazilChileEmpCoexBS <- EmpiricalCoexFunction(MostU[,c(1,2,3)])
  BrazilArgentinaEmpCoexBS <- EmpiricalCoexFunction(MostU[,c(1,2,4)])
  BrazilMexicoEmpCoexBS <- EmpiricalCoexFunction(MostU[,c(1,2,5)])
  ChileArgentinaEmpCoexBS <- EmpiricalCoexFunction(MostU[,c(1,3,4)])
  ChileMexicoEmpCoexBS <- EmpiricalCoexFunction(MostU[,c(1,3,5)])
  ArgentinaMexicoEmpCoexBS <- EmpiricalCoexFunction(MostU[,c(1,4,5)])
  
  
  
  coex <- c(coex, BrazilChileEmpCopBS, BrazilArgentinaEmpCopBS, BrazilMexicoEmpCopBS, ChileArgentinaEmpCopBS, ChileMexicoEmpCopBS, 
            ArgentinaMexicoEmpCopBS, BrazilChileEmpCoexBS, BrazilArgentinaEmpCoexBS, BrazilMexicoEmpCoexBS, ChileArgentinaEmpCopBS,
            ChileMexicoEmpCoexBS, ArgentinaMexicoEmpCoexBS)
  return(coex)
}
########
########
# ########
# BrazilChile.Temp <- merge(BrazilReturns, ChileReturns, by = 1)
# ArgentinaMexico.Temp <- merge(ArgentinaReturns, MexicoReturns, by = 1)
# AllReturns <- merge(BrazilChile.Temp, ArgentinaMexico.Temp, by = 1)
# ########
# ########
# t.boot.Fx.Coex <- boot(data = AllReturns, statistic = tCopulaSE.Bootstrap.Function, R = 250,  stype = "i",
#      param = t.CappJoint$par)


######## BOOTSTRAPPED T-COPULA DIAGONAL SECTIONS
BrazilChile.BS.Diag <- matrix(0:0, 100, 250)
BrazilArgentina.BS.Diag <- matrix(0:0, 100, 250)
BrazilMexico.BS.Diag <- matrix(0:0, 100, 250)
ChileArgentina.BS.Diag <- matrix(0:0, 100, 250)
ChileMexico.BS.Diag <- matrix(0:0, 100, 250)
ChileArgentina.BS.Diag <- matrix(0:0, 100, 250)
#
BrazilChile.BS.Diag <- t(t.boot.Fx.Coex$t[,101:200])
BrazilArgentina.BS.Diag <- t(t.boot.Fx.Coex$t[,301:400])
BrazilMexico.BS.Diag <- t(t.boot.Fx.Coex$t[,501:600])
ChileArgentina.BS.Diag <- t(t.boot.Fx.Coex$t[,701:800])
ChileMexico.BS.Diag <- t(t.boot.Fx.Coex$t[,901:1000])
ArgentinaMexico.BS.Diag <- t(t.boot.Fx.Coex$t[,1101:1200])
#
######## BOOTSTRAPPED T-COPULA COEX DIAGONAL SECTIONS
BrazilChile.BS.Coex <- matrix(0:0, 100, 250)
BrazilArgentina.BS.Coex <- matrix(0:0, 100, 250)
BrazilMexico.BS.Coex <- matrix(0:0, 100, 250)
ChileArgentina.BS.Coex <- matrix(0:0, 100, 250)
ChileMexico.BS.Coex <- matrix(0:0, 100, 250)
ChileArgentina.BS.Coex <- matrix(0:0, 100, 250)
#
BrazilChile.BS.Coex <- t(t.boot.Fx.Coex$t[,1:100])
BrazilArgentina.BS.Coex <- t(t.boot.Fx.Coex$t[,201:300])
BrazilMexico.BS.Coex <- t(t.boot.Fx.Coex$t[,401:500])
ChileArgentina.BS.Coex <- t(t.boot.Fx.Coex$t[,601:700])
ChileMexico.BS.Coex <- t(t.boot.Fx.Coex$t[,801:900])
ArgentinaMexico.BS.Coex <- t(t.boot.Fx.Coex$t[,1001:1100])
#
for(i in 1:100){
  for(j in 1:250){
    if (is.na(BrazilChile.BS.Coex[i,j])) BrazilChile.BS.Coex[i,j] <- 0
    if (is.na(BrazilArgentina.BS.Coex[i,j])) BrazilArgentina.BS.Coex[i,j] <- 0
    if (is.na(BrazilMexico.BS.Coex[i,j])) BrazilMexico.BS.Coex[i,j] <- 0
    if (is.na(ChileArgentina.BS.Coex[i,j])) ChileArgentina.BS.Coex[i,j] <- 0
    if (is.na(ChileMexico.BS.Coex[i,j])) ChileMexico.BS.Coex[i,j] <- 0
    if (is.na(ArgentinaMexico.BS.Coex[i,j])) ArgentinaMexico.BS.Coex[i,j] <- 0
  }
}

######## BOOTSTRAPPED EMPIRICAL DIAGONAL SECTIONS
BrazilChile.BS.Emp.Diag <- matrix(0:0, 100, 250)
BrazilArgentina.BS.Emp.Diag <- matrix(0:0, 100, 250)
BrazilMexico.BS.Emp.Diag <- matrix(0:0, 100, 250)
ChileArgentina.BS.Emp.Diag <- matrix(0:0, 100, 250)
ChileMexico.BS.Emp.Diag <- matrix(0:0, 100, 250)
ChileArgentina.BS.Emp.Diag <- matrix(0:0, 100, 250)
#c((1:50 * 20), (501:550 * 2))
BrazilChile.BS.Emp.Diag <- t(t.boot.Fx.Coex$t[,1201:1300])
BrazilArgentina.BS.Emp.Diag <- t(t.boot.Fx.Coex$t[,1301:1400])
BrazilMexico.BS.Emp.Diag <- t(t.boot.Fx.Coex$t[,1401:1500])
ChileArgentina.BS.Emp.Diag <- t(t.boot.Fx.Coex$t[,1501:1600])
ChileMexico.BS.Emp.Diag <- t(t.boot.Fx.Coex$t[,1601:1700])
ArgentinaMexico.BS.Emp.Diag <- t(t.boot.Fx.Coex$t[,1701:1800])
#
######## BOOTSTRAPPED EMPIRICAL COEX SECTIONS
BrazilChile.BS.Emp.Coex <- matrix(0:0, 100, 250)
BrazilArgentina.BS.Emp.Coex <- matrix(0:0, 100, 250)
BrazilMexico.BS.Emp.Coex <- matrix(0:0, 100, 250)
ChileArgentina.BS.Emp.Coex <- matrix(0:0, 100, 250)
ChileMexico.BS.Emp.Coex <- matrix(0:0, 100, 250)
ArgentinaMexico.BS.Emp.Coex <- matrix(0:0, 100, 250)
#
BrazilChile.BS.Emp.Coex[1:97,] <- t(t.boot.Fx.Coex$t[,1801:1897])
BrazilArgentina.BS.Emp.Coex[1:97,] <- t(t.boot.Fx.Coex$t[,1898:1994])
BrazilMexico.BS.Emp.Coex[1:97,] <- t(t.boot.Fx.Coex$t[,1995:2091])
ChileArgentina.BS.Emp.Coex[1:97,] <- t(t.boot.Fx.Coex$t[,2092:2188])
ChileMexico.BS.Emp.Coex[1:97,] <- t(t.boot.Fx.Coex$t[,2189:2285])
ArgentinaMexico.BS.Emp.Coex[1:97,] <- t(t.boot.Fx.Coex$t[,2286:2382])
#
########################################################################
########################################################################
BootStrapDistanceSDFunction <- function(M1, M2){
  distance <- rep(0, ncol(M1))
  #
  for (j in 1:ncol(M1)) {
    d.temp <- rep(0, nrow(M1))
    for (i in 1:nrow(M1)) {
      d.temp[i] <- (M1[i,j] - M2[i,j])^2
    }
    distance[j] <- sqrt((1/nrow(M1)) * sum(d.temp)) 
  }
  return(sd(distance))
}
#
BootStrapDistanceSDFunctionCAV <- function(Cav, M1){
  distance <- rep(0, ncol(M1))
  #
  for (j in 1:ncol(M1)) {
    d.temp <- rep(0, nrow(M1))
    for (i in 1:nrow(M1)) {
      d.temp[i] <- (M1[i,j] - Cav[1,(i*10-1)])^2
    }
    distance[j] <- sqrt((1/nrow(M1)) * sum(d.temp)) 
  }
  return(sd(distance))
}
#
#
########################################################################
########################################################################
BrazilChile.Diag.Dist.SD <- BootStrapDistanceSDFunction(BrazilChile.BS.Diag, BrazilChile.BS.Emp.Diag)
#
BrazilArgentina.Diag.Dist.SD <- BootStrapDistanceSDFunction(BrazilArgentina.BS.Diag, BrazilArgentina.BS.Emp.Diag)
#
BrazilMexico.Diag.Dist.SD <- BootStrapDistanceSDFunction(BrazilMexico.BS.Diag, BrazilMexico.BS.Emp.Diag)
#
ChileArgentina.Diag.Dist.SD <- BootStrapDistanceSDFunction(ChileArgentina.BS.Diag, ChileArgentina.BS.Emp.Diag)
#
ChileMexico.Diag.Dist.SD <- BootStrapDistanceSDFunction(ChileMexico.BS.Diag, ChileMexico.BS.Emp.Diag)
#
ArgentinaMexico.Diag.Dist.SD <- BootStrapDistanceSDFunction(ArgentinaMexico.BS.Diag, ArgentinaMexico.BS.Emp.Diag)
#
########################################################################
########################################################################
BrazilChile.Coex.Dist.SD <- BootStrapDistanceSDFunction(BrazilChile.BS.Coex, BrazilChile.BS.Emp.Coex)
#
BrazilArgentina.Coex.Dist.SD <- BootStrapDistanceSDFunction(BrazilArgentina.BS.Coex, BrazilArgentina.BS.Emp.Coex)
#
BrazilMexico.Coex.Dist.SD <- BootStrapDistanceSDFunction(BrazilMexico.BS.Coex, BrazilMexico.BS.Emp.Coex)
#
ChileArgentina.Coex.Dist.SD <- BootStrapDistanceSDFunction(ChileArgentina.BS.Coex, ChileArgentina.BS.Emp.Coex)
#
ChileMexico.Coex.Dist.SD <- BootStrapDistanceSDFunction(ChileMexico.BS.Coex, ChileMexico.BS.Emp.Coex)
#
ArgentinaMexico.Coex.Dist.SD <- BootStrapDistanceSDFunction(ArgentinaMexico.BS.Coex, ArgentinaMexico.BS.Emp.Coex)
#
########################################################################
########################################################################
########################################################################
########################################################################
BrazilChile.Diag.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilChile.LT, BrazilChile.BS.Emp.Diag)
#
BrazilArgentina.Diag.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilArgentina.LT, BrazilArgentina.BS.Emp.Diag)
#
BrazilMexico.Diag.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilMexico.LT, BrazilMexico.BS.Emp.Diag)
#
ChileArgentina.Diag.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ChileArgentina.LT, ChileArgentina.BS.Emp.Diag)
#
ChileMexico.Diag.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ChileMexico.LT, ChileMexico.BS.Emp.Diag)
#
ArgentinaMexico.Diag.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ArgentinaMexico.LT, ArgentinaMexico.BS.Emp.Diag)
#
########################################################################
########################################################################
BrazilChile.Coex.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilChile.Coex, BrazilChile.BS.Emp.Coex)
#
BrazilArgentina.Coex.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilArgentina.Coex, BrazilArgentina.BS.Emp.Coex)
#
BrazilMexico.Coex.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilMexico.Coex, BrazilMexico.BS.Emp.Coex)
#
ChileArgentina.Coex.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ChileArgentina.Coex, ChileArgentina.BS.Emp.Coex)
#
ChileMexico.Coex.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ChileMexico.Coex, ChileMexico.BS.Emp.Coex)
#
ArgentinaMexico.Coex.EmpCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ArgentinaMexico.Coex, ArgentinaMexico.BS.Emp.Coex)
#
########################################################################
########################################################################
########################################################################
########################################################################
BrazilChile.Diag.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilChile.LT, BrazilChile.BS.Diag)
#
BrazilArgentina.Diag.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilArgentina.LT, BrazilArgentina.BS.Diag)
#
BrazilMexico.Diag.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilMexico.LT, BrazilMexico.BS.Diag)
#
ChileArgentina.Diag.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ChileArgentina.LT, ChileArgentina.BS.Diag)
#
ChileMexico.Diag.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ChileMexico.LT, ChileMexico.BS.Diag)
#
ArgentinaMexico.Diag.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ArgentinaMexico.LT, ArgentinaMexico.BS.Diag)
#
########################################################################
########################################################################
BrazilChile.Coex.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilChile.Coex, BrazilChile.BS.Coex)
#
BrazilArgentina.Coex.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilArgentina.Coex, BrazilArgentina.BS.Coex)
#
BrazilMexico.Coex.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(BrazilMexico.Coex, BrazilMexico.BS.Coex)
#
ChileArgentina.Coex.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ChileArgentina.Coex, ChileArgentina.BS.Coex)
#
ChileMexico.Coex.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ChileMexico.Coex, ChileMexico.BS.Coex)
#
ArgentinaMexico.Coex.tCAV.Dist.SD <- BootStrapDistanceSDFunctionCAV(ArgentinaMexico.Coex, ArgentinaMexico.BS.Coex)
#
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
###############  T-COP SD
# BrazilChile.t.Diagonal.CI <- matrix(0:0, 100, 2)
BrazilArgentina.t.Diagonal.CI <- matrix(0:0, 100, 2)
BrazilMexico.t.Diagonal.CI <- matrix(0:0, 100, 2)
ChileArgentina.t.Diagonal.CI <- matrix(0:0, 100, 2)
ChileMexico.t.Diagonal.CI <- matrix(0:0, 100, 2)
ArgentinaMexico.t.Diagonal.CI <- matrix(0:0, 100, 2)
####
for (i in 1:nrow(BrazilChile.t.Diagonal.CI)){
  BrazilChile.t.Diagonal.CI[i,1] <- BrazilChile.t.Diagonal[(10*i),2] + sd(BrazilChile.BS.Diag[i,])*1.660 
  BrazilArgentina.t.Diagonal.CI[i,1] <- BrazilArgentina.t.Diagonal[(10*i),2] + sd(BrazilArgentina.BS.Diag[i,])*1.660 
  BrazilMexico.t.Diagonal.CI[i,1] <- BrazilMexico.t.Diagonal[(10*i),2] + sd(BrazilMexico.BS.Diag[i,])*1.660 
  ChileArgentina.t.Diagonal.CI[i,1] <- ChileArgentina.t.Diagonal[(10*i),2] + sd(ChileArgentina.BS.Diag[i,])*1.660 
  ChileMexico.t.Diagonal.CI[i,1] <- ChileMexico.t.Diagonal[(10*i),2] + sd(ChileMexico.BS.Diag[i,])*1.660 
  ArgentinaMexico.t.Diagonal.CI[i,1] <- ArgentinaMexico.t.Diagonal[(10*i),2] + sd(ArgentinaMexico.BS.Diag[i,])*1.660 
  ##
  BrazilChile.t.Diagonal.CI[i,2] <- BrazilChile.t.Diagonal[(10*i),2] - sd(BrazilChile.BS.Diag[i,])*1.660 
  BrazilArgentina.t.Diagonal.CI[i,2] <- BrazilArgentina.t.Diagonal[(10*i),2] - sd(BrazilArgentina.BS.Diag[i,])*1.660 
  BrazilMexico.t.Diagonal.CI[i,2] <- BrazilMexico.t.Diagonal[(10*i),2] + sd(BrazilMexico.BS.Diag[i,])*1.660 
  ChileArgentina.t.Diagonal.CI[i,2] <- ChileArgentina.t.Diagonal[(10*i),2] - sd(ChileArgentina.BS.Diag[i,])*1.660 
  ChileMexico.t.Diagonal.CI[i,2] <- ChileMexico.t.Diagonal[(10*i),2] + sd(ChileMexico.BS.Diag[i,])*1.660 
  ArgentinaMexico.t.Diagonal.CI[i,2] <- ArgentinaMexico.t.Diagonal[(10*i),2] - sd(ArgentinaMexico.BS.Diag[i,])*1.660 
  ##  
}
#####################################################################
#######################################################################
######################################################################
BrazilChile.t.Coex.CI <- matrix(0:0, 100, 2)
BrazilArgentina.t.Coex.CI <- matrix(0:0, 100, 2)
BrazilMexico.t.Coex.CI <- matrix(0:0, 100, 2)
ChileArgentina.t.Coex.CI <- matrix(0:0, 100, 2)
ChileMexico.t.Coex.CI <- matrix(0:0, 100, 2)
ArgentinaMexico.t.Coex.CI <- matrix(0:0, 100, 2)
####
for (i in 2:(nrow(BrazilChile.t.Coex.CI)-1)){
  BrazilChile.t.Coex.CI[i,1] <- BrazilChile.t.Coex[((10*(i-2)+1)),2] + sd(BrazilChile.BS.Coex[i,])*1.660 
  BrazilArgentina.t.Coex.CI[i,1] <- BrazilArgentina.t.Coex[((10*(i-2)+1)),2] + sd(BrazilArgentina.BS.Coex[i,])*1.660 
  BrazilMexico.t.Coex.CI[i,1] <- BrazilMexico.t.Coex[((10*(i-2)+1)),2] + sd(BrazilMexico.BS.Coex[i,])*1.660 
  ChileArgentina.t.Coex.CI[i,1] <- ChileArgentina.t.Coex[((10*(i-2)+1)),2] + sd(ChileArgentina.BS.Coex[i,])*1.660 
  ChileMexico.t.Coex.CI[i,1] <- ChileMexico.t.Coex[((10*(i-2)+1)),2] + sd(ChileMexico.BS.Coex[i,])*1.660 
  ArgentinaMexico.t.Coex.CI[i,1] <- ArgentinaMexico.t.Coex[((10*(i-2)+1)),2] + sd(ArgentinaMexico.BS.Coex[i,])*1.660 
  ##
  BrazilChile.t.Coex.CI[i,2] <- BrazilChile.t.Coex[((10*(i-2)+1)),2] - sd(BrazilChile.BS.Coex[i,])*1.660 
  BrazilArgentina.t.Coex.CI[i,2] <- BrazilArgentina.t.Coex[((10*(i-2)+1)),2] - sd(BrazilArgentina.BS.Coex[i,])*1.660 
  BrazilMexico.t.Coex.CI[i,2] <- BrazilMexico.t.Coex[((10*(i-2)+1)),2] - sd(BrazilMexico.BS.Coex[i,])*1.660 
  ChileArgentina.t.Coex.CI[i,2] <- ChileArgentina.t.Coex[((10*(i-2)+1)),2] - sd(ChileArgentina.BS.Coex[i,])*1.660 
  ChileMexico.t.Coex.CI[i,2] <- ChileMexico.t.Coex[((10*(i-2)+1)),2] - sd(ChileMexico.BS.Coex[i,])*1.660 
  ArgentinaMexico.t.Coex.CI[i,2] <- ArgentinaMexico.t.Coex[((10*(i-2)+1)),2] - sd(ArgentinaMexico.BS.Coex[i,])*1.660 
  ##  
}
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
###############  EMP-COP SD

BrazilChile.Emp.Diagonal.CI <- matrix(0:0, 100, 2)
BrazilArgentina.Emp.Diagonal.CI <- matrix(0:0, 100, 2)
BrazilMexico.Emp.Diagonal.CI <- matrix(0:0, 100, 2)
ChileArgentina.Emp.Diagonal.CI <- matrix(0:0, 100, 2)
ChileMexico.Emp.Diagonal.CI <- matrix(0:0, 100, 2)
ArgentinaMexico.Emp.Diagonal.CI <- matrix(0:0, 100, 2)
####
for (i in 1:nrow(BrazilChile.Emp.Diagonal.CI)){
  BrazilChile.Emp.Diagonal.CI[i,1] <- BrazilChile.Emp.Cop[WeirdSeries[i],2] + sd(BrazilChile.BS.Diag[i,])*1.984 
  BrazilArgentina.Emp.Diagonal.CI[i,1] <- BrazilArgentina.Emp.Cop[WeirdSeries[i]] + sd(BrazilArgentina.BS.Diag[i,])*1.984 
  BrazilMexico.Emp.Diagonal.CI[i,1] <- BrazilMexico.Emp.Cop[WeirdSeries[i]] + sd(BrazilMexico.BS.Diag[i,])*1.984 
  ChileArgentina.Emp.Diagonal.CI[i,1] <- ChileArgentina.Emp.Cop[WeirdSeries[i]] + sd(ChileArgentina.BS.Diag[i,])*1.984 
  ChileMexico.Emp.Diagonal.CI[i,1] <- ChileMexico.Emp.Cop[WeirdSeries[i]] + sd(ChileMexico.BS.Diag[i,])*1.984 
  ArgentinaMexico.Emp.Diagonal.CI[i,1] <- ArgentinaMexico.Emp.Cop[WeirdSeries[i]] + sd(ArgentinaMexico.BS.Diag[i,])*1.984 
  ##
  BrazilChile.Emp.Diagonal.CI[i,2] <- BrazilChile.Emp.Cop[WeirdSeries[i],2] - sd(BrazilChile.BS.Diag[i,])*1.984 
  BrazilArgentina.Emp.Diagonal.CI[i,2] <- BrazilArgentina.Emp.Cop[WeirdSeries[i]] - sd(BrazilArgentina.BS.Diag[i,])*1.984 
  BrazilMexico.Emp.Diagonal.CI[i,2] <- BrazilMexico.Emp.Cop[WeirdSeries[i]] - sd(BrazilMexico.BS.Diag[i,])*1.984 
  ChileArgentina.Emp.Diagonal.CI[i,2] <- ChileArgentina.Emp.Cop[WeirdSeries[i]] - sd(ChileArgentina.BS.Diag[i,])*1.984 
  ChileMexico.Emp.Diagonal.CI[i,2] <- ChileMexico.Emp.Cop[WeirdSeries[i]] - sd(ChileMexico.BS.Diag[i,])*1.984 
  ArgentinaMexico.Emp.Diagonal.CI[i,2] <- ArgentinaMexico.Emp.Cop[WeirdSeries[i]] - sd(ArgentinaMexico.BS.Diag[i,])*1.984 
  ##  
}
###############  EMP-COP SD

BrazilChile.Emp.Coex.CI <- matrix(0:0, 100, 2)
BrazilArgentina.Emp.Coex.CI <- matrix(0:0, 100, 2)
BrazilMexico.Emp.Coex.CI <- matrix(0:0, 100, 2)
ChileArgentina.Emp.Coex.CI <- matrix(0:0, 100, 2)
ChileMexico.Emp.Coex.CI <- matrix(0:0, 100, 2)
ArgentinaMexico.Emp.Coex.CI <- matrix(0:0, 100, 2)
####
for (i in 1:nrow(BrazilChile.Emp.Coex.CI)){
  BrazilChile.Emp.Coex.CI[i,1] <- BrazilChile.Emp.Coex[(WeirdSeries[i]-1)] + sd(BrazilChile.BS.Coex[i,])*1.984 
  BrazilArgentina.Emp.Coex.CI[i,1] <- BrazilArgentina.Emp.Coex[(WeirdSeries[i]-1)] + sd(BrazilArgentina.BS.Coex[i,])*1.984 
  BrazilMexico.Emp.Coex.CI[i,1] <- BrazilMexico.Emp.Coex[(WeirdSeries[i]-1)] + sd(BrazilMexico.BS.Coex[i,])*1.984 
  ChileArgentina.Emp.Coex.CI[i,1] <- ChileArgentina.Emp.Coex[(WeirdSeries[i]-1)] + sd(ChileArgentina.BS.Coex[i,])*1.984 
  ChileMexico.Emp.Coex.CI[i,1] <- ChileMexico.Emp.Coex[(WeirdSeries[i]-1)] + sd(ChileMexico.BS.Coex[i,])*1.984 
  ArgentinaMexico.Emp.Coex.CI[i,1] <- ArgentinaMexico.Emp.Coex[(WeirdSeries[i]-1)] + sd(ArgentinaMexico.BS.Coex[i,])*1.984 
  ##
  BrazilChile.Emp.Coex.CI[i,2] <- BrazilChile.Emp.Coex[(WeirdSeries[i]-1)] - sd(BrazilChile.BS.Coex[i,])*1.984 
  BrazilArgentina.Emp.Coex.CI[i,2] <- BrazilArgentina.Emp.Coex[(WeirdSeries[i]-1)] - sd(BrazilArgentina.BS.Coex[i,])*1.984 
  BrazilMexico.Emp.Coex.CI[i,2] <- BrazilMexico.Emp.Coex[(WeirdSeries[i]-1)] - sd(BrazilMexico.BS.Coex[i,])*1.984 
  ChileArgentina.Emp.Coex.CI[i,2] <- ChileArgentina.Emp.Coex[(WeirdSeries[i]-1)] - sd(ChileArgentina.BS.Coex[i,])*1.984 
  ChileMexico.Emp.Coex.CI[i,2] <- ChileMexico.Emp.Coex[(WeirdSeries[i]-1)] - sd(ChileMexico.BS.Coex[i,])*1.984 
  ArgentinaMexico.Emp.Coex.CI[i,2] <- ArgentinaMexico.Emp.Coex[(WeirdSeries[i]-1)] - sd(ArgentinaMexico.BS.Coex[i,])*1.984 
  ##  
}

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################


pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Tail/BCTailPlot.pdf")
plot(1:20/100, BrazilChile.Emp.Coex.CI[2:21,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,0.8),
     main = "Lower Tail Dependence for Brazil-Chile", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:20/100, BrazilChile.Emp.Coex.CI[2:21,2], type = "l", lty = 2, col = "royalblue1")
points(10:200/1000, BrazilChile.Coex.SE[2,10:200], type = "l", lty = 2, col = "coral1")
points(10:200/1000, BrazilChile.Coex.SE[3,10:200], type = "l", lty = 2, col = "coral1")
points(1:20/100, BrazilChile.t.Coex.CI[2:21,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, BrazilChile.t.Coex.CI[2:21,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, BrazilChile.Emp.Coex[WeirdSeries[1:20]], type = "l", col = "royalblue1", lwd = 2)
points(10:200/1000, BrazilChile.t.Coex[10:200,2], type = "l", lwd = 2, col = "mediumpurple3")
points(10:200/1000, BrazilChile.Coex[1,10:200], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Tail/BATailPlot.pdf")
plot(1:20/100, BrazilArgentina.Emp.Coex.CI[2:21,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,0.8),
     main = "Lower Tail Dependence for Brazil-Argentina", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:20/100, BrazilArgentina.Emp.Coex.CI[2:21,2], type = "l", lty = 2, col = "royalblue1")
points(10:200/1000, BrazilArgentina.Coex.SE[2,10:200], type = "l", lty = 2, col = "coral1")
points(10:200/1000, BrazilArgentina.Coex.SE[3,10:200], type = "l", lty = 2, col = "coral1")
points(1:20/100, BrazilArgentina.t.Coex.CI[2:21,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, BrazilArgentina.t.Coex.CI[2:21,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, BrazilArgentina.Emp.Coex[WeirdSeries[1:20]], type = "l", col = "royalblue1", lwd = 2)
points(10:200/1000, BrazilArgentina.t.Coex[10:200,2], type = "l", lwd = 2, col = "mediumpurple3")
points(10:200/1000, BrazilArgentina.Coex[1,10:200], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Tail/BMTailPlot.pdf")
plot(1:20/100, BrazilMexico.Emp.Coex.CI[2:21,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,0.8),
     main = "Lower Tail Dependence for Brazil-Mexico", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:20/100, BrazilMexico.Emp.Coex.CI[2:21,2], type = "l", lty = 2, col = "royalblue1")
points(10:200/1000, BrazilMexico.Coex.SE[2,10:200], type = "l", lty = 2, col = "coral1")
points(10:200/1000, BrazilMexico.Coex.SE[3,10:200], type = "l", lty = 2, col = "coral1")
points(1:20/100, BrazilMexico.t.Coex.CI[2:21,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, BrazilMexico.t.Coex.CI[2:21,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, BrazilMexico.Emp.Coex[WeirdSeries[1:20]], type = "l", col = "royalblue1", lwd = 2)
points(10:200/1000, BrazilMexico.t.Coex[10:200,2], type = "l", lwd = 2, col = "mediumpurple3")
points(10:200/1000, BrazilMexico.Coex[1,10:200], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Tail/CATailPlot.pdf")
plot(1:20/100, ChileArgentina.Emp.Coex.CI[2:21,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,0.8),
     main = "Lower Tail Dependence for Chile-Argentina", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:20/100, ChileArgentina.Emp.Coex.CI[2:21,2], type = "l", lty = 2, col = "royalblue1")
points(10:200/1000, ChileArgentina.Coex.SE[2,10:200], type = "l", lty = 2, col = "coral1")
points(10:200/1000, ChileArgentina.Coex.SE[3,10:200], type = "l", lty = 2, col = "coral1")
points(1:20/100, ChileArgentina.t.Coex.CI[2:21,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, ChileArgentina.t.Coex.CI[2:21,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, ChileArgentina.Emp.Coex[WeirdSeries[1:20]], type = "l", col = "royalblue1", lwd = 2)
points(10:200/1000, ChileArgentina.t.Coex[10:200,2], type = "l", lwd = 2, col = "mediumpurple3")
points(10:200/1000, ChileArgentina.Coex[1,10:200], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Tail/CMTailPlot.pdf")
plot(1:20/100, ChileMexico.Emp.Coex.CI[2:21,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,0.8),
     main = "Lower Tail Dependence for Chile-Mexico", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:20/100, ChileMexico.Emp.Coex.CI[2:21,2], type = "l", lty = 2, col = "royalblue1")
points(10:200/1000, ChileMexico.Coex.SE[2,10:200], type = "l", lty = 2, col = "coral1")
points(10:200/1000, ChileMexico.Coex.SE[3,10:200], type = "l", lty = 2, col = "coral1")
points(1:20/100, ChileMexico.t.Coex.CI[2:21,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, ChileMexico.t.Coex.CI[2:21,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, ChileMexico.Emp.Coex[WeirdSeries[1:20]], type = "l", col = "royalblue1", lwd = 2)
points(10:200/1000, ChileMexico.t.Coex[10:200,2], type = "l", lwd = 2, col = "mediumpurple3")
points(10:200/1000, ChileMexico.Coex[1,10:200], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Tail/AMTailPlot.pdf")
plot(1:20/100, ArgentinaMexico.Emp.Coex.CI[2:21,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,0.8),
     main = "Lower Tail Dependence for Argentina-Mexico", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:20/100, ArgentinaMexico.Emp.Coex.CI[2:21,2], type = "l", lty = 2, col = "royalblue1")
points(10:200/1000, ArgentinaMexico.Coex.SE[2,10:200], type = "l", lty = 2, col = "coral1")
points(10:200/1000, ArgentinaMexico.Coex.SE[3,10:200], type = "l", lty = 2, col = "coral1")
points(1:20/100, ArgentinaMexico.t.Coex.CI[2:21,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, ArgentinaMexico.t.Coex.CI[2:21,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:20/100, ArgentinaMexico.Emp.Coex[WeirdSeries[1:20]], type = "l", col = "royalblue1", lwd = 2)
points(10:200/1000, ArgentinaMexico.t.Coex[10:200,2], type = "l", lwd = 2, col = "mediumpurple3")
points(10:200/1000, ArgentinaMexico.Coex[1,10:200], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Diag/BCDiagPlot.pdf")
plot(1:100/100, BrazilChile.Emp.Diagonal.CI[,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,1),
     main = "Diagonal Section for Brazil-Chile", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:100/100, BrazilChile.Emp.Diagonal.CI[,2], type = "l", lty = 2, col = "royalblue1")
points(1:999/1000, BrazilChile.LT.SE[2,], type = "l", lty = 2, col = "coral1")
points(1:999/1000, BrazilChile.LT.SE[3,], type = "l", lty = 2, col = "coral1")
points(1:100/100, BrazilChile.t.Diagonal.CI[,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, BrazilChile.t.Diagonal.CI[,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, BrazilChile.Emp.Cop[WeirdSeries,2], type = "l", col = "royalblue1", lwd = 2)
points(1:1000/1000, BrazilChile.t.Diagonal[,2], type = "l", lwd = 2, col = "mediumpurple3")
points(1:999/1000, BrazilChile.LT[1,], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()

pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Diag/BADiagPlot.pdf")
plot(1:100/100, BrazilArgentina.Emp.Diagonal.CI[,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,1),
     main = "Diagonal Section for Brazil-Argentina", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:100/100, BrazilArgentina.Emp.Diagonal.CI[,2], type = "l", lty = 2, col = "royalblue1")
points(1:999/1000, BrazilArgentina.LT.SE[2,], type = "l", lty = 2, col = "coral1")
points(1:999/1000, BrazilArgentina.LT.SE[3,], type = "l", lty = 2, col = "coral1")
points(1:100/100, BrazilArgentina.t.Diagonal.CI[,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, BrazilArgentina.t.Diagonal.CI[,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, BrazilArgentina.Emp.Cop[WeirdSeries], type = "l", col = "royalblue1", lwd = 2)
points(1:1000/1000, BrazilArgentina.t.Diagonal[,2], type = "l", lwd = 2, col = "mediumpurple3")
points(1:999/1000, BrazilArgentina.LT[1,], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()

pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Diag/BMDiagPlot.pdf")
plot(1:100/100, BrazilMexico.Emp.Diagonal.CI[,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,1),
     main = "Diagonal Section for Brazil-Mexico", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:100/100, BrazilMexico.Emp.Diagonal.CI[,2], type = "l", lty = 2, col = "royalblue1")
points(1:999/1000, BrazilMexico.LT.SE[2,], type = "l", lty = 2, col = "coral1")
points(1:999/1000, BrazilMexico.LT.SE[3,], type = "l", lty = 2, col = "coral1")
points(1:100/100, BrazilMexico.t.Diagonal.CI[,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, BrazilMexico.t.Diagonal.CI[,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, BrazilMexico.Emp.Cop[WeirdSeries], type = "l", col = "royalblue1", lwd = 2)
points(1:1000/1000, BrazilMexico.t.Diagonal[,2], type = "l", lwd = 2, col = "mediumpurple3")
points(1:999/1000, BrazilMexico.LT[1,], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()

pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Diag/CADiagPlot.pdf")
plot(1:100/100, ChileArgentina.Emp.Diagonal.CI[,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,1),
     main = "Diagonal Section for Chile-Argentina", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:100/100, ChileArgentina.Emp.Diagonal.CI[,2], type = "l", lty = 2, col = "royalblue1")
points(1:999/1000, ChileArgentina.LT.SE[2,], type = "l", lty = 2, col = "coral1")
points(1:999/1000, ChileArgentina.LT.SE[3,], type = "l", lty = 2, col = "coral1")
points(1:100/100, ChileArgentina.t.Diagonal.CI[,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, ChileArgentina.t.Diagonal.CI[,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, ChileArgentina.Emp.Cop[WeirdSeries], type = "l", col = "royalblue1", lwd = 2)
points(1:1000/1000, ChileArgentina.t.Diagonal[,2], type = "l", lwd = 2, col = "mediumpurple3")
points(1:999/1000, ChileArgentina.LT[1,], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()

pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Diag/CMDiagPlot.pdf")
plot(1:100/100, ChileMexico.Emp.Diagonal.CI[,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,1),
     main = "Diagonal Section for Chile-Mexico", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:100/100, ChileMexico.Emp.Diagonal.CI[,2], type = "l", lty = 2, col = "royalblue1")
points(1:999/1000, ChileMexico.LT.SE[2,], type = "l", lty = 2, col = "coral1")
points(1:999/1000, ChileMexico.LT.SE[3,], type = "l", lty = 2, col = "coral1")
points(1:100/100, ChileMexico.t.Diagonal.CI[,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, ChileMexico.t.Diagonal.CI[,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, ChileMexico.Emp.Cop[WeirdSeries], type = "l", col = "royalblue1", lwd = 2)
points(1:1000/1000, ChileMexico.t.Diagonal[,2], type = "l", lwd = 2, col = "mediumpurple3")
points(1:999/1000, ChileMexico.LT[1,], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()

pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Diag/AMDiagPlot.pdf")
plot(1:100/100, ArgentinaMexico.Emp.Diagonal.CI[,1], type = "l", lty = 2, col = "royalblue1", ylim = c(0,1),
     main = "Diagonal Section for Argentina-Mexico", ylab = "P(X < t, Y < t)/t", xlab = "t")
points(1:100/100, ArgentinaMexico.Emp.Diagonal.CI[,2], type = "l", lty = 2, col = "royalblue1")
points(1:999/1000, ArgentinaMexico.LT.SE[2,], type = "l", lty = 2, col = "coral1")
points(1:999/1000, ArgentinaMexico.LT.SE[3,], type = "l", lty = 2, col = "coral1")
points(1:100/100, ArgentinaMexico.t.Diagonal.CI[,1], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, ArgentinaMexico.t.Diagonal.CI[,2], type = "l", lty = 2, col = "mediumpurple3")
points(1:100/100, ArgentinaMexico.Emp.Cop[WeirdSeries], type = "l", col = "royalblue1", lwd = 2)
points(1:1000/1000, ArgentinaMexico.t.Diagonal[,2], type = "l", lwd = 2, col = "mediumpurple3")
points(1:999/1000, ArgentinaMexico.LT[1,], type = "l", lwd = 2, col = "coral1")
legend("topleft", c("CAViaR w/ 95% CI", "Student-t w/ 95% CI", "Empirical w/ 95% CI"), col = c("coral1", "mediumpurple3", "royalblue"),
       lwd = c(2,2,2))
dev.off()




























