################################################################################################################################
################################################################################################################################
###################### STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - DISTANCE BETWEEN CURVES ####################
################################################################################################################################
################################################################################################################################
EmpDistanceL2 <- function(CX, data){
  seqseq <- (1:99) / 100
  emp.cop <- matrix(0:0, length(seqseq), 1)
  for (i in 1:length(seqseq)){
    indictemp <- as.numeric((data[,2] < seqseq[i]) & (data[,3] < seqseq[i]))
    emp.cop[i,1] <- mean(indictemp)
    #
  }
  tCopP <- matrix(0:0, nrow(emp.cop), 1)
  tCopP[,1] <- pCopula(u = cbind(seqseq, seqseq), copula = tCopula(dim = 2, param = (t.CappJoint$par[(20+CX)]/100000),
                                                                     df = floor(t.CappJoint$par[27]/100000)))
  #
  ss <- (tCopP[,1] - emp.cop[,1]) ^ 2
  #
  L2 <- (sum(ss) / nrow(tCopP)) ^ 0.5
  #
  sqer <- rep(0, length(ss))
  for (i in 1:length(ss)){
    sqer[i] <- (L2 - sqrt(ss[i])) ^ 2
  }
  #
  se <- sqrt((1/(length(sqer) - 1)) * (sum(sqer)))
  return(L2)
}
#
CAViaRDistanceL2 <- function(CAViaRLT, CX){
  seqseq <- (1:99) / 100
  tCopP <- matrix(0:0, length(seqseq), 1)
  tCopP[,1] <- pCopula(u = cbind(seqseq, seqseq), copula = tCopula(dim = 2, param = (t.CappJoint$par[(20+CX)]/100000),
                                                                       df = floor(t.CappJoint$par[27]/100000)))
  #
  ss <- (tCopP[,1] - CAViaRLT[1,(seqseq*1000)]) ^ 2
  #
  L2 <- (sum(ss) / nrow(tCopP)) ^ 0.5
  #
  sqer <- rep(0, nrow(tCopP))
  for (i in 1:length(ss)){
    sqer[i] <- (L2 - sqrt(ss[i])) ^ 2
  }
  #
  se <- sqrt((1/(length(sqer) - 1)) * (sum(sqer)))
  return(L2)
}
#
EmpCAViaRL2 <- function(CAViaRLT, data){
  seqseq <- (1:99) / 100
  emp.cop <- matrix(0:0, length(seqseq), 1)
  for (i in 1:length(seqseq)){
    indictemp <- as.numeric((data[,2] < seqseq[i]) & (data[,3] < seqseq[i]))
    emp.cop[i,1] <- mean(indictemp)
  }
    #
    ss <- (emp.cop[,1] - CAViaRLT[1,(seqseq*1000)]) ^ 2
    #
    L2 <- (sum(ss) / nrow(emp.cop)) ^ 0.5
    #
    sqer <- rep(0, nrow(emp.cop))
    for (i in 1:length(ss)){
      sqer[i] <- (L2 - sqrt(ss[i])) ^ 2
    }
    #
    se <- sqrt((1/(length(sqer) - 1)) * (sum(sqer)))
    return(L2)
}

#################
#################
BrazilChile.Empt.L2 <- EmpDistanceL2(1, All.cappU[,c(1,2,3)])
#
BrazilArgentina.Empt.L2 <- EmpDistanceL2(2, All.cappU[,c(1,2,4)])
#
BrazilMexico.Empt.L2 <- EmpDistanceL2(3, All.cappU[,c(1,2,5)])
#
ChileArgentina.Empt.L2 <- EmpDistanceL2(4, All.cappU[,c(1,3,4)])
#
ChileMexico.Empt.L2 <- EmpDistanceL2(5, All.cappU[,c(1,3,5)])
#
ArgentinaMexico.Empt.L2 <- EmpDistanceL2(6, All.cappU[,c(1,4,5)])
#################
#################
BrazilChile.tCAViaR.L2 <- CAViaRDistanceL2(BrazilChile.LT, 1)
#
BrazilArgentina.tCAViaR.L2 <- CAViaRDistanceL2(BrazilArgentina.LT, 2)
#
BrazilMexico.tCAViaR.L2 <- CAViaRDistanceL2(BrazilMexico.LT, 3)
#
ChileArgentina.tCAViaR.L2 <- CAViaRDistanceL2(ChileArgentina.LT, 4)
#
ChileMexico.tCAViaR.L2 <- CAViaRDistanceL2(ChileMexico.LT, 5)
#
ArgentinaMexico.tCAViaR.L2 <- CAViaRDistanceL2(ArgentinaMexico.LT, 6)
#################
#################
BrazilChile.EmpCAViaR.L2 <- EmpCAViaRL2(BrazilChile.LT, All.cappU[,c(1,2,3)])
#
BrazilArgentina.EmpCAViaR.L2 <- EmpCAViaRL2(BrazilArgentina.LT, All.cappU[,c(1,2,4)])
#
BrazilMexico.EmpCAViaR.L2 <- EmpCAViaRL2(BrazilMexico.LT, All.cappU[,c(1,2,5)])
#
ChileArgentina.EmpCAViaR.L2 <- EmpCAViaRL2(ChileArgentina.LT, All.cappU[,c(1,3,4)])
#
ChileMexico.EmpCAViaR.L2 <- EmpCAViaRL2(ChileMexico.LT, All.cappU[,c(1,3,5)])
#
ArgentinaMexico.EmpCAViaR.L2 <- EmpCAViaRL2(ArgentinaMexico.LT, All.cappU[,c(1,4,5)])

TablePrint(c(
  "\\hline ", "\\hline \\hline Brazil-Chile", " ", "Brazil-Argentina", " ", "Brazil-Mexico", " ", "Chile-Argentina", " ",
  "Chile-Mexico", " ", "Argentina-Mexico", " ",
  "Student-t and Empirical", BrazilChile.Empt.L2, BrazilChile.Diag.Dist.SD, 
  BrazilArgentina.Empt.L2, BrazilArgentina.Diag.Dist.SD, BrazilMexico.Empt.L2, BrazilMexico.Diag.Dist.SD,
  ChileArgentina.Empt.L2, ChileArgentina.Diag.Dist.SD, ChileMexico.Empt.L2, ChileMexico.Diag.Dist.SD,
  ArgentinaMexico.Empt.L2, ArgentinaMexico.Diag.Dist.SD,
  
  "Student-t and CAViaR", BrazilChile.tCAViaR.L2, BrazilChile.Diag.tCAV.Dist.SD, 
  BrazilArgentina.tCAViaR.L2, BrazilArgentina.Diag.tCAV.Dist.SD, BrazilMexico.tCAViaR.L2, BrazilMexico.Diag.tCAV.Dist.SD,
  ChileArgentina.tCAViaR.L2, ChileArgentina.Diag.tCAV.Dist.SD, ChileMexico.tCAViaR.L2, ChileMexico.Diag.tCAV.Dist.SD,
  ArgentinaMexico.tCAViaR.L2, ArgentinaMexico.Diag.tCAV.Dist.SD,
  
  "Empirical and CAViaR", BrazilChile.EmpCAViaR.L2, BrazilChile.Diag.EmpCAV.Dist.SD, 
  BrazilArgentina.EmpCAViaR.L2, BrazilArgentina.Diag.EmpCAV.Dist.SD, BrazilMexico.EmpCAViaR.L2, BrazilMexico.Diag.EmpCAV.Dist.SD,
  ChileArgentina.EmpCAViaR.L2, ChileArgentina.Diag.EmpCAV.Dist.SD, ChileMexico.EmpCAViaR.L2, ChileMexico.Diag.EmpCAV.Dist.SD,
  ArgentinaMexico.EmpCAViaR.L2, ArgentinaMexico.Diag.EmpCAV.Dist.SD
), 13, 4)




















































