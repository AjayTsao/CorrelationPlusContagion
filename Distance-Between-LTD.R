################################################################################################################################
################################################################################################################################
####################### STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - DISTANCE BW LTD ###########################
################################################################################################################################
################################################################################################################################
EmpDistanceLTD <- function(tCoex, EmpCoex){
  tseq <- c(2:200)
  empseq <- c((2:100 * 10), 1001:1100)
  #
  ss <- rep(0,length(tseq))
  for (i in 1:length(tseq)){
    ss[i] <- (tCoex[(tseq[i]),2] - EmpCoex[(empseq[i])]) ^ 2
  }
  #
  L2 <- (sum(ss) / length(tseq)) ^ 0.5
  #
  sqer <- rep(0, length(ss))
  for (i in 1:length(ss)){
    sqer[i] <- (L2 - sqrt(ss[i])) ^ 2
  }
  #
  se <- sqrt((1/(length(ss) - 1)) * (sum(sqer)))
  return(L2)
}
################
################
CavDistanceLTD <- function(tCoex, CavCoex){
  tseq <- c(2:200)
  #
  ss <- rep(0,length(tseq))
  for (i in 1:length(tseq)){
    ss[i] <- (tCoex[(tseq[i]),2] - CavCoex[1,(tseq[i])]) ^ 2
  }
  #
  L2 <- (sum(ss) / length(tseq)) ^ 0.5
  #
  sqer <- rep(0, length(ss))
  for (i in 1:length(ss)){
    sqer[i] <- (L2 - sqrt(ss[i])) ^ 2
  }
  #
  se <- sqrt((1/(length(ss) - 1)) * (sum(sqer)))
  return(L2)
}
################
################
CavEmpLTD <- function(EmpCoex, CavCoex){
  Cavseq <- c(2:200)
  empseq <- c((2:100 * 10), 1001:1100)
  #
  ss <- rep(0,length(Cavseq))
  for (i in 1:length(Cavseq)){
    ss[i] <- (EmpCoex[(empseq[i])] - CavCoex[1,(Cavseq[i])]) ^ 2
  }
  #
  L2 <- (sum(ss) / length(Cavseq)) ^ 0.5
  #
  sqer <- rep(0, length(ss))
  for (i in 1:length(ss)){
    sqer[i] <- (L2 - sqrt(ss[i])) ^ 2
  }
  #
  se <- sqrt((1/(length(ss) - 1)) * (sum(sqer)))
  return(L2)
}
################
################
BrazilChile.Empt.Coex.L2 <- EmpDistanceLTD(BrazilChile.t.Coex, BrazilChile.Emp.Coex)
#
BrazilArgentina.Empt.Coex.L2 <- EmpDistanceLTD(BrazilArgentina.t.Coex, BrazilArgentina.Emp.Coex)
#
BrazilMexico.Empt.Coex.L2 <- EmpDistanceLTD(BrazilMexico.t.Coex, BrazilMexico.Emp.Coex)
#
ChileArgentina.Empt.Coex.L2 <- EmpDistanceLTD(ChileArgentina.t.Coex, ChileArgentina.Emp.Coex)
#
ChileMexico.Empt.Coex.L2 <- EmpDistanceLTD(ChileMexico.t.Coex, ChileMexico.Emp.Coex)
#
ArgentinaMexico.Empt.Coex.L2 <- EmpDistanceLTD(ArgentinaMexico.t.Coex, ArgentinaMexico.Emp.Coex)
################
################
BrazilChile.Cavt.Coex.L2 <- CavDistanceLTD(BrazilChile.t.Coex, BrazilChile.Coex)
#
BrazilArgentina.Cavt.Coex.L2 <- CavDistanceLTD(BrazilArgentina.t.Coex, BrazilArgentina.Coex)
#
BrazilMexico.Cavt.Coex.L2 <- CavDistanceLTD(BrazilMexico.t.Coex, BrazilMexico.Coex)
#
ChileArgentina.Cavt.Coex.L2 <- CavDistanceLTD(ChileArgentina.t.Coex, ChileArgentina.Coex)
#
ChileMexico.Cavt.Coex.L2 <- CavDistanceLTD(ChileMexico.t.Coex, ChileMexico.Coex)
#
ArgentinaMexico.Cavt.Coex.L2 <- CavDistanceLTD(ArgentinaMexico.t.Coex, ArgentinaMexico.Coex)
################
################
BrazilChile.CavEmp.Coex.L2 <- CavEmpLTD(BrazilChile.Emp.Coex, BrazilChile.Coex)
#
BrazilArgentina.CavEmp.Coex.L2 <- CavEmpLTD(BrazilArgentina.Emp.Coex, BrazilArgentina.Coex)
#
BrazilMexico.CavEmp.Coex.L2 <- CavEmpLTD(BrazilMexico.Emp.Coex, BrazilMexico.Coex)
#
ChileArgentina.CavEmp.Coex.L2 <- CavEmpLTD(ChileArgentina.Emp.Coex, ChileArgentina.Coex)
#
ChileMexico.CavEmp.Coex.L2 <- CavEmpLTD(ChileMexico.Emp.Coex, ChileMexico.Coex)
#
ArgentinaMexico.CavEmp.Coex.L2 <- CavEmpLTD(ArgentinaMexico.Emp.Coex, ArgentinaMexico.Coex)


TablePrint(c(
  "\\hline ", "\\hline \\hline Brazil-Chile", " ", "Brazil-Argentina", " ", "Brazil-Mexico", " ", "Chile-Argentina", " ",
  "Chile-Mexico", " ", "Argentina-Mexico", " ",
  "Student-t and Empirical", BrazilChile.Empt.Coex.L2, BrazilChile.Coex.Dist.SD, 
  BrazilArgentina.Empt.Coex.L2, BrazilArgentina.Coex.Dist.SD, BrazilMexico.Empt.Coex.L2, BrazilMexico.Coex.Dist.SD,
  ChileArgentina.Empt.Coex.L2, ChileArgentina.Coex.Dist.SD, ChileMexico.Empt.Coex.L2, ChileMexico.Coex.Dist.SD,
  ArgentinaMexico.Empt.Coex.L2, ArgentinaMexico.Coex.Dist.SD,
  
  "Student-t and CAViaR", BrazilChile.Cavt.Coex.L2, BrazilChile.Coex.tCAV.Dist.SD, 
  BrazilArgentina.Cavt.Coex.L2, BrazilArgentina.Coex.tCAV.Dist.SD, BrazilMexico.Cavt.Coex.L2, BrazilMexico.Coex.tCAV.Dist.SD,
  ChileArgentina.Cavt.Coex.L2, ChileArgentina.Coex.tCAV.Dist.SD, ChileMexico.Cavt.Coex.L2, ChileMexico.Coex.tCAV.Dist.SD,
  ArgentinaMexico.Cavt.Coex.L2, ArgentinaMexico.Coex.tCAV.Dist.SD,
  
  "Empirical and CAViaR", BrazilChile.CavEmp.Coex.L2, BrazilChile.Coex.EmpCAV.Dist.SD, 
  BrazilArgentina.CavEmp.Coex.L2, BrazilArgentina.Coex.EmpCAV.Dist.SD, BrazilMexico.CavEmp.Coex.L2, BrazilMexico.Coex.EmpCAV.Dist.SD,
  ChileArgentina.CavEmp.Coex.L2, ChileArgentina.Coex.EmpCAV.Dist.SD, ChileMexico.CavEmp.Coex.L2, ChileMexico.Coex.EmpCAV.Dist.SD,
  ArgentinaMexico.CavEmp.Coex.L2, ArgentinaMexico.Coex.EmpCAV.Dist.SD
), 13, 4)
























