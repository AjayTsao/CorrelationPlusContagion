################################################################################################################################
################################################################################################################################
######################### STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - EMPIRICAL COPULA ########################
################################################################################################################################
################################################################################################################################
setwd("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/MLE and CAViaR/")
############
############
############
emp.seq <- seq(from = 0, to = 1, length = 100)
############
############
############
EmpiricalCopulaFunction <- function(PIT){
  emp.cop <- matrix(0:0, length(emp.seq), 6)
  emp.cop[,1] <- emp.seq
  for (i in 2:length(emp.seq)){
    indictemp <- as.numeric((PIT[ ,2] < emp.seq[i]) & (PIT[ ,3] < emp.seq[i]))
    emp.cop[i,2] <- mean(indictemp)
    #
    # ss <- (indictemp - emp.cop[i,2]) ^ 2
    # emp.cop[i,3] <- ((sum(ss) / nrow(PIT)) / nrow(PIT)) ^ 0.5
    # emp.cop[i,4] <- emp.cop[i,2] + (emp.cop[i,3] * qt(0.025, df = sum(indictemp)))
    # emp.cop[i,5] <- emp.cop[i,2] + (emp.cop[i,3] * qt(0.975, df = sum(indictemp)))
    # emp.cop[i,6] <- sum(indictemp)
  }
  return(emp.cop[-1,2])
}
############
############
############
EmpiricalCoexFunction <- function(PIT){
  emp.coex <- matrix(0:0, length(emp.seq), 6)
  emp.coex[,1] <- emp.seq
  #
  for (i in 2:(nrow(emp.coex)-1)){
    ss <- rep(0, nrow(PIT))
    if (emp.coex[i,1] <= 0.5){
      indictemp1 <- as.numeric(PIT[ ,2] < emp.seq[i])
      indictemp2 <- as.numeric(PIT[ ,3] < emp.seq[i])
      indictemp3 <- indictemp1 * indictemp2
      if (mean(indictemp2) != 0) emp.coex[i,2] <- mean(indictemp3) / mean(indictemp2)


      #   ss <- indictemp2 * ((indictemp3 - emp.coex[i,2]) ^ 2)
      # 
      # emp.coex[i,3] <- (((sum(ss)) / sum(indictemp2 == 1)) / sum(indictemp2 == 1)) ^ 0.5
      # emp.coex[i,4] <- emp.coex[i,2] + (emp.coex[i,3] * qt(0.025, df = sum(indictemp2)))
      # emp.coex[i,5] <- emp.coex[i,2] + (emp.coex[i,3] * qt(0.975, df = sum(indictemp2)))
      # emp.coex[i,6] <- sum(indictemp2)
    } 
    if (emp.coex[i,1] > 0.5){
      indictemp1 <- as.numeric(PIT[ ,2] >= emp.seq[i])
      indictemp2 <- as.numeric(PIT[ ,3] >= emp.seq[i])
      indictemp3 <- indictemp1 * indictemp2
      if (mean(indictemp2) != 0) emp.coex[i,2] <- mean(indictemp3) / mean(indictemp2)


        ss <- indictemp2 * ((indictemp3 - emp.coex[i,2]) ^ 2)
# 
#       emp.coex[i,3] <- (((sum(ss)) / sum(indictemp2 == 1)) / sum(indictemp2 == 1)) ^ 0.5
#       emp.coex[i,4] <- emp.coex[i,2] + (emp.coex[i,3] * qt(0.025, df = sum(indictemp2)))
#       emp.coex[i,5] <- emp.coex[i,2] + (emp.coex[i,3] * qt(0.975, df = sum(indictemp2)))
#       emp.coex[i,6] <- sum(indictemp2)
    } 
    #
  }
  return(emp.coex[2:(nrow(emp.coex)-1),2])
}
  


############
############
############
BrazilChile.Emp.Cop <- EmpiricalCopulaFunction(BrazilChile.cappU)
#
BrazilArgentina.Emp.Cop <- EmpiricalCopulaFunction(BrazilArgentina.cappU)
#
BrazilMexico.Emp.Cop <- EmpiricalCopulaFunction(BrazilMexico.cappU)
#
ChileArgentina.Emp.Cop <- EmpiricalCopulaFunction(ChileArgentina.cappU)
#
ChileMexico.Emp.Cop <- EmpiricalCopulaFunction(ChileMexico.cappU)
#
ArgentinaMexico.Emp.Cop <- EmpiricalCopulaFunction(ArgentinaMexico.cappU)
#
############
############
############
BrazilChile.Emp.Coex <- EmpiricalCoexFunction(BrazilChile.cappU)
#
BrazilArgentina.Emp.Coex <- EmpiricalCoexFunction(BrazilArgentina.cappU)
#
BrazilMexico.Emp.Coex <- EmpiricalCoexFunction(BrazilMexico.cappU)
#
ChileArgentina.Emp.Coex <- EmpiricalCoexFunction(ChileArgentina.cappU)
#
ChileMexico.Emp.Coex <- EmpiricalCoexFunction(ChileMexico.cappU)
#
ArgentinaMexico.Emp.Coex <- EmpiricalCoexFunction(ArgentinaMexico.cappU)
########################
########################
BrazilChile.Emp.Cop.BSSE <-  boot(data = BrazilChile.cappU, statistic = EmpiricalCopulaFunction, R = 1000,  stype = "i")
#
BrazilArgentina.Emp.Cop.BSSE <-  boot(data = BrazilArgentina.cappU, statistic = EmpiricalCopulaFunction, R = 1000,  stype = "i")
#
BrazilMexico.Emp.Cop.BSSE <-  boot(data = BrazilMexico.cappU, statistic = EmpiricalCopulaFunction, R = 1000,  stype = "i")
#
ChileArgentina.Emp.Cop.BSSE <-  boot(data = ChileArgentina.cappU, statistic = EmpiricalCopulaFunction, R = 1000,  stype = "i")
#
ChileMexico.Emp.Cop.BSSE <-  boot(data = ChileMexico.cappU, statistic = EmpiricalCopulaFunction, R = 1000,  stype = "i")
#
ArgentinaMexico.Emp.Cop.BSSE <-  boot(data = ArgentinaMexico.cappU, statistic = EmpiricalCopulaFunction, R = 1000,  stype = "i")
########################
########################
BrazilChile.Emp.Coex.BSSE <-  boot(data = BrazilChile.cappU, statistic = EmpiricalCoexFunction, R = 1000,  stype = "i")
#
BrazilArgentina.Emp.Coex.BSSE <-  boot(data = BrazilArgentina.cappU, statistic = EmpiricalCoexFunction, R = 1000,  stype = "i")
#
BrazilMexico.Emp.Coex.BSSE <-  boot(data = BrazilMexico.cappU, statistic = EmpiricalCoexFunction, R = 1000,  stype = "i")
#
ChileArgentina.Emp.Coex.BSSE <-  boot(data = ChileArgentina.cappU, statistic = EmpiricalCoexFunction, R = 1000,  stype = "i")
#
ChileMexico.Emp.Coex.BSSE <-  boot(data = ChileMexico.cappU, statistic = EmpiricalCoexFunction, R = 1000,  stype = "i")
#
ArgentinaMexico.Emp.Coex.BSSE <-  boot(data = ArgentinaMexico.cappU, statistic = EmpiricalCoexFunction, R = 1000,  stype = "i")
########################
########################
BrazilChile.Emp.Cop.CI <- matrix(0:0, 2, 1899)
BrazilChile.Emp.Coex.CI <- matrix(0:0, 2, 1899)
BrazilArgentina.Emp.Cop.CI <- matrix(0:0, 2, 1899)
BrazilArgentina.Emp.Coex.CI <- matrix(0:0, 2, 1899)
BrazilMexico.Emp.Cop.CI <- matrix(0:0, 2, 1899)
BrazilMexico.Emp.Coex.CI <- matrix(0:0, 2, 1899)
ChileArgentina.Emp.Cop.CI <- matrix(0:0, 2, 1899)
ChileArgentina.Emp.Coex.CI <- matrix(0:0, 2, 1899)
ChileMexico.Emp.Cop.CI <- matrix(0:0, 2, 1899)
ChileMexico.Emp.Coex.CI <- matrix(0:0, 2, 1899)
ArgentinaMexico.Emp.Cop.CI <- matrix(0:0, 2, 1899)
ArgentinaMexico.Emp.Coex.CI <- matrix(0:0, 2, 1899)
#
for (i in 1:1899){
  BrazilChile.Emp.Cop.CI[1,i] <- BrazilChile.Emp.Cop[i,2] + 3*sd(BrazilChile.Emp.Cop.BSSE$t[,i]) * qt(0.975, df = BrazilChile.Emp.Cop[i,6])
  BrazilArgentina.Emp.Cop.CI[1,i] <- BrazilArgentina.Emp.Cop[i,2] + 3*sd(BrazilArgentina.Emp.Cop.BSSE$t[,i]) * qt(0.975, df = BrazilArgentina.Emp.Cop[i,6])
  BrazilMexico.Emp.Cop.CI[1,i] <- BrazilMexico.Emp.Cop[i,2] + 3*sd(BrazilMexico.Emp.Cop.BSSE$t[,i]) * qt(0.975, df = BrazilMexico.Emp.Cop[i,6])
  ChileArgentina.Emp.Cop.CI[1,i] <- ChileArgentina.Emp.Cop[i,2] + 3*sd(ChileArgentina.Emp.Cop.BSSE$t[,i]) * qt(0.975, df = ChileArgentina.Emp.Cop[i,6])
  ChileMexico.Emp.Cop.CI[1,i] <- ChileMexico.Emp.Cop[i,2] + 3*sd(ChileMexico.Emp.Cop.BSSE$t[,i]) * qt(0.975, df = ChileMexico.Emp.Cop[i,6])
  ArgentinaMexico.Emp.Cop.CI[1,i] <- ArgentinaMexico.Emp.Cop[i,2] + 3* sd(ArgentinaMexico.Emp.Cop.BSSE$t[,i]) * qt(0.975, df = ArgentinaMexico.Emp.Cop[i,6])
  #
  BrazilChile.Emp.Cop.CI[2,i] <- BrazilChile.Emp.Cop[i,2] + 3*sd(BrazilChile.Emp.Cop.BSSE$t[,i]) * qt(0.025, df = BrazilChile.Emp.Cop[i,6])
  BrazilArgentina.Emp.Cop.CI[2,i] <- BrazilArgentina.Emp.Cop[i,2] + 3*sd(BrazilArgentina.Emp.Cop.BSSE$t[,i]) * qt(0.025, df = BrazilArgentina.Emp.Cop[i,6])
  BrazilMexico.Emp.Cop.CI[2,i] <- BrazilMexico.Emp.Cop[i,2] + 3*sd(BrazilMexico.Emp.Cop.BSSE$t[,i]) * qt(0.025, df = BrazilMexico.Emp.Cop[i,6])
  ChileArgentina.Emp.Cop.CI[2,i] <- ChileArgentina.Emp.Cop[i,2] + 3*sd(ChileArgentina.Emp.Cop.BSSE$t[,i]) * qt(0.025, df = ChileArgentina.Emp.Cop[i,6])
  ChileMexico.Emp.Cop.CI[2,i] <- ChileMexico.Emp.Cop[i,2] + 3*sd(ChileMexico.Emp.Cop.BSSE$t[,i]) * qt(0.025, df = ChileMexico.Emp.Cop[i,6])
  ArgentinaMexico.Emp.Cop.CI[2,i] <- ArgentinaMexico.Emp.Cop[i,2] + 3*sd(ArgentinaMexico.Emp.Cop.BSSE$t[,i]) * qt(0.025, df = ArgentinaMexico.Emp.Cop[i,6])
}
for (i in 2:1898){
BrazilChile.Emp.Coex.CI[1,i] <- BrazilChile.Emp.Coex[i,2] + sd(BrazilChile.Emp.Coex.BSSE$t[,i])  * qt(0.975, df = BrazilChile.Emp.Coex[i,6])
BrazilArgentina.Emp.Coex.CI[1,i] <- BrazilArgentina.Emp.Coex[i,2] + sd(BrazilArgentina.Emp.Coex.BSSE$t[,i])  * qt(0.975, df = BrazilArgentina.Emp.Coex[i,6])
BrazilMexico.Emp.Coex.CI[1,i] <- BrazilMexico.Emp.Coex[i,2] + sd(BrazilMexico.Emp.Coex.BSSE$t[,i])  * qt(0.975, df = BrazilMexico.Emp.Coex[i,6])
ChileArgentina.Emp.Coex.CI[1,i] <- ChileArgentina.Emp.Coex[i,2] + sd(ChileArgentina.Emp.Coex.BSSE$t[,i])  * qt(0.975, df = ChileArgentina.Emp.Coex[i,6])
ChileMexico.Emp.Coex.CI[1,i] <- ChileMexico.Emp.Coex[i,2] + sd(ChileMexico.Emp.Coex.BSSE$t[,i])  * qt(0.975, df = ChileMexico.Emp.Coex[i,6])
ArgentinaMexico.Emp.Coex.CI[1,i] <- ArgentinaMexico.Emp.Coex[i,2] +  sd(ArgentinaMexico.Emp.Coex.BSSE$t[,i])  * qt(0.975, df = ArgentinaMexico.Emp.Coex[i,6])
#
BrazilChile.Emp.Coex.CI[2,i] <-  BrazilChile.Emp.Coex[i,2] + sd(BrazilChile.Emp.Coex.BSSE$t[,i])  * qt(0.025, df = BrazilChile.Emp.Coex[i,6])
BrazilArgentina.Emp.Coex.CI[2,i] <- BrazilArgentina.Emp.Coex[i,2] + sd(BrazilArgentina.Emp.Coex.BSSE$t[,i])  * qt(0.025, df = BrazilArgentina.Emp.Coex[i,6])
BrazilMexico.Emp.Coex.CI[2,i] <- BrazilMexico.Emp.Coex[i,2] + sd(BrazilMexico.Emp.Coex.BSSE$t[,i])  * qt(0.025, df = BrazilMexico.Emp.Coex[i,6])
ChileArgentina.Emp.Coex.CI[2,i] <- ChileArgentina.Emp.Coex[i,2] + sd(ChileArgentina.Emp.Coex.BSSE$t[,i])  * qt(0.025, df = ChileArgentina.Emp.Coex[i,6])
ChileMexico.Emp.Coex.CI[2,i] <- ChileMexico.Emp.Coex[i,2] + sd(ChileMexico.Emp.Coex.BSSE$t[,i])  * qt(0.025, df = ChileMexico.Emp.Coex[i,6])
ArgentinaMexico.Emp.Coex.CI[2,i] <- ArgentinaMexico.Emp.Coex[i,2] +  sd(ArgentinaMexico.Emp.Coex.BSSE$t[,i])  * qt(0.025, df = ArgentinaMexico.Emp.Coex[i,6])
}













