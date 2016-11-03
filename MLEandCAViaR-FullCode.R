################################################################################################################################
################################################################################################################################
############################# STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - ECON6910 ############################
################################################################################################################################
################################################################################################################################
setwd("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/MLE and CAViaR/")
#####
library(MASS)
library(copula)
library(numDeriv)
library(R2Cuba)
########################################
### SOURCE OF ARMA-GARCH LH AND PIT ####
########################################
source("Functions/ARMA10-LogLH.R")
source("Functions/ARMA11-LogLH.R")
source("Functions/GARCH10-LogLH.R")
source("Functions/GARCH11-LogLH.R")
source("Functions/CAPPGARCH-LogLH.R")
#
source("Functions/ARMA10-PIT.R")
source("Functions/ARMA11-PIT.R")
source("Functions/GARCH10-PIT.R")
source("Functions/GARCH11-PIT.R")
source("Functions/CAPPGARCH-PIT.R")
source("Functions/CAPPGARCH-sigma.R")
#
source("Functions/CAPP-Joint-MLE.R")
source("Functions/Indep-Joint-MLE.R")
#
source("Functions/CAViaRCheckLoss.R")
source("Functions/CAViaROptimCaller.R")
source("Functions/CAViaRQuantiles.R")
source("Functions/CAViaR-PIT.R")
source("Functions/CAViaR-Indicator-LTGT.R")
source("Functions/CAViaR-Indicator-LT.R")
source("Functions/CAViaR-JointProb.R")
source("Functions/CAViaR-CondProb.R")
#
source("Functions/AICBIC.R")
source("Functions/TablePrint.R")
########################################
############ RETURNS IMPORT ############
########################################
Braziltemp <- read.csv("ReturnsData/BrazilReturns.csv", header = T)
Chiletemp <- read.csv("ReturnsData/ChileReturns.csv", header = T)
Argentinatemp <- read.csv("ReturnsData/ArgentinaReturns.csv", header = T)
Mexicotemp <- read.csv("ReturnsData/MexicoReturns.csv", header = T)
#
BrazilReturns <- matrix(0:0, nrow(Braziltemp), 2)
BrazilReturns[,1] <- Braziltemp[,"V1"]
BrazilReturns[,2] <- Braziltemp[,"V2"]
#
ChileReturns <- matrix(0:0, nrow(Chiletemp), 2)
ChileReturns[,1] <- Chiletemp[,"V1"]
ChileReturns[,2] <- Chiletemp[,"V2"]
#
ArgentinaReturns <- matrix(0:0, nrow(Argentinatemp), 2)
ArgentinaReturns[,1] <- Argentinatemp[,"V1"]
ArgentinaReturns[,2] <- Argentinatemp[,"V2"]
#
MexicoReturns <- matrix(0:0, nrow(Mexicotemp), 2)
MexicoReturns[,1] <- Mexicotemp[,"V1"]
MexicoReturns[,2] <- Mexicotemp[,"V2"]
#
################################################################################################################################
############################################ JOINT MAXIMUM LIKELIHOOD ESTIMATION ###############################################
################################################################################################################################
NormCop4d <- ellipCopula(family = "normal", dim = 4, dispstr = "un")
#
ClaytonCop4d <- archmCopula(family = "clayton", dim = 4)
#
GumbelCop4d <- archmCopula(family = "gumbel", dim = 4)
#
tCop4d <- ellipCopula(family = "t", dim = 4, dispstr = "un")
#
####################
# GAUSSIAN JOINT MLE
####################
# Gaussian.CappJoint <- optim(par = c(cc[1:26]), fn = CappJointMLE, method = "L-BFGS-B",
#                        lower = c(0, -100000, 0, 0, 0,
#                                  0, -100000, 0, 0, 0,
#                                  0, -100000, 0, 0, 0,
#                                  0, -100000, 0, 0, 0,
#                                  0,0,0,0,0,0), 
#                        upper = c(200000, 100000, 200000, 100000, 100000,
#                                  200000, 100000, 200000, 100000, 100000,
#                                  200000, 100000, 200000, 100000, 100000,
#                                  200000, 100000, 200000, 100000, 100000,
#                                  100000,100000,100000,100000,100000,100000), 
#                        Brazil = BrazilReturns, Chile = ChileReturns, Argentina = ArgentinaReturns, Mexico = MexicoReturns, Copula = NormCop4d,
#                        control = list(trace = 3, fnscale = (-1), maxit = 25000), hessian = T)
# #####################
# # Student-t JOINT MLE
# #####################
# t.CappJoint <- optim(par = c(cc), fn = CappJointMLE, method = "L-BFGS-B",
#                             lower = c(0, -100000, 0, 0, 0,
#                                       0, -100000, 0, 0, 0,
#                                       0, -100000, 0, 0, 0,
#                                       0, -100000, 0, 0, 0,
#                                       0,0,0,0,0,0,0), 
#                             upper = c(200000, 100000, 200000, 100000, 100000,
#                                       200000, 100000, 200000, 100000, 100000,
#                                       200000, 100000, 200000, 100000, 100000,
#                                       200000, 100000, 200000, 100000, 100000,
#                                       100000,100000,100000,100000,100000,100000,10000000), 
#                             Brazil = BrazilReturns, Chile = ChileReturns, Argentina = ArgentinaReturns, Mexico = MexicoReturns, Copula = tCop4d,
#                             control = list(trace = 3, fnscale = (-1), maxit = 25000), hessian = T)
# #####################
# # Clayton JOINT MLE
# #####################
# clayton.CappJoint <- optim(par = c(cc[1:20], 500000), fn = CappJointMLE, method = "L-BFGS-B",
#                      lower = c(0, -100000, 0, 0, 0,
#                                0, -100000, 0, 0, 0,
#                                0, -100000, 0, 0, 0,
#                                0, -100000, 0, 0, 0,0), 
#                      upper = c(200000, 100000, 200000, 100000, 100000,
#                                200000, 100000, 200000, 100000, 100000,
#                                200000, 100000, 200000, 100000, 100000,
#                                200000, 100000, 200000, 100000, 100000,10000000), 
#                      Brazil = BrazilReturns, Chile = ChileReturns, Argentina = ArgentinaReturns, Mexico = MexicoReturns, Copula = ClaytonCop4d,
#                      control = list(trace = 3, fnscale = (-1), maxit = 25000), hessian = T)
# #####################
# # Gumbel JOINT MLE
# #####################
# gumbel.CappJoint <- optim(par = c(cc[1:20], 500000), fn = CappJointMLE, method = "L-BFGS-B",
#                      lower = c(0, -100000, 0, 0, 0,
#                                0, -100000, 0, 0, 0,
#                                0, -100000, 0, 0, 0,
#                                0, -100000, 0, 0, 0,0), 
#                      upper = c(200000, 100000, 200000, 100000, 100000,
#                                200000, 100000, 200000, 100000, 100000,
#                                200000, 100000, 200000, 100000, 100000,
#                                200000, 100000, 200000, 100000, 100000,10000000), 
#                      Brazil = BrazilReturns, Chile = ChileReturns, Argentina = ArgentinaReturns, Mexico = MexicoReturns, Copula = GumbelCop4d,
#                      control = list(trace = 3, fnscale = (-1), maxit = 25000), hessian = T)
# #####################
# # Indep JOINT MLE
# #####################
# indep.CappJoint <- optim(par = c(cc[1:20]), fn = IndepJointMLE, method = "L-BFGS-B",
#                           lower = c(0, -100000, 0, 0, 0,
#                                     0, -100000, 0, 0, 0,
#                                     0, -100000, 0, 0, 0,
#                                     0, -100000, 0, 0, 0), 
#                           upper = c(200000, 100000, 200000, 100000, 100000,
#                                     200000, 100000, 200000, 100000, 100000,
#                                     200000, 100000, 200000, 100000, 100000,
#                                     200000, 100000, 200000, 100000, 100000), 
#                           Brazil = BrazilReturns, Chile = ChileReturns, Argentina = ArgentinaReturns, Mexico = MexicoReturns,
#                           control = list(trace = 3, fnscale = (-1), maxit = 25000), hessian = T)
# save.image()
#########################################################################################################
########################################## AICBIC ESTIMATION ############################################
#########################################################################################################
BrazilChileReturns <- merge(BrazilReturns, ChileReturns, by = 1)
#
ArgentinaMexicoReturns <- merge(ArgentinaReturns, MexicoReturns, by = 1)
#
AllReturns <- merge(BrazilChileReturns, ArgentinaMexicoReturns, by = 1)
#
TotalN <- nrow(AllReturns)
########
Gaussian.IC <- AICBIC(LL = Gaussian.CappJoint$value, n = (TotalN * 4), k = 26)
#
t.IC <- AICBIC(LL = t.CappJoint$value, n = (TotalN * 4), k = 27)
#
gumbel.IC <- AICBIC(LL = gumbel.CappJoint$value, n = (TotalN * 4), k = 21)
#
clayton.IC <- AICBIC(LL = clayton.CappJoint$value, n = (TotalN * 4), k = 21)
#
indep.IC <- AICBIC(LL = indep.CappJoint$value, n = (TotalN * 4), k = 20)
#
#########################################################################################################
########################################## SPEARMAN ESTIMATION ##########################################
#########################################################################################################
NormalIntegrateReturns <- function(data){
  MyLittleNormalCopula <-  ellipCopula(family = "normal", dim = 4, dispstr = "un",  param = (Gaussian.CappJoint$par[21:26]/100000))
  Prod <- pCopula(data, copula = MyLittleNormalCopula)
  return(Prod)
}
#
NormalIntegration <- cuhre(ndim = 4, ncomp = 1, integrand =  NormalIntegrateReturns, lower = rep(0,4), upper = rep(1,4))
##################
ClaytonIntegrateReturns <- function(data){
  MyLittleClaytonCopula <-  ClaytonCop4d <- archmCopula(family = "clayton", dim = 4,  param = (clayton.CappJoint$par[21]/100000))
  Prod <- pCopula(data, copula = MyLittleClaytonCopula)
  return(Prod)
}
#
ClaytonIntegration <- cuhre(ndim = 4, ncomp = 1, integrand =  ClaytonIntegrateReturns, lower = rep(0,4), upper = rep(1,4))
##################
GumbelIntegrateReturns <- function(data){
  MyLittleGumbelCopula <-  GumbelCop4d <- archmCopula(family = "gumbel", dim = 4,  param = (gumbel.CappJoint$par[21]/100000))
  Prod <- pCopula(data, copula = MyLittleGumbelCopula)
  return(Prod)
}
#
GumbelIntegration <- cuhre(ndim = 4, ncomp = 1, integrand =  GumbelIntegrateReturns, lower = rep(0,4), upper = rep(1,4))
##################
tIntegrateReturns <- function(data){
  MyLittletCopula <-  tCop4d <- ellipCopula(family = "t", dim = 4, dispstr = "un",  param = (t.CappJoint$par[21:26]/100000), df = floor((t.CappJoint$par[27]/100000)))
  Prod <- pCopula(data, copula = MyLittletCopula)
  return(Prod)
}
#
tIntegration <- cuhre(ndim = 4, ncomp = 1, integrand =  tIntegrateReturns, lower = rep(0,4), upper = rep(1,4))

##################
d <- 4
hd <- (d + 1) / ((2 ^ d) - (d + 1))
#
GaussianSpearman <- hd * (((2 ^ d) * NormalIntegration$value) - 1)
#
ClaytonSpearman <- hd * (((2 ^ d) * ClaytonIntegration$value) - 1)
#
GumbelSpearman <- hd * (((2 ^ d) * GumbelIntegration$value) - 1)
#
tSpearman <- hd * (((2 ^ d) * tIntegration$value) - 1)

#####
#########################################################################################################
########################################### CAViaR ESTIMATION ###########################################
# #########################################################################################################
# TauCount <- 999
# #
# BrazilCAViaR <- CAViaRnlmCheckLoss(BrazilReturns, TauCount)
# #
# ChileCAViaR <- CAViaRnlmCheckLoss(ChileReturns, TauCount)
# #
# ArgentinaCAViaR <- CAViaRnlmCheckLoss(ArgentinaReturns, TauCount)
# #
# MexicoCAViaR <- CAViaRnlmCheckLoss(MexicoReturns, TauCount)
# #
# save.image()
#########################
# CONDITIONAL QUANTILES #
#########################
BrazilQuantiles <- CAViaRQuantiles(BrazilCAViaR, BrazilReturns, TauCount)
#
ChileQuantiles <- CAViaRQuantiles(ChileCAViaR, ChileReturns, TauCount)
#
ArgentinaQuantiles <- CAViaRQuantiles(ArgentinaCAViaR, ArgentinaReturns, TauCount)
#
MexicoQuantiles <- CAViaRQuantiles(MexicoCAViaR, MexicoReturns, TauCount)
#
save.image()
####################
# INDICATOR TABLES #
####################
BrazilIndic.LTGT <- CAViaRIndicatorLTGT(Quantiles = BrazilQuantiles, Returns = BrazilReturns)
BrazilIndic.LT <- CAViaRIndicatorLT(Quantiles = BrazilQuantiles, Returns = BrazilReturns)
###
ChileIndic.LTGT <- CAViaRIndicatorLTGT(Quantiles = ChileQuantiles, Returns = ChileReturns)
ChileIndic.LT <- CAViaRIndicatorLT(Quantiles = ChileQuantiles, Returns = ChileReturns)
###
ArgentinaIndic.LTGT <- CAViaRIndicatorLTGT(Quantiles = ArgentinaQuantiles, Returns = ArgentinaReturns)
ArgentinaIndic.LT <- CAViaRIndicatorLT(Quantiles = ArgentinaQuantiles, Returns = ArgentinaReturns)
###
MexicoIndic.LTGT <- CAViaRIndicatorLTGT(Quantiles = MexicoQuantiles, Returns = MexicoReturns)
MexicoIndic.LT <- CAViaRIndicatorLT(Quantiles = MexicoQuantiles, Returns = MexicoReturns)
###
#####################
# JOINT PROB TABLES #
#####################
#
BrazilChile.LT <- CAViaR.JointProbFunction(IndicatorA = BrazilIndic.LT, IndicatorB =  ChileIndic.LT, Tau = c(1:TauCount))
#
BrazilArgentina.LT <- CAViaR.JointProbFunction(IndicatorA = BrazilIndic.LT, IndicatorB =  ArgentinaIndic.LT, Tau = c(1:TauCount))
#
BrazilMexico.LT <- CAViaR.JointProbFunction(IndicatorA = BrazilIndic.LT, IndicatorB =  MexicoIndic.LT, Tau = c(1:TauCount))
#
ChileArgentina.LT <- CAViaR.JointProbFunction(IndicatorA = ChileIndic.LT, IndicatorB =  ArgentinaIndic.LT, Tau = c(1:TauCount))
#
ChileMexico.LT <- CAViaR.JointProbFunction(IndicatorA = ChileIndic.LT, IndicatorB =  MexicoIndic.LT, Tau = c(1:TauCount))
#
ArgentinaMexico.LT <- CAViaR.JointProbFunction(IndicatorA = ArgentinaIndic.LT, IndicatorB =  MexicoIndic.LT, Tau = c(1:TauCount))
####
#
###
BrazilChile.LTGT <- CAViaR.JointProbFunction(IndicatorA = BrazilIndic.LTGT, IndicatorB =  ChileIndic.LTGT, Tau = c(1:TauCount))
#
BrazilArgentina.LTGT <- CAViaR.JointProbFunction(IndicatorA = BrazilIndic.LTGT, IndicatorB =  ArgentinaIndic.LTGT, Tau = c(1:TauCount))
#
BrazilMexico.LTGT <- CAViaR.JointProbFunction(IndicatorA = BrazilIndic.LTGT, IndicatorB =  MexicoIndic.LTGT, Tau = c(1:TauCount))
#
ChileArgentina.LTGT <- CAViaR.JointProbFunction(IndicatorA = ChileIndic.LTGT, IndicatorB =  ArgentinaIndic.LTGT, Tau = c(1:TauCount))
#
ChileMexico.LTGT <- CAViaR.JointProbFunction(IndicatorA = ChileIndic.LTGT, IndicatorB =  MexicoIndic.LTGT, Tau = c(1:TauCount))
#
ArgentinaMexico.LTGT <- CAViaR.JointProbFunction(IndicatorA = ArgentinaIndic.LTGT, IndicatorB =  MexicoIndic.LTGT, Tau = c(1:TauCount))
####
#
###
BrazilChile.Coex <- CAViaR.CondProb(JointProb = BrazilChile.LTGT, ConditionerIndicator = ChileIndic.LTGT, Tau = c(1:TauCount))
#
BrazilArgentina.Coex <- CAViaR.CondProb(JointProb = BrazilArgentina.LTGT, ConditionerIndicator = ArgentinaIndic.LTGT, Tau = c(1:TauCount))
#
BrazilMexico.Coex <- CAViaR.CondProb(JointProb = BrazilMexico.LTGT, ConditionerIndicator = MexicoIndic.LTGT, Tau = c(1:TauCount))
#
ChileArgentina.Coex <- CAViaR.CondProb(JointProb = ChileArgentina.LTGT, ConditionerIndicator = ArgentinaIndic.LTGT, Tau = c(1:TauCount))
#
ChileMexico.Coex <- CAViaR.CondProb(JointProb = ChileMexico.LTGT, ConditionerIndicator = MexicoIndic.LTGT, Tau = c(1:TauCount))
#
ArgentinaMexico.Coex <- CAViaR.CondProb(JointProb = ArgentinaMexico.LTGT, ConditionerIndicator = MexicoIndic.LTGT, Tau = c(1:TauCount))
#
save.image()

#########################################################################################################
########################################## INTERNAL FORECASTING #########################################
#########################################################################################################
Brazil.cappU <- cappgarch.PIT(param = t.CappJoint$par[1:5], data = BrazilReturns)
#
Chile.cappU <- cappgarch.PIT(param = t.CappJoint$par[6:10], data = ChileReturns)
#
Argentina.cappU <- cappgarch.PIT(param = t.CappJoint$par[11:15], data = ArgentinaReturns)
#
Mexico.cappU <- cappgarch.PIT(param = t.CappJoint$par[16:20], data = MexicoReturns)
####
#
BrazilChile.cappU <- merge(Brazil.cappU, Chile.cappU, by = 1)
#
BrazilArgentina.cappU <- merge(Brazil.cappU, Argentina.cappU, by = 1)
#
BrazilMexico.cappU <- merge(Brazil.cappU, Mexico.cappU, by = 1)
#
ChileArgentina.cappU <- merge(Chile.cappU, Argentina.cappU, by = 1)
#
ChileMexico.cappU <- merge(Chile.cappU, Mexico.cappU, by = 1)
#
ArgentinaMexico.cappU <- merge(Argentina.cappU, Mexico.cappU, by = 1)
#
All.cappU <- merge(BrazilChile.cappU, ArgentinaMexico.cappU, by = 1)


######
##
######
t.cop.til.you.pop <- ellipCopula(family = "t", param = (t.CappJoint$par[21:26] / 100000), df = (t.CappJoint$par[27]/ 100000), 
                                 dispstr = "un", dim = 4)
#

############
############
save.image()
######
##
######
Brazil.caviarU <- matrix(0:0, nrow(BrazilIndic.LT), 2)
Brazil.caviarU[,1] <- BrazilIndic.LT[,1]
for (i in 1:nrow(Brazil.caviarU)){
  Brazil.caviarU[i,2] <- sum(BrazilIndic.LT[i,2:ncol(BrazilIndic.LT)]) / (TauCount + 1)
}
#
Chile.caviarU <- matrix(0:0, nrow(ChileIndic.LT), 2)
Chile.caviarU[,1] <- ChileIndic.LT[,1]
for (i in 1:nrow(Chile.caviarU)){
  Chile.caviarU[i,2] <- sum(ChileIndic.LT[i,2:ncol(ChileIndic.LT)]) / (TauCount + 1)
}
#
Argentina.caviarU <- matrix(0:0, nrow(ArgentinaIndic.LT), 2)
Argentina.caviarU[,1] <- ArgentinaIndic.LT[,1]
for (i in 1:nrow(Argentina.caviarU)){
  Argentina.caviarU[i,2] <- sum(ArgentinaIndic.LT[i,2:ncol(ArgentinaIndic.LT)]) / (TauCount + 1)
}
#
Mexico.caviarU <- matrix(0:0, nrow(MexicoIndic.LT), 2)
Mexico.caviarU[,1] <- MexicoIndic.LT[,1]
for (i in 1:nrow(Mexico.caviarU)){
  Mexico.caviarU[i,2] <- sum(MexicoIndic.LT[i,2:ncol(MexicoIndic.LT)]) / (TauCount + 1)
}
save.image()
######
##
######
BrazilChile.caviarU <- merge(Brazil.caviarU, Chile.caviarU, by = 1)
#
BrazilArgentina.caviarU <- merge(Brazil.caviarU, Argentina.caviarU, by = 1)
#
BrazilMexico.caviarU <- merge(Brazil.caviarU, Mexico.caviarU, by = 1)
#
ChileArgentina.caviarU <- merge(Chile.caviarU, Argentina.caviarU, by = 1)
#
ChileMexico.caviarU <- merge(Chile.caviarU, Mexico.caviarU, by = 1)
#
ArgentinaMexico.caviarU <- merge(Argentina.caviarU, Mexico.caviarU, by = 1)
######
##
######

#########################################################################################################
############################################# STANDARD ERRORS ###########################################
#########################################################################################################
# t.Hessian <- hessian(func = CappJointMLE, x = t.CappJoint$par, Brazil = BrazilReturns, Chile = ChileReturns,
#                      Argentina = ArgentinaReturns, Mexico = MexicoReturns, Copula = tCop4d)
# t.VC <- solve(-t.Hessian)
# t.SE <- matrix(0:0, nrow(t.VC), 1)
# for (i in 1:nrow(t.SE)){
#   t.SE[i,1] <- sqrt(t.VC[i,i]) / 100000
# }
# save.image()
# #######
# gaussian.Hessian <- hessian(func = CappJointMLE, x = Gaussian.CappJoint$par, Brazil = BrazilReturns, Chile = ChileReturns,
#                      Argentina = ArgentinaReturns, Mexico = MexicoReturns, Copula = NormCop4d)
# gaussian.VC <- solve(-gaussian.Hessian)
# gaussian.SE <- matrix(0:0, nrow(gaussian.VC), 1)
# for (i in 1:nrow(gaussian.SE)){ 
#   gaussian.SE[i,1] <- sqrt(gaussian.VC[i,i]) / 100000
# }
# save.image()
# #######
# clayton.Hessian <- hessian(func = CappJointMLE, x = clayton.CappJoint$par, Brazil = BrazilReturns, Chile = ChileReturns,
#                             Argentina = ArgentinaReturns, Mexico = MexicoReturns, Copula = ClaytonCop4d)
# clayton.VC <- solve(-clayton.Hessian)
# clayton.SE <- matrix(0:0, nrow(clayton.VC), 1)
# for (i in 1:nrow(clayton.SE)){ 
#   clayton.SE[i,1] <- sqrt(clayton.VC[i,i]) / 100000
# } 
# save.image()
# #######
# gumbel.Hessian <- hessian(func = CappJointMLE, x = gumbel.CappJoint$par, Brazil = BrazilReturns, Chile = ChileReturns,
#                            Argentina = ArgentinaReturns, Mexico = MexicoReturns, Copula = GumbelCop4d)
# gumbel.VC <- solve(-gumbel.Hessian)
# gumbel.SE <- matrix(0:0, nrow(gumbel.VC), 1)
# for (i in 1:nrow(gumbel.SE)){ 
#   gumbel.SE[i,1] <- sqrt(gumbel.VC[i,i]) / 100000
# } 
# save.image()
# #######
# indep.Hessian <- hessian(func = IndepJointMLE, x = indep.CappJoint$par, Brazil = BrazilReturns, Chile = ChileReturns,
#                           Argentina = ArgentinaReturns, Mexico = MexicoReturns)
# indep.VC <- solve(-indep.Hessian)
# indep.SE <- matrix(0:0, nrow(indep.VC), 1)
# for (i in 1:nrow(indep.SE)){ 
#   indep.SE[i,1] <- sqrt(indep.VC[i,i]) / 100000
# } 
# save.image()
####### 
#######
###
#######
# Empirical Copula ###
# Diagonal Sections ###
  # Distance between diagonals sections ###
# Tail dependence - especially empirical ###

# Symmetry of E(G)
# rename "discrete" to CAViaR
# rewrite formulas, make conditional explicit

# Confidence intervals for coexceedence plots
  # SE from regression for CAViaR ###
  # SE of empirical ###
  # bivariate marginal BS for t ###

# Coexceedences
  # CAViaR, empirical, bivariate margin, ###
  # conditional ###

TablePrint(c(
  "\\hline ", "Days Available", "Min", "Mean", "Max",
  "\\hline \\hline Brazil", nrow(BrazilReturns), min(BrazilReturns[,2]), mean(BrazilReturns[,2]), max(BrazilReturns[,2]),
  "\\hline Chile", nrow(ChileReturns), min(ChileReturns[,2]), mean(ChileReturns[,2]), max(ChileReturns[,2]),
  "\\hline Argentina", nrow(ArgentinaReturns), min(ArgentinaReturns[,2]), mean(ArgentinaReturns[,2]), max(ArgentinaReturns[,2]),
  "\\hline Mexico", nrow(MexicoReturns), min(MexicoReturns[,2]), mean(MexicoReturns[,2]), max(MexicoReturns[,2])
),5,5,5)

TablePrint(c(
  "\\hline ", "\\hline \\hline Brazil", " ", "\\hline Chile", " ", "\\hline Argentina", " ", "\\hline Mexico", " ", 
  "$\\hat{\\beta}_{0}$", BrazilCAViaR[50,1], " ", ChileCAViaR[50,1], " ", ArgentinaCAViaR[50,1], " ", MexicoCAViaR[50,1]," ", 
  "$\\hat{\\beta}_{1}$", BrazilCAViaR[50,2], " ", ChileCAViaR[50,2], " ", ArgentinaCAViaR[50,2], " ", MexicoCAViaR[50,2]," ", 
  "$\\hat{\\beta}_{2}$", BrazilCAViaR[50,3], " ", ChileCAViaR[50,3], " ", ArgentinaCAViaR[50,3], " ", MexicoCAViaR[50,3]," ", 
  "$\\hat{\\beta}_{3}$", BrazilCAViaR[50,4], " ", ChileCAViaR[50,4], " ", ArgentinaCAViaR[50,4], " ",  MexicoCAViaR[50,4], " "
), 9, 5, 4)

TablePrint(c(
  "\\hline ", "\\hline \\hline Brazil", " ", "\\hline Chile", " ", "\\hline Argentina", " ", "\\hline Mexico", " ", 
  "$\\hat{\\beta}_{0}$", BrazilCAViaR[250,1], " ", ChileCAViaR[250,1], " ", ArgentinaCAViaR[250,1], " ", MexicoCAViaR[250,1]," ", 
  "$\\hat{\\beta}_{1}$", BrazilCAViaR[250,2], " ", ChileCAViaR[250,2], " ", ArgentinaCAViaR[250,2], " ", MexicoCAViaR[250,2]," ", 
  "$\\hat{\\beta}_{2}$", BrazilCAViaR[250,3], " ", ChileCAViaR[250,3], " ", ArgentinaCAViaR[250,3], " ", MexicoCAViaR[250,3]," ", 
  "$\\hat{\\beta}_{3}$", BrazilCAViaR[250,4], " ", ChileCAViaR[250,4], " ", ArgentinaCAViaR[250,4], " ",  MexicoCAViaR[250,4], " "
), 9, 5, 4)

TablePrint(c(
  "\\hline ", "\\hline \\hline Brazil", " ", "\\hline Chile", " ", "\\hline Argentina", " ", "\\hline Mexico", " ", 
  "$\\hat{\\beta}_{0}$", BrazilCAViaR[750,1], " ", ChileCAViaR[750,1], " ", ArgentinaCAViaR[750,1], " ", MexicoCAViaR[750,1]," ", 
  "$\\hat{\\beta}_{1}$", BrazilCAViaR[750,2], " ", ChileCAViaR[750,2], " ", ArgentinaCAViaR[750,2], " ", MexicoCAViaR[750,2]," ", 
  "$\\hat{\\beta}_{2}$", BrazilCAViaR[750,3], " ", ChileCAViaR[750,3], " ", ArgentinaCAViaR[750,3], " ", MexicoCAViaR[750,3]," ", 
  "$\\hat{\\beta}_{3}$", BrazilCAViaR[750,4], " ", ChileCAViaR[750,4], " ", ArgentinaCAViaR[750,4], " ",  MexicoCAViaR[750,4], " "
), 9, 5, 4)

TablePrint(c(
  "\\hline ", "\\hline \\hline Brazil", " ", "\\hline Chile", " ", "\\hline Argentina", " ", "\\hline Mexico", " ", 
  "$\\hat{\\beta}_{0}$", BrazilCAViaR[950,1], " ", ChileCAViaR[950,1], " ", ArgentinaCAViaR[950,1], " ", MexicoCAViaR[950,1]," ", 
  "$\\hat{\\beta}_{1}$", BrazilCAViaR[950,2], " ", ChileCAViaR[950,2], " ", ArgentinaCAViaR[950,2], " ", MexicoCAViaR[950,2]," ", 
  "$\\hat{\\beta}_{2}$", BrazilCAViaR[950,3], " ", ChileCAViaR[950,3], " ", ArgentinaCAViaR[950,3], " ", MexicoCAViaR[950,3]," ", 
  "$\\hat{\\beta}_{3}$", BrazilCAViaR[950,4], " ", ChileCAViaR[950,4], " ", ArgentinaCAViaR[950,4], " ",  MexicoCAViaR[950,4], " "
), 9, 5, 4)

TablePrint(c(
  "Copula Specification", "Independent", "Gumbel", 
))



























