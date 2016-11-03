CAViaRnlmBootstrapCaller <- function(ReturnsSeries, inds, Tau){
  EstimateOutput <- rep(0,4)
  estimate <- optim(par = c(0,0,0,0), CAViaRLoss, Returns = ReturnsSeries[inds,], Tau = Tau)
  EstimateOutput[1] <- estimate$par[1]
  EstimateOutput[2] <- estimate$par[2]
  EstimateOutput[3] <- estimate$par[3]
  EstimateOutput[4] <- estimate$par[4]
 
  return(EstimateOutput)
}
####################
####################
library(boot)
#
BrazilCAViaR.BS.05 <- boot(data = BrazilReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.05)
#
ChileCAViaR.BS.05 <- boot(data = ChileReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.05)
#
ArgentinaCAViaR.BS.05 <- boot(data = ArgentinaReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.05)
#
MexicoCAViaR.BS.05 <- boot(data = MexicoReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.05)
#
##########
BrazilCAViaR.BS.10 <- boot(data = BrazilReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.10)
#
ChileCAViaR.BS.10 <- boot(data = ChileReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.10)
#
ArgentinaCAViaR.BS.10 <- boot(data = ArgentinaReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.10)
#
MexicoCAViaR.BS.10 <- boot(data = MexicoReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.10)
##############
BrazilCAViaR.BS.25 <- boot(data = BrazilReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.25)
#
ChileCAViaR.BS.25 <- boot(data = ChileReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.25)
#
ArgentinaCAViaR.BS.25 <- boot(data = ArgentinaReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.25)
#
MexicoCAViaR.BS.25 <- boot(data = MexicoReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.25)
###########
BrazilCAViaR.BS.50 <- boot(data = BrazilReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.50)
#
ChileCAViaR.BS.50 <- boot(data = ChileReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.50)
#
ArgentinaCAViaR.BS.50 <- boot(data = ArgentinaReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.50)
#
MexicoCAViaR.BS.50 <- boot(data = MexicoReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.50)
###########
BrazilCAViaR.BS.75 <- boot(data = BrazilReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.75)
#
ChileCAViaR.BS.75 <- boot(data = ChileReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.75)
#
ArgentinaCAViaR.BS.75 <- boot(data = ArgentinaReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.75)
#
MexicoCAViaR.BS.75 <- boot(data = MexicoReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.75)
###########
BrazilCAViaR.BS.90 <- boot(data = BrazilReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.90)
#
ChileCAViaR.BS.90 <- boot(data = ChileReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.90)
#
ArgentinaCAViaR.BS.90 <- boot(data = ArgentinaReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.90)
#
MexicoCAViaR.BS.90 <- boot(data = MexicoReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.90)
#############
BrazilCAViaR.BS.95 <- boot(data = BrazilReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.95)
#
ChileCAViaR.BS.95 <- boot(data = ChileReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.95)
#
ArgentinaCAViaR.BS.95 <- boot(data = ArgentinaReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.95)
#
MexicoCAViaR.BS.95 <- boot(data = MexicoReturns, statistic = CAViaRnlmBootstrapCaller, R = 750, stype = "i", Tau = 0.95)
##################
##################
BrazilCAViaR.BSSE.05 <- rep(0, 4)
ChileCAViaR.BSSE.05 <- rep(0, 4)
ArgentinaCAViaR.BSSE.05 <- rep(0, 4)
MexicoCAViaR.BSSE.05 <- rep(0, 4)
#
BrazilCAViaR.BSSE.25 <- rep(0, 4)
ChileCAViaR.BSSE.25 <- rep(0, 4)
ArgentinaCAViaR.BSSE.25 <- rep(0, 4)
MexicoCAViaR.BSSE.25 <- rep(0, 4)
#
BrazilCAViaR.BSSE.75 <- rep(0, 4)
ChileCAViaR.BSSE.75 <- rep(0, 4)
ArgentinaCAViaR.BSSE.75 <- rep(0, 4)
MexicoCAViaR.BSSE.75 <- rep(0, 4)
#
BrazilCAViaR.BSSE.95 <- rep(0, 4)
ChileCAViaR.BSSE.95 <- rep(0, 4)
ArgentinaCAViaR.BSSE.95 <- rep(0, 4)
MexicoCAViaR.BSSE.95 <- rep(0, 4)
#
for (i in 1:4){
  BrazilCAViaR.BSSE.05[i] <- sd(BrazilCAViaR.BS.05$t[,i])
  ChileCAViaR.BSSE.05[i] <- sd(ChileCAViaR.BS.05$t[,i])
  ArgentinaCAViaR.BSSE.05[i] <- sd(ArgentinaCAViaR.BS.05$t[,i])
  MexicoCAViaR.BSSE.05[i] <- sd(MexicoCAViaR.BS.05$t[,i])
  #
  BrazilCAViaR.BSSE.25[i] <- sd(BrazilCAViaR.BS.25$t[,i])
  ChileCAViaR.BSSE.25[i] <- sd(ChileCAViaR.BS.25$t[,i])
  ArgentinaCAViaR.BSSE.25[i] <- sd(ArgentinaCAViaR.BS.25$t[,i])
  MexicoCAViaR.BSSE.25[i] <- sd(MexicoCAViaR.BS.25$t[,i])
  #
  BrazilCAViaR.BSSE.75[i] <- sd(BrazilCAViaR.BS.75$t[,i])
  ChileCAViaR.BSSE.75[i] <- sd(ChileCAViaR.BS.75$t[,i])
  ArgentinaCAViaR.BSSE.75[i] <- sd(ArgentinaCAViaR.BS.75$t[,i])
  MexicoCAViaR.BSSE.75[i] <- sd(MexicoCAViaR.BS.75$t[,i])
  #
  BrazilCAViaR.BSSE.95[i] <- sd(BrazilCAViaR.BS.95$t[,i])
  ChileCAViaR.BSSE.95[i] <- sd(ChileCAViaR.BS.95$t[,i])
  ArgentinaCAViaR.BSSE.95[i] <- sd(ArgentinaCAViaR.BS.95$t[,i])
  MexicoCAViaR.BSSE.95[i] <- sd(MexicoCAViaR.BS.95$t[,i])
}

TablePrint(c( "\\hline $\\boldsymbol{\\tau}\\mathbf{=0.05}$", "\\hline \\hline $\\hat{\\beta}_{0}$", " ", "$\\hat{\\beta}_{1}$", " ", 
              "$\\hat{\\beta}_{2}$", " ", "$\\hat{\\beta}_{3}$", " ", 
              "\\hline \\hline $\\boldsymbol{\\tau}\\mathbf{=0.25}$", "\\hline \\hline $\\hat{\\beta}_{0}$", " ", "$\\hat{\\beta}_{1}$", " ", 
              "$\\hat{\\beta}_{2}$", " ", "$\\hat{\\beta}_{3}$", " ", 
              "\\hline \\hline $\\boldsymbol{\\tau}\\mathbf{=0.75}$", "\\hline \\hline $\\hat{\\beta}_{0}$", " ", "$\\hat{\\beta}_{1}$", " ", 
              "$\\hat{\\beta}_{2}$", " ", "$\\hat{\\beta}_{3}$", " ", 
              "\\hline \\hline $\\boldsymbol{\\tau}\\mathbf{=0.95}$", "\\hline \\hline $\\hat{\\beta}_{0}$", " ", "$\\hat{\\beta}_{1}$", " ", 
              "$\\hat{\\beta}_{2}$", " ", "$\\hat{\\beta}_{3}$", " ", 
  "Brazil", BrazilCAViaR[50,1], BrazilCAViaR.BSSE.05[1],  BrazilCAViaR[50,2], BrazilCAViaR.BSSE.05[2], 
  BrazilCAViaR[50,3], BrazilCAViaR.BSSE.05[3],  BrazilCAViaR[50,4], BrazilCAViaR.BSSE.05[4], 
  "Brazil", BrazilCAViaR[250,1], BrazilCAViaR.BSSE.25[1],  BrazilCAViaR[250,2], BrazilCAViaR.BSSE.25[2], 
  BrazilCAViaR[250,3], BrazilCAViaR.BSSE.25[3],  BrazilCAViaR[250,4], BrazilCAViaR.BSSE.25[4], 
  "Brazil", BrazilCAViaR[750,1], BrazilCAViaR.BSSE.75[1],  BrazilCAViaR[750,2], BrazilCAViaR.BSSE.75[2], 
  BrazilCAViaR[750,3], BrazilCAViaR.BSSE.75[3],  BrazilCAViaR[750,4], BrazilCAViaR.BSSE.75[4], 
  "Brazil", BrazilCAViaR[950,1], BrazilCAViaR.BSSE.95[1],  BrazilCAViaR[950,2], BrazilCAViaR.BSSE.95[2], 
  BrazilCAViaR[950,3], BrazilCAViaR.BSSE.95[3],  BrazilCAViaR[950,4], BrazilCAViaR.BSSE.95[4], 
  
  "Chile", ChileCAViaR[50,1], ChileCAViaR.BSSE.05[1],  ChileCAViaR[50,2], ChileCAViaR.BSSE.05[2], 
  ChileCAViaR[50,3], ChileCAViaR.BSSE.05[3],  ChileCAViaR[50,4], ChileCAViaR.BSSE.05[4], 
  "Chile", ChileCAViaR[250,1], ChileCAViaR.BSSE.25[1],  ChileCAViaR[250,2], ChileCAViaR.BSSE.25[2], 
  ChileCAViaR[250,3], ChileCAViaR.BSSE.25[3],  ChileCAViaR[250,4], ChileCAViaR.BSSE.25[4], 
  "Chile", ChileCAViaR[750,1], ChileCAViaR.BSSE.75[1],  ChileCAViaR[750,2], ChileCAViaR.BSSE.75[2], 
  ChileCAViaR[750,3], ChileCAViaR.BSSE.75[3],  ChileCAViaR[750,4], ChileCAViaR.BSSE.75[4], 
  "Chile", ChileCAViaR[950,1], ChileCAViaR.BSSE.95[1],  ChileCAViaR[950,2], ChileCAViaR.BSSE.95[2], 
  ChileCAViaR[950,3], ChileCAViaR.BSSE.95[3],  ChileCAViaR[950,4], ChileCAViaR.BSSE.95[4], 
  
  "Argentina", ArgentinaCAViaR[50,1], ArgentinaCAViaR.BSSE.05[1],  ArgentinaCAViaR[50,2], ArgentinaCAViaR.BSSE.05[2], 
  ArgentinaCAViaR[50,3], ArgentinaCAViaR.BSSE.05[3],  ArgentinaCAViaR[50,4], ArgentinaCAViaR.BSSE.05[4], 
  "Argentina", ArgentinaCAViaR[250,1], ArgentinaCAViaR.BSSE.25[1],  ArgentinaCAViaR[250,2], ArgentinaCAViaR.BSSE.25[2], 
  ArgentinaCAViaR[250,3], ArgentinaCAViaR.BSSE.25[3],  ArgentinaCAViaR[250,4], ArgentinaCAViaR.BSSE.25[4], 
  "Argentina", ArgentinaCAViaR[750,1], ArgentinaCAViaR.BSSE.75[1],  ArgentinaCAViaR[750,2], ArgentinaCAViaR.BSSE.75[2], 
  ArgentinaCAViaR[750,3], ArgentinaCAViaR.BSSE.75[3],  ArgentinaCAViaR[750,4], ArgentinaCAViaR.BSSE.75[4], 
  "Argentina", ArgentinaCAViaR[950,1], ArgentinaCAViaR.BSSE.95[1],  ArgentinaCAViaR[950,2], ArgentinaCAViaR.BSSE.95[2], 
  ArgentinaCAViaR[950,3], ArgentinaCAViaR.BSSE.95[3],  ArgentinaCAViaR[950,4], ArgentinaCAViaR.BSSE.95[4], 

  "Mexico", MexicoCAViaR[50,1], MexicoCAViaR.BSSE.05[1],  MexicoCAViaR[50,2], MexicoCAViaR.BSSE.05[2], 
  MexicoCAViaR[50,3], MexicoCAViaR.BSSE.05[3],  MexicoCAViaR[50,4], MexicoCAViaR.BSSE.05[4], 
  "Mexico", MexicoCAViaR[250,1], MexicoCAViaR.BSSE.25[1],  MexicoCAViaR[250,2], MexicoCAViaR.BSSE.25[2], 
  MexicoCAViaR[250,3], MexicoCAViaR.BSSE.25[3],  MexicoCAViaR[250,4], MexicoCAViaR.BSSE.25[4], 
  "Mexico", MexicoCAViaR[750,1], MexicoCAViaR.BSSE.75[1],  MexicoCAViaR[750,2], MexicoCAViaR.BSSE.75[2], 
  MexicoCAViaR[750,3], MexicoCAViaR.BSSE.75[3],  MexicoCAViaR[750,4], MexicoCAViaR.BSSE.75[4], 
  "Mexico", MexicoCAViaR[950,1], MexicoCAViaR.BSSE.95[1],  MexicoCAViaR[950,2], MexicoCAViaR.BSSE.95[2], 
  MexicoCAViaR[950,3], MexicoCAViaR.BSSE.95[3],  MexicoCAViaR[950,4], MexicoCAViaR.BSSE.95[4] 
),36,5)






































