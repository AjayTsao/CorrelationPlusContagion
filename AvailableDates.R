AvailableDates <- read.csv("ReturnsData/Dates.csv", header = F)
MatrixDates <- matrix(as.Date(AvailableDates[,1], "%d/%m/%Y"), nrow(AvailableDates), 1)
DataFrameDates <- data.frame(MatrixDates, AvailableDates)
##
##
AllCappUMatDates <- data.frame(All.cappU[,1])
AllCAViaRUMatDates <- data.frame(All.CAViaRU[,1])
##
AvailableCappDates <- merge(DataFrameDates, AllCappUMatDates, by = 1)
AvailableCAViaRDates <- merge(DataFrameDates, AllCAViaRUMatDates, by = 1)

TequilaCrisis.dates <-matrix(as.Date("01/11/1994", "%d/%m/%Y"):as.Date("31/03/1995", "%d/%m/%Y"))
AsianCrisis.dates <-matrix(as.Date("02/06/1997", "%d/%m/%Y"):as.Date("31/12/1997", "%d/%m/%Y"))
RussianCrisis.dates <- matrix(as.Date("03/08/1998", "%d/%m/%Y"):as.Date("31/12/1998", "%d/%m/%Y"))
ArgentineanCrisis.dates <- matrix(as.Date("26/03/2001", "%d/%m/%Y"):as.Date("15/05/2001", "%d/%m/%Y"))
SubprimeCrisis.dates <- matrix(as.Date("15/02/2007", "%d/%m/%Y"):as.Date("30/03/2007", "%d/%m/%Y"))
LehmanCrisis.dates <- matrix(as.Date("01/09/2008", "%d/%m/%Y"):as.Date("31/10/2008", "%d/%m/%Y"))
EuroCrisis.dates <- matrix(as.Date("01/08/2011", "%d/%m/%Y"):as.Date("30/09/2011", "%d/%m/%Y"))

pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/TrialPlots/BCCondCav1.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.2), xlab = "Date",
     ylab = "Probability of Coexceedance", main = "Logit Regression Model for Brazil-Chile \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with F(Chile) < 0.05")
grid(nx = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"))
polygon(c(TequilaCrisis.dates[1,1], TequilaCrisis.dates[1,1], TequilaCrisis.dates[nrow(TequilaCrisis.dates),1], TequilaCrisis.dates[nrow(TequilaCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(AsianCrisis.dates[1,1], AsianCrisis.dates[1,1], AsianCrisis.dates[nrow(AsianCrisis.dates),1], AsianCrisis.dates[nrow(AsianCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(RussianCrisis.dates[1,1], RussianCrisis.dates[1,1], RussianCrisis.dates[nrow(RussianCrisis.dates),1], RussianCrisis.dates[nrow(RussianCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(ArgentineanCrisis.dates[1,1], ArgentineanCrisis.dates[1,1], ArgentineanCrisis.dates[nrow(ArgentineanCrisis.dates),1], ArgentineanCrisis.dates[nrow(ArgentineanCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(SubprimeCrisis.dates[1,1], SubprimeCrisis.dates[1,1], SubprimeCrisis.dates[nrow(SubprimeCrisis.dates),1], SubprimeCrisis.dates[nrow(SubprimeCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(LehmanCrisis.dates[1,1], LehmanCrisis.dates[1,1], LehmanCrisis.dates[nrow(LehmanCrisis.dates),1], LehmanCrisis.dates[nrow(LehmanCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(EuroCrisis.dates[1,1], EuroCrisis.dates[1,1], EuroCrisis.dates[nrow(EuroCrisis.dates),1], EuroCrisis.dates[nrow(EuroCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
points(BrazilChile.CondCAViaR.MSE.05[,c(1,2)], type = "l", col = "deepskyblue")
points(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(BrazilChile.Coex[1,50], nrow(AvailableCAViaRDates)), type = "l", col = "red")
legend("topleft", c("Logit Regression", "Cappiello et. al.", "Financial Crises"), col = c("deepskyblue", "red", "gray"), lwd = c(1,1,4), lty = c(1,1,1))
dev.off()
####




####
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/TrialPlots/BCCondCav2.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.2), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit Regression Model for Brazil-Chile \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on All Available Dates")
grid(nx = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"))
polygon(c(TequilaCrisis.dates[1,1], TequilaCrisis.dates[1,1], TequilaCrisis.dates[nrow(TequilaCrisis.dates),1], TequilaCrisis.dates[nrow(TequilaCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(AsianCrisis.dates[1,1], AsianCrisis.dates[1,1], AsianCrisis.dates[nrow(AsianCrisis.dates),1], AsianCrisis.dates[nrow(AsianCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(RussianCrisis.dates[1,1], RussianCrisis.dates[1,1], RussianCrisis.dates[nrow(RussianCrisis.dates),1], RussianCrisis.dates[nrow(RussianCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(ArgentineanCrisis.dates[1,1], ArgentineanCrisis.dates[1,1], ArgentineanCrisis.dates[nrow(ArgentineanCrisis.dates),1], ArgentineanCrisis.dates[nrow(ArgentineanCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(SubprimeCrisis.dates[1,1], SubprimeCrisis.dates[1,1], SubprimeCrisis.dates[nrow(SubprimeCrisis.dates),1], SubprimeCrisis.dates[nrow(SubprimeCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(LehmanCrisis.dates[1,1], LehmanCrisis.dates[1,1], LehmanCrisis.dates[nrow(LehmanCrisis.dates),1], LehmanCrisis.dates[nrow(LehmanCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(EuroCrisis.dates[1,1], EuroCrisis.dates[1,1], EuroCrisis.dates[nrow(EuroCrisis.dates),1], EuroCrisis.dates[nrow(EuroCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
points(BrazilChile.CondCAViaR.MSE.05AllDates[,c(1,2)], type = "l", col = "deepskyblue", lwd = 2)
points(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(BrazilChile.Coex[1,50], nrow(AvailableCAViaRDates)), type = "l", col = "red")
legend("topleft", c("Logit Regression", "Cappiello et. al.", "Financial Crises"), col = c("deepskyblue", "red", "gray"), lwd = c(2,1,4), lty = c(1,1,1))
dev.off()
####
rbPal <- colorRampPalette(c('blue','red'))
condcaviarcolour <- rbPal(10)[as.numeric(cut(BrazilChile.CondCAViaR.MSE.05AllDates[,2], breaks = 10))]
####
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/TrialPlots/BCCondCav3.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.2), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit Regression Model for Brazil-Chile \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on All Available Dates")
grid(nx = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"))
polygon(c(TequilaCrisis.dates[1,1], TequilaCrisis.dates[1,1], TequilaCrisis.dates[nrow(TequilaCrisis.dates),1], TequilaCrisis.dates[nrow(TequilaCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(AsianCrisis.dates[1,1], AsianCrisis.dates[1,1], AsianCrisis.dates[nrow(AsianCrisis.dates),1], AsianCrisis.dates[nrow(AsianCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(RussianCrisis.dates[1,1], RussianCrisis.dates[1,1], RussianCrisis.dates[nrow(RussianCrisis.dates),1], RussianCrisis.dates[nrow(RussianCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(ArgentineanCrisis.dates[1,1], ArgentineanCrisis.dates[1,1], ArgentineanCrisis.dates[nrow(ArgentineanCrisis.dates),1], ArgentineanCrisis.dates[nrow(ArgentineanCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(SubprimeCrisis.dates[1,1], SubprimeCrisis.dates[1,1], SubprimeCrisis.dates[nrow(SubprimeCrisis.dates),1], SubprimeCrisis.dates[nrow(SubprimeCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(LehmanCrisis.dates[1,1], LehmanCrisis.dates[1,1], LehmanCrisis.dates[nrow(LehmanCrisis.dates),1], LehmanCrisis.dates[nrow(LehmanCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
polygon(c(EuroCrisis.dates[1,1], EuroCrisis.dates[1,1], EuroCrisis.dates[nrow(EuroCrisis.dates),1], EuroCrisis.dates[nrow(EuroCrisis.dates),1]),
        c(0,1,1,0), col = "gray", border = NA)
points(BrazilChile.CondCAViaR.MSE.05AllDates[,c(1,2)],  col = condcaviarcolour, lwd = 2)
points(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(BrazilChile.Coex[1,50], nrow(AvailableCAViaRDates)), type = "l", col = "green", lwd = 2)
legend("topleft", c("Logit Regression", "Cappiello et. al.", "Financial Crises"), col = c("blue", "green", "gray"), lwd = c(2,1,4), lty = c(1,1,1))
dev.off()
############

###########################################################################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/TrialPlots/BCCondCavCoexDays.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit Regression Model for Brazil-Chile \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Chile < VaR(0.05)")
grid(nx = NULL, col = "gray34", lty = "dotted",
     lwd = par("lwd"))
points(BrazilChile.CondCAViaR.MSE.05[,c(1,3)],  col = "gray25", type = "l")
points(BrazilChile.CondCAViaR.MSE.05[,c(1,2)],  col = "red", type = "l")
points(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(BrazilChile.Coex[1,50], nrow(AvailableCAViaRDates)), type = "l", col = "green", lwd = 2)
legend("topleft", c("Logit Regression",  "Unconditional CAViaR", "Instances of Coexceedances"), col = c("red",  "green", "gray28"), 
       lwd = c(1,1,1), bg ="white")
dev.off()


############################################################################
TimeSeriesAIC <- matrix(c(-22814,-23713,-24155,-24882,-25034,-29799,-29788,
                  -30321,-23688,-30996,-16829,-18942,-19060,-19660,-20033,
                  -14114, -14396, -14491, -14249, -16050), 5,4)
colnames(TimeSeriesAIC) <- c("Brazil", "Chile", "Argentina", "Mexico")
rownames(TimeSeriesAIC) <- c("ARMA(1,0)", "ARMA(1,1)", "GARCH(1,0)", "GARCH(1,1)", "SAV-GARCH" )

TimeSeriesBIC <- matrix(c(-22801,-23694,-24122,-24842,-24995,
                          -29799,-29788,-30321,-23688,-30996,
                          -16817,-18923,-19029,-19622,-19995,
                          -14102,-14378,-14461,-14213,-16014), 5,4)
colnames(TimeSeriesBIC) <- c("Brazil", "Chile", "Argentina", "Mexico")
rownames(TimeSeriesBIC) <- c("ARMA(1,0)", "ARMA(1,1)", "GARCH(1,0)", "GARCH(1,1)", "SAV-GARCH" )

pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/AICBIC/SATimeSeriesAIC.pdf")
colB <- c("skyblue", "skyblue1", "skyblue2", "skyblue3", "skyblue4")
barplot(TimeSeriesAIC, beside =  T , col = c(colB, colB, colB, colB), main = "AIC for Different Time-Series Specifications" , ylab = "AIC")
legend("bottomright",  col = colB, c("ARMA(1,0)", "ARMA(1,1)", "GARCH(1,0)", "GARCH(1,1)", "SAV-GARCH" ), pch = rep(15,5))
dev.off()

pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/AICBIC/SATimeSeriesBIC.pdf")
colA <- c("coral", "coral1", "coral2", "coral3", "coral4")
barplot(TimeSeriesBIC, beside =  T , col = c(colA, colA, colA, colA), main = "BIC for Different Time-Series Specifications", ylab = "BIC")
legend("bottomright", col = colA, c("ARMA(1,0)", "ARMA(1,1)", "GARCH(1,0)", "GARCH(1,1)", "SAV-GARCH" ), pch = rep(15,5))
dev.off()

CopulaAICBIC <- matrix(c(indep.IC$AIC, clayton.IC$AIC, gumbel.IC$AIC, Gaussian.IC$AIC, t.IC$AIC,
                         indep.IC$BIC, clayton.IC$BIC, gumbel.IC$BIC, Gaussian.IC$BIC, t.IC$BIC), 5,2)
colnames(CopulaAICBIC) <- c("AIC", "BIC")
rownames(CopulaAICBIC) <- c("Indepdent Copula", "Clayton Copula", "Gumbel Copula", "Gaussian Copula", "t-Copula" )

pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/AICBIC/CopulaIC.pdf")
barplot(CopulaAICBIC, beside = T, col = c(colB, colA), main = "AIC and BIC for Different Copula Specifications", 
        ylim = c((min(CopulaAICBIC)-45000),0), names.arg = c("AIC", "BIC"), ylab = "IC Values")
legend("bottomleft", col = c(NA, colB), c("AIC", "Indepdent Copula", "Clayton Copula", "Gumbel Copula", "Gaussian Copula", "t-Copula"), 
       pch = c(NA, rep(15,5)))
legend("bottomright", col = c(NA, colA), c("BIC", "Indepdent Copula", "Clayton Copula", "Gumbel Copula", "Gaussian Copula", "t-Copula"), 
       pch = c(NA, rep(15,5)))
dev.off()


##########################################
cfPal <- colorRampPalette(c("deepskyblue","deeppink3"))
condempcolour <- cfPal(10)[as.numeric(cut(BrazilChile.CondEmp.MSE.05[BrazilChile.CondEmp.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/TrialPlots/BCCondEmp05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Conditional Empirical Copula for Brazil-Chile \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Chile < VaR(0.05)")
grid(nx = NULL, col = "gray34", lty = "dotted",
     lwd = par("lwd"))
polygon(c(TequilaCrisis.dates[1,1], TequilaCrisis.dates[1,1], TequilaCrisis.dates[nrow(TequilaCrisis.dates),1], TequilaCrisis.dates[nrow(TequilaCrisis.dates),1]),
        c(0,1,1,0), col = "gray82", border = NA)
polygon(c(AsianCrisis.dates[1,1], AsianCrisis.dates[1,1], AsianCrisis.dates[nrow(AsianCrisis.dates),1], AsianCrisis.dates[nrow(AsianCrisis.dates),1]),
        c(0,1,1,0), col = "gray82", border = NA)
polygon(c(RussianCrisis.dates[1,1], RussianCrisis.dates[1,1], RussianCrisis.dates[nrow(RussianCrisis.dates),1], RussianCrisis.dates[nrow(RussianCrisis.dates),1]),
        c(0,1,1,0), col = "gray82", border = NA)
polygon(c(ArgentineanCrisis.dates[1,1], ArgentineanCrisis.dates[1,1], ArgentineanCrisis.dates[nrow(ArgentineanCrisis.dates),1], ArgentineanCrisis.dates[nrow(ArgentineanCrisis.dates),1]),
        c(0,1,1,0), col = "gray82", border = NA)
polygon(c(SubprimeCrisis.dates[1,1], SubprimeCrisis.dates[1,1], SubprimeCrisis.dates[nrow(SubprimeCrisis.dates),1], SubprimeCrisis.dates[nrow(SubprimeCrisis.dates),1]),
        c(0,1,1,0), col = "gray82", border = NA)
polygon(c(LehmanCrisis.dates[1,1], LehmanCrisis.dates[1,1], LehmanCrisis.dates[nrow(LehmanCrisis.dates),1], LehmanCrisis.dates[nrow(LehmanCrisis.dates),1]),
        c(0,1,1,0), col = "gray82", border = NA)
polygon(c(EuroCrisis.dates[1,1], EuroCrisis.dates[1,1], EuroCrisis.dates[nrow(EuroCrisis.dates),1], EuroCrisis.dates[nrow(EuroCrisis.dates),1]),
        c(0,1,1,0), col = "gray82", border = NA)
points(BrazilChile.CondEmp.MSE.05[BrazilChile.CondEmp.MSE.05[,2]!=0,c(1,2)],  col = condempcolour, pch = 19)
points(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(BrazilChile.Coex[1,50], nrow(AvailableCAViaRDates)), type = "l", col = "green", lwd = 2)
legend("topleft", c("Conditional Empirical Copula - Low Probability", "Conditional Empirical Copula - Medium Probability", "Conditional Empirical Copula - High Probability", "Unconditional CAViaR", "Financial Crises"), col = c("deepskyblue", "purple", "deeppink3", "green", "gray"), 
       lwd = c(NA,NA,NA,1,4), pch = c(19,19,19, NA, NA), bg ="white")
dev.off()





