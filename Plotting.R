################################################################################################################################
################################################################################################################################
############################## STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - PLOTS ##############################
################################################################################################################################
################################################################################################################################
##########################################
########### DIAGONAL SECTIONS ############
##########################################
# Brazil-Chile
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/Diag/BCDiag.pdf")
plot(2:998/1000, BrazilChile.LT.SE[2,2:998], type = "l", col = "indianred1", lty = 2, xlab = "u", ylab = "C(u,u)", 
     main = "Brazil-Chile Diagonal Section")
points(2:998/1000, BrazilChile.LT.SE[3,2:998], type = "l", col = "indianred1", lty = 2)
points(seq(from = 0.1, to =0.9988889, length = 899), BrazilChile.Emp.Cop.CI[1,1001:1899], type = "l", col = "lightskyblue", lty = 2)
points(seq(from = 0.1, to =0.9988889, length = 899), BrazilChile.Emp.Cop.CI[2,1001:1899], type = "l", col = "lightskyblue", lty = 2)
points(2:99/100, BrazilChile.t.Diag.CI[1, 2:99], type = "l", col = "black", lty = 2)
points(2:99/100, BrazilChile.t.Diag.CI[2, 2:99], type = "l", col = "black", lty = 2)
points(BrazilChile.Emp.Cop[,c(1,2)], type = "l", lwd = 2, col = "blue")
points(BrazilChile.t.Diagonal, type = "l", lwd = 2)
points(1:999/1000, BrazilChile.LT[1,], type = "l", col = "red", lwd = 2)
legend("topleft", col = c("black", "blue", "red"), lwd = c(2,2,2), c("t-Copula w/ 95% CI", "Empirical w/ 95% CI", "CAViaR w/ 95%"))
dev.off()
####################
##########################################
##########################################
##########################################
########### TAIL SECTIONS ############
##########################################
# Brazil-Chile
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/Images/Tail/BCTail.pdf")
plot(6:200/1000, BrazilChile.Coex.SE[2,6:200], type = "l", col = "indianred1", lty = 2, xlab = "u", ylab = "C(u,u)/u", 
     main = "Brazil-Chile Lower Tail Dependence", xlim = c(0,0.2), ylim = c(0,0.8))
points(6:200/1000, BrazilChile.Coex.SE[3,6:200], type = "l", col = "indianred1", lty = 2)
points(c(seq(from = 0.002002002, to = 0.1, length.out = 981), seq(from = 0.1, to = 0.2, length.out = 100)),
       BrazilChile.Emp.Coex.CI[1, 20:1100], type = "l", col = "lightskyblue", lty = 2)
points(c(seq(from = 0.002002002, to = 0.1, length.out = 981), seq(from = 0.1, to = 0.2, length.out = 100)),
       BrazilChile.Emp.Coex.CI[2, 20:1100], type = "l", col = "lightskyblue", lty = 2)
points(seq(from = 0.001, to = 0.2, length.out = 20), BrazilChile.t.Coex.CI[1,1:20], type = "l", col = "gray40", lty = 2)
points(seq(from = 0.001, to = 0.2, length.out = 20), BrazilChile.t.Coex.CI[2,1:20], type = "l", col = "gray40", lty = 2)
points(BrazilChile.Emp.Coex[20:1100, c(1,2)], type = "l", lwd = 2, col = "blue")
points(BrazilChile.t.Coex[2:200,], type = "l", lwd = 2, col = "black")
points(6:200/1000, BrazilChile.Coex[1,6:200], type = "l", col = "red", lwd = 2)
legend("topleft", col = c("black", "blue", "red"), lwd = c(2,2,2), c("t-Copula  w/ 95% CI", "Empirical w/ 95% CI", "CAViaR w/ 95%"))
dev.off()
################################################################################################################################
################################################################################################################################
##########################################
########### RETURNS ############
##########################################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/Returns/SAReturnsPlots.pdf")
par(mfrow = c(2,2))
plot(as.Date(DataFrameDates[2227:7322,2], "%d/%m/%Y"), BrazilReturns[,2], type = "l", main = "Brazil Returns", xlab = c("Dates"), 
     ylab = c("Daily Returns"), col = "royalblue1")
plot(as.Date(AvailableDates[1419:7322,1], "%d/%m/%Y"), ChileReturns[,2], type = "l", main = "Chile Returns", xlab = c("Dates"), 
     ylab = c("Daily Returns"), col = "royalblue1")
plot(as.Date(AvailableDates[2227:7322,1], "%d/%m/%Y"), ArgentinaReturns[,2], type = "l", main = "Argentina Returns", xlab = c("Dates"), 
     ylab = c("Daily Returns"), col = "royalblue1")
plot(as.Date(AvailableDates[2686:7322,1], "%d/%m/%Y"), MexicoReturns[,2], type = "l", main = "Mexico Returns", xlab = c("Dates"), 
     ylab = c("Daily Returns"), col = "royalblue1")
dev.off()
par(mfrow = c(1,1))

################################################################################################################################
################################################################################################################################
##########################################
########### COEXCEEDANCE AT 5% ############
##########################################
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondCAViaR/BCCondCavCoexDays05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit-Based Model for Brazil-Chile \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Chile < VaR(0.05)")
points(BrazilChile.CondCAViaR.MSE.05[,c(1,3)], type = "l", col = "black", lwd = 2)
points(BrazilChile.CondCAViaR.MSE.05[,c(1,2)], type = "l", col = "coral1", lwd = 2)
legend("topleft", c("Logit-Based Probability", "Instances of Coexceedance"), col = c("coral1", "black"), lwd = c(2,2))
dev.off()

rbPal <- colorRampPalette(c('blue','red'))
BC5Col <- rbPal(10)[as.numeric(cut(BrazilChile.CondCAViaR.MSE.05[BrazilChile.CondCAViaR.MSE.05[,2]!=0,2], breaks = 10))]
####
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondCAViaR/BCCondCav05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit-Based Model for Brazil-Chile \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Chile < VaR(0.05)")
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
points(BrazilChile.CondCAViaR.MSE.05[BrazilChile.CondCAViaR.MSE.05[,2]!=0,c(1,2)],  col = BC5Col, pch = 19)
legend("topleft", c("Logit-Based - Low Probability", "Logit-Based - Medium Probability", "Logit-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
BA5Col <- rbPal(10)[as.numeric(cut(BrazilArgentina.CondCAViaR.MSE.05[BrazilArgentina.CondCAViaR.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondCAViaR/BACondCav05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit-Based Model for Brazil-Argentina \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Argentina < VaR(0.05)")
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
points(BrazilArgentina.CondCAViaR.MSE.05[BrazilArgentina.CondCAViaR.MSE.05[,2]!=0,c(1,2)],  col = BA5Col, pch = 19)
legend("topleft", c("Logit-Based - Low Probability", "Logit-Based - Medium Probability", "Logit-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
BM5Col <- rbPal(10)[as.numeric(cut(BrazilMexico.CondCAViaR.MSE.05[BrazilMexico.CondCAViaR.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondCAViaR/BMCondCav05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit-Based Model for Brazil-Mexico \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Mexico < VaR(0.05)")
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
points(BrazilMexico.CondCAViaR.MSE.05[BrazilMexico.CondCAViaR.MSE.05[,2]!=0,c(1,2)],  col = BM5Col, pch = 19)
legend("topleft", c("Logit-Based - Low Probability", "Logit-Based - Medium Probability", "Logit-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
#################.
###################
CA5Col <- rbPal(10)[as.numeric(cut(ChileArgentina.CondCAViaR.MSE.05[ChileArgentina.CondCAViaR.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondCAViaR/CACondCav05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit-Based Model for Chile-Argentina \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Argentina < VaR(0.05)")
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
points(ChileArgentina.CondCAViaR.MSE.05[ChileArgentina.CondCAViaR.MSE.05[,2]!=0,c(1,2)],  col = CA5Col, pch = 19)
legend("topleft", c("Logit-Based - Low Probability", "Logit-Based - Medium Probability", "Logit-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
CM5Col <- rbPal(10)[as.numeric(cut(ChileMexico.CondCAViaR.MSE.05[ChileMexico.CondCAViaR.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondCAViaR/CMCondCav05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit-Based Model for Chile-Mexico \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Mexico < VaR(0.05)")
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
points(ChileMexico.CondCAViaR.MSE.05[ChileMexico.CondCAViaR.MSE.05[,2]!=0,c(1,2)],  col = CM5Col, pch = 19)
legend("topleft", c("Logit-Based - Low Probability", "Logit-Based - Medium Probability", "Logit-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
AM5Col <- rbPal(10)[as.numeric(cut(ArgentinaMexico.CondCAViaR.MSE.05[ArgentinaMexico.CondCAViaR.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondCAViaR/AMCondCav05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Logit-Based Model for Argentina-Mexico \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Mexico < VaR(0.05)")
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
points(ArgentinaMexico.CondCAViaR.MSE.05[ArgentinaMexico.CondCAViaR.MSE.05[,2]!=0,c(1,2)],  col = AM5Col, pch = 19)
legend("topleft", c("Logit-Based - Low Probability", "Logit-Based - Medium Probability", "Logit-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()

################################################################################################################################
################################################################################################################################
####### CONDITIONAL EMPIRICAL
##########################################
########### COEXCEEDANCE AT 5% ############
##########################################
BC5Colemp <- rbPal(10)[as.numeric(cut(BrazilChile.CondEmp.MSE.05[BrazilChile.CondEmp.MSE.05[,2]!=0,2], breaks = 10))]
####
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondEmp/BCCondEmp05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Kernel-Based Model for Brazil-Chile \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Chile < VaR(0.05)")
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
points(BrazilChile.CondEmp.MSE.05[BrazilChile.CondEmp.MSE.05[,2]!=0,c(1,2)],  col = BC5Colemp, pch = 19)
legend("topleft", c("Kernel-Based - Low Probability", "Kernel-Based - Medium Probability", "Kernel-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
BA5Colemp <- rbPal(10)[as.numeric(cut(BrazilArgentina.CondEmp.MSE.05[BrazilArgentina.CondEmp.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondEmp/BACondEmp05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Kernel-Based Model for Brazil-Argentina \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Argentina < VaR(0.05)")
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
points(BrazilArgentina.CondEmp.MSE.05[BrazilArgentina.CondEmp.MSE.05[,2]!=0,c(1,2)],  col = BA5Colemp, pch = 19)
legend("topleft", c("Kernel-Based - Low Probability", "Kernel-Based - Medium Probability", "Kernel-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
BM5Colemp <- rbPal(10)[as.numeric(cut(BrazilMexico.CondEmp.MSE.05[BrazilMexico.CondEmp.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondEmp/BMCondEmp05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Kernel-Based Model for Brazil-Mexico \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Mexico < VaR(0.05)")
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
points(BrazilMexico.CondEmp.MSE.05[BrazilMexico.CondEmp.MSE.05[,2]!=0,c(1,2)],  col = BM5Colemp, pch = 19)
legend("topleft", c("Kernel-Based - Low Probability", "Kernel-Based - Medium Probability", "Kernel-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
#################.
###################
CA5Colemp <- rbPal(10)[as.numeric(cut(ChileArgentina.CondEmp.MSE.05[ChileArgentina.CondEmp.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondEmp/CACondEmp05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Kernel-Based Model for Chile-Argentina \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Argentina < VaR(0.05)")
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
points(ChileArgentina.CondEmp.MSE.05[ChileArgentina.CondEmp.MSE.05[,2]!=0,c(1,2)],  col = CA5Colemp, pch = 19)
legend("topleft", c("Kernel-Based - Low Probability", "Kernel-Based - Medium Probability", "Kernel-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
CM5Colemp <- rbPal(10)[as.numeric(cut(ChileMexico.CondEmp.MSE.05[ChileMexico.CondEmp.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondEmp/CMCondEmp05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Kernel-Based Model for Chile-Mexico \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Mexico < VaR(0.05)")
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
points(ChileMexico.CondEmp.MSE.05[ChileMexico.CondEmp.MSE.05[,2]!=0,c(1,2)],  col = CM5Colemp, pch = 19)
legend("topleft", c("Kernel-Based - Low Probability", "Kernel-Based - Medium Probability", "Kernel-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()
#################.
###################
AM5Colemp <- rbPal(10)[as.numeric(cut(ArgentinaMexico.CondEmp.MSE.05[ArgentinaMexico.CondEmp.MSE.05[,2]!=0,2], breaks = 10))]
pdf("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/ThesisWriteUp/Images/CondEmp/AMCondEmp05.pdf")
plot(as.Date(AvailableCAViaRDates[,2], "%d/%m/%Y"), rep(0, nrow(AvailableCAViaRDates)), type = "l", ylim = c(0,1.3), xlab = "Date", 
     ylab = "Probability of Coexceedance", main = "Kernel-Based Model for Argentina-Mexico \n Expected Proabability of Coexceedance at tau = 0.05 \n Estimated on Days with Mexico < VaR(0.05)")
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
points(ArgentinaMexico.CondEmp.MSE.05[ArgentinaMexico.CondEmp.MSE.05[,2]!=0,c(1,2)],  col = AM5Colemp, pch = 19)
legend("topleft", c("Kernel-Based - Low Probability", "Kernel-Based - Medium Probability", "Kernel-Based - High Probability", "Financial Crises"),
       col = c("blue", "purple", "red",  "gray"), 
       lwd = c(NA,NA,NA,4), pch = c(19,19,19,  NA), bg ="white")
dev.off()





























