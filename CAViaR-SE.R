################################################################################################################################
################################################################################################################################
###################### STUART MORRISON - 42657927 - CODE FOR BECON HONOURS THESIS - CAViaR Standard Errors #####################
################################################################################################################################
################################################################################################################################
setwd("C:/Users/stuar.DESKTOP-C85RTEH/Dropbox/UNI/Honours/Thesis/MLE-CAViaR-Full-Repos/MLE and CAViaR/")
######
######
CAViaR.LT.SE <- function(indic1, indic2, prob){
  LTSE <- matrix(0:0, 3, ncol(prob))
  for (i in 1:ncol(LTSE)){
    tempindic <- merge(indic1[,c(1,(1+i))], indic2[,c(1,(1+i))], by = 1)
    Y <- tempindic[,2] * tempindic[,3]
    ss <- (Y - prob[1,(i-1)]) ^ 2
    LTSE[1,i] <- ((sum(ss) / nrow(tempindic)) / nrow(tempindic)) ^ 0.5 
    LTSE[2,i] <- prob[1,i] + (qt(0.025, df = nrow(tempindic))) * LTSE[1,i]
    LTSE[3,i] <- prob[1,i] + (qt(0.975, df = nrow(tempindic))) * LTSE[1,i]
  }
  return(LTSE)
}
######
######
CAViaR.COEX.SE <- function(indic1, indic2, prob){
  LTSE <- matrix(0:0, 3, ncol(prob))
  #
  for (i in 1:ncol(LTSE)){
    indictemp <- merge(indic1[,c(1,(1+i))], indic2[,c(1,(1+i))], by = 1)
    ss <- indictemp[,3] * (indictemp[,2] - prob[1,i]) ^ 2
    LTSE[1,i] <- ((sum(ss) / sum(indictemp[,3])) / sum(indictemp[,3])) ^ 0.5 
    LTSE[2,i] <- prob[1,i] + (qt(0.025, df = sum(indictemp[,3]))) * LTSE[1,i]
    LTSE[3,i] <- prob[1,i] + (qt(0.975, df = sum(indictemp[,3]))) * LTSE[1,i]
    }
  return(LTSE)
}
######
######
BrazilChile.LT.SE <- CAViaR.LT.SE(BrazilIndic.LT, ChileIndic.LT,  BrazilChile.LT)
#
BrazilArgentina.LT.SE <- CAViaR.LT.SE(BrazilIndic.LT, ArgentinaIndic.LT,  BrazilArgentina.LT)
#
BrazilMexico.LT.SE <- CAViaR.LT.SE(BrazilIndic.LT, MexicoIndic.LT,  BrazilMexico.LT)
#
ChileArgentina.LT.SE <- CAViaR.LT.SE(ChileIndic.LT, ArgentinaIndic.LT,  ChileArgentina.LT)
#
ChileMexico.LT.SE <- CAViaR.LT.SE(ChileIndic.LT, MexicoIndic.LT,  ChileMexico.LT)
#
ArgentinaMexico.LT.SE <- CAViaR.LT.SE(ArgentinaIndic.LT, MexicoIndic.LT,  ArgentinaMexico.LT)
######
############
######
BrazilChile.Coex.SE <- CAViaR.COEX.SE(BrazilIndic.LTGT, ChileIndic.LTGT, BrazilChile.Coex)
#
BrazilArgentina.Coex.SE <- CAViaR.COEX.SE(BrazilIndic.LTGT, ArgentinaIndic.LTGT, BrazilArgentina.Coex)
#
BrazilMexico.Coex.SE <- CAViaR.COEX.SE(BrazilIndic.LTGT, MexicoIndic.LTGT, BrazilMexico.Coex)
#
ChileArgentina.Coex.SE <- CAViaR.COEX.SE(ChileIndic.LTGT, ArgentinaIndic.LTGT, ChileArgentina.Coex)
#
ChileMexico.Coex.SE <- CAViaR.COEX.SE(ChileIndic.LTGT, MexicoIndic.LTGT, ChileMexico.Coex)
#
ArgentinaMexico.Coex.SE <- CAViaR.COEX.SE(ArgentinaIndic.LTGT, MexicoIndic.LTGT, ArgentinaMexico.Coex)
#
























