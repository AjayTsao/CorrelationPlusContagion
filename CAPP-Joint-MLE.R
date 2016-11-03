CappJointMLE <- function(param, Brazil, Chile, Argentina, Mexico, Copula){
  K <- length(param)
  #
  BrazilParam <- param[1:5]
  ChileParam <- param[6:10]
  ArgentinaParam <- param[11:15]
  MexicoParam <- param[16:20]
  CopulaParam <- (param[21:K] / 100000)
  #
  BrazilLL <- cappgarchLL(param = BrazilParam, data = Brazil)
  BrazilU <- cappgarch.PIT(param = BrazilParam, data = Brazil)
  #
  ChileLL <- cappgarchLL(param = ChileParam, data = Chile)
  ChileU <- cappgarch.PIT(param = ChileParam, data = Chile)
  #
  ArgentinaLL <- cappgarchLL(param = ArgentinaParam, data = Argentina)
  ArgentinaU <- cappgarch.PIT(param = ArgentinaParam, data = Argentina)
  #
  MexicoLL <- cappgarchLL(param = MexicoParam, data = Mexico)
  MexicoU <- cappgarch.PIT(param = MexicoParam, data = Mexico)
  #
  BrazilChileU <- merge(BrazilU, ChileU, by = 1)
  ArgentinaMexicoU <- merge(ArgentinaU, MexicoU, by = 1)
  AllU <- merge(BrazilChileU, ArgentinaMexicoU, by = 1)
  #
  friendship <- do.call(cbind, list(AllU[,2], AllU[,3], AllU[,4], AllU[,5]))
  #
  CopulaLL <- loglikCopula(param = CopulaParam, u = friendship, copula = Copula)
  #
  LL <- BrazilLL + ChileLL + ArgentinaLL + MexicoLL + CopulaLL
  #

  return(LL)
}