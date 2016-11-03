IndepJointMLE <- function(param, Brazil, Chile, Argentina, Mexico){
  K <- length(param)
  #
  BrazilParam <- param[1:5]
  ChileParam <- param[6:10]
  ArgentinaParam <- param[11:15]
  MexicoParam <- param[16:20]
  #
  BrazilLL <- cappgarchLL(param = BrazilParam, data = Brazil)
  #
  ChileLL <- cappgarchLL(param = ChileParam, data = Chile)
  #
  ArgentinaLL <- cappgarchLL(param = ArgentinaParam, data = Argentina)
  #
  MexicoLL <- cappgarchLL(param = MexicoParam, data = Mexico)
  #
  LL <- BrazilLL + ChileLL + ArgentinaLL + MexicoLL
  #
  if (is.infinite(LL)){
    LL <- 0
  }
  return(LL)
}