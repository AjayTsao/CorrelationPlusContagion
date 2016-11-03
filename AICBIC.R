AICBIC <- function(LL, n, k){
  AIC <- (2 * k) - (2 * LL)
  BIC <- (-2 * LL) + (k * log(n))
  return(list(AIC = AIC, BIC = BIC))
}