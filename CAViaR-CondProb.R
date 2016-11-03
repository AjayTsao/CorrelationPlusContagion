CAViaR.CondProb <- function(JointProb, ConditionerIndicator, Tau){
  ConditionalProb <- matrix(0:0, 1, ncol(JointProb))
  colnames(ConditionalProb) <- Tau
  #
  for (i in 1:ncol(ConditionalProb)){
    ConditionalProb[1,i] <- JointProb[1,i] / mean(ConditionerIndicator[ ,(i+1)])
  }
  return(ConditionalProb)
}