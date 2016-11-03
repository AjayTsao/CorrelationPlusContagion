CAViaR.JointProbFunction <- function(IndicatorA, IndicatorB, Tau){
  select.joint <- matrix(0:0, 1, (ncol(IndicatorA)-1))
  colnames(select.joint) <- Tau
  #
  joint.indicator <- merge(IndicatorA, IndicatorB, by = 1)
  
  for (i in 1:ncol(select.joint)){
    select.joint[1,i] <- mean(joint.indicator[ ,(i+1)] * joint.indicator[ ,(ncol(IndicatorA)+i)])
  }
  return(select.joint)
}