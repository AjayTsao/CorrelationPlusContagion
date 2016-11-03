TablePrint <- function(Data, Rows, Columns, Rounding = 4){
  
  for (i in 1:length(Data)){
    if (grepl("\\.", Data[i]) & grepl("[^a-zA-Z]", Data[i]) &! grepl("\\$", Data[i]) &! grepl("[a-zA-Z]", Data[i])){
      Data[i] <- sprintf("%.4f", round(as.numeric(Data[i]), Rounding))
    }
  }
  mat.dat <- matrix(Data, Rows, Columns)
  #
  Converted <- matrix(0:0, Rows, Columns)
  #
  for (i in 1:Rows){
    for (j in 1:Columns){
      if(j < Columns){
        Converted[i,j] <- toString(cat(mat.dat[i,j], "& "))
      } else {
        Converted[i,j] <- toString(cat(mat.dat[i,j], "\\\\ ","\n"))
      }
    }
  }
  return(Converted)
}
