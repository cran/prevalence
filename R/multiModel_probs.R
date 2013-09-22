## Define AP[.] in terms of theta[.]

multiModel_probs <-
function(s){
  p <- character(dim(s[[1]])[1])
  for (i in seq(dim(s[[1]])[1])){
    p[i] <- paste("AP[", i, "] <- theta[1]", sep = "")
    for (j in seq(dim(s[[1]])[2])){
      p[i] <- paste(p[i],
        ifelse(s[[1]][i,j] == 1, "*theta[", "*(1-theta["),
        s[[2]][i,j],
        ifelse(s[[1]][i,j] == 1, "]", "])"),
        sep = "")
    }
    p[i] <- paste(p[i], " + (1-theta[1])", sep = "")
    for (j in seq(dim(s[[1]])[2])){
      p[i] <- paste(p[i],
        ifelse(s[[1]][i,j] == 0, "*theta[", "*(1-theta["),
        s[[3]][i,j],
        ifelse(s[[1]][i,j] == 0, "]", "])"),
        sep = "")
    }
  }
  return(p)
}