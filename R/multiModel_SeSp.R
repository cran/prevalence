## Expressions for TP, SE[] and SP[]

multiModel_SeSp <-
function(n){
  TPSESP <- character(1 + 2 * n)
  TPSESP[1] <- "TP <- theta[1]"
  TPSESP[2] <- "SE1 <- theta[2]"
  TPSESP[3] <- "SP1 <- theta[3]"

  if (n > 1){
    for (i in 2:n){
      buildSE <- multiModel_build(i, SP = FALSE)
      buildSP <- multiModel_build(i, SP = TRUE)
      if (i > 2){
        for (j in i:3){
          buildSE <- multiModel_collapse(buildSE, j, SP = FALSE)
          buildSP <- multiModel_collapse(buildSP, j, SP = TRUE)
        }
      }
      TPSESP[(2*i)]   <- paste("SE", i, " <- ", buildSE, sep = "")
      TPSESP[(2*i)+1] <- paste("SP", i, " <- ", buildSP, sep = "")
    }
  }
  return(TPSESP)
}

multiModel_build <-
function(n, SP){
  N <- 2^(n-1)
  Next <- c(2*N, 2*N+1)
  out <- character(N/2)
  for (i in seq(N/2)){
    out[i] <-
      paste("theta[", N+(i-1)+(SP*N/2),
            "] * theta[", Next[1]+(SP*N),
            "] + (1-theta[", N+(i-1)+(SP*N/2), 
            "]) * theta[", Next[2]+(SP*N),
            "]",
            sep = "")
    Next <- Next + 2
  }
  return(out)
}


multiModel_collapse <-
function(build, n, SP){
  N <- length(build) / 2
  Next <- 2^(n-2)
  out <- character(N)
  for (i in seq(N)){
    ii <- (2*i)-1
    out[i] <-
      paste("theta[", Next+(i-1)+(SP*Next/2),
            "] * (", build[ii],
            ") + (1-theta[", Next+(i-1)+(SP*Next/2),
            "]) * (", build[ii+1],
            ")",
            sep = "")
  }
  return(out)
}