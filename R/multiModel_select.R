## Define theta construct for SE/SP
## out01 defines construct of SE/SP: 0=(1-theta[.]), 1=theta[.]
## outSE and outSP define which thetas in expression for SE and SP

multiModel_select <-
function(n){
  out01 <- array(dim = c(2^n, n))
  outSE <- array(dim = c(2^n, n))
  outSP <- array(dim = c(2^n, n))
  for (i in seq(n)){
    out01[, i] <- rep(c(0, 1), each = 2^(n-i), times = 2^(i-1))
    outSE[, i] <- rep(c((2^i+2^(i-1)-1):(2^i)), each = 2^(n-i+1))
    outSP[, i] <- rep(c((2^i+2^(i-1)):(2^(i+1)-1)), each = 2^(n-i+1))
  }
  return(list(out01, outSE, outSP))
}