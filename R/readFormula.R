readFormula <-
function(f){
  s <- as.character(f[[2]])
  d <- s[1]
  p <- s[-1]
  if (!any(c("dfixed", "dunif", "dbeta", "dpert") == d))
    stop("Distribution must be 'dfixed', 'dunif', 'dpert' or 'dbeta'")
  if (length(p) == 0)
    stop("No distribution parameters specified")

  dist <- c("fixed", "uniform", "pert", "beta")[
              which(c("dfixed", "dunif", "dpert", "dbeta") == d)]

  if (dist == "fixed"){
    if (length(p) > 1)
      warning("A fixed distribution requires only 1 parameter")
    if (p < 0 | p > 1)
      stop(paste("Parameter of fixed distribution must be",
                 "numeric value between 0 and 1"))
    out <- list(dist = dist, p = p)
  }

  if (dist == "uniform"){
    if (length(p) > 2)
      warning("A uniform distribution requires only 2 parameters")
    if (length(p) < 2)
      warning("A uniform distribution requires 2 parameters")
    if (any(p < 0) | any(p > 1))
      stop(paste("Parameters of uniform distribution must be",
                 "numeric values between 0 and 1"))
    if (p[1] > p[2])
      stop("'min' of uniform distribution cannot be larger than 'max'")
    out <- list(dist = dist, min = p[1], max = p[2])
  }

  if (dist == "beta"){
    if (length(p) > 2)
      warning("A beta distribution requires only 2 parameters")
    if (length(p) < 2)
      warning("A beta distribution requires 2 parameters")
    if (any(p <= 0))
      stop(paste("Parameters of beta distribution must be",
                 "numeric values larger than 0"))
    out <- list(dist = dist, alpha = p[1], beta = p[2])
  }

if (FALSE){
  if (dist == "pert"){
    if (length(x) > 5)
      warning("A PERT distribution requires maximum 5 parameters")
    if (length(x) < 3)
      warning("A PERT distribution requires at least 3 parameters")
    if (any(x < 0) | any(x > 1))
      stop(paste("Parameters of PERT distribution must be",
                 "numeric values between 0 and 1"))
    if (x[1] > x[2])
      stop("'a' of PERT distribution cannot be larger than 'm'")
    if (x[2] > x[3])
      stop("'m' of PERT distribution cannot be larger than 'b'")
    pertK <- ifelse(is.null(x$k), 4, x$k)
    pertP <- ifelse(is.null(x$p), .95, x$p)
    pertM <- ifelse(is.null(x$method), "classic", x$method)
    param <- c(x$a, x$m, x$b, pertK, pertP)
    distr <- c(distr, pertM)
  }
}
  return(out)
}

# f <- ~dunif(0, 1)
# f <- ~dfixed(.5)
# f <- ~dbeta(1, 99)
# readFormula(f)
# checkSeSp(readFormula(f))