## main explode function
explode <-
function(x){
  theta <- vector("list", 2)
  theta[[1]] <- explode_theta(x[2])
  explode_operator(x[1])
  theta[[2]] <- explode_dist(x[3])
  return(theta)
}

## explode theta
explode_theta <-
function(x){
  ## find 'theta[]'
  if (length(grep("theta", x, fixed = TRUE)) != 1)
    stop("Priors must be defined as vector 'theta'")
  if (length(grep("[", x, fixed = TRUE)) != 1 |
      length(grep("]", x, fixed = TRUE)) != 1)
    stop("The different values of theta must be defined as 'theta[.]'")

  ## extract '.' in 'theta[.]'
  x <- strsplit(x, "theta[", fixed = TRUE)[[1]][2]
  theta <- strsplit(x, "]", fixed = TRUE)[[1]][1]

  ## theta should be an integer
  if (!is.numeric(theta) && as.numeric(theta) %% 1 != 0)
    stop("'theta[.]' not specified correctly")

  return(theta)
}

## explode operator
explode_operator <-
function(operator){
  ## operator should be '~' or '<-' or '='
  if (!any(c("~", "<-", "=") == operator))
    stop("Operator should be either '~', '<-' or '='")
}

## explode distribution
explode_dist <-
function(x){
  d <- dist2list(x)
  return(d)
}