checkSeSp <-
function(x){
  distr <- NULL
  param <- NULL

  if (length(x) > 6)
    warning("Too many parameters specified")

  if (length(x) == 1){
    distr <- "fixed"
    if (x < 0 | x > 1)
      stop(paste("Parameter 'par' of fixed distribution must be",
                 "numeric value between 0 and 1"))
    param <- x
  } else {
    distr <- x$dist
  }

  if (length(unlist(x)) > length(x))
    stop("Parameters cannot be specified as vectors")

  if (is.null(distr))
    stop("No distribution specified")
  if (!is.character(distr))
    stop("Invalid distribution specified")
  distr <- tolower(distr)
  if (!any(c("fixed", "uniform", "pert", "beta", "beta-expert") == distr))
    stop(paste("Distribution must be",
               "'fixed', 'uniform', 'pert', 'beta' or 'beta-expert'",
               collapse = ""))

  if (distr == "fixed" & is.null(param)){
    if (length(x) > 2)
      warning("A fixed distribution requires only 1 parameter")
    if (is.null(x$par))
      stop("Parameter 'par' not specified")
    if (x$par < 0 | x$par > 1)
      stop(paste("Parameter 'par' of fixed distribution must be",
                 "numeric value between 0 and 1"))
    param <- x$par
  }

  if (distr == "uniform"){
    if (length(x) > 3)
      warning("A uniform distribution requires only 2 parameters")
    if (length(x) < 3)
      warning("A uniform distribution requires 2 parameters")
    if (is.null(x$min))
      stop("Parameter 'min' not specified")
    if (is.null(x$max))
      stop("Parameter 'max' not specified")
    if (any(c(x$min, x$max) < 0) | any(c(x$min, x$max) > 1))
      stop(paste("Parameters of uniform distribution must be",
                 "numeric values between 0 and 1"))
    if (x$min > x$max)
      stop("'min' of uniform distribution cannot be larger than 'max'")
    param <- c(x$min, x$max)
  }

  if (distr == "beta"){
    if (length(x) > 3)
      warning("A beta distribution requires only 2 parameters")
    if (length(x) < 3)
      warning("A beta distribution requires 2 parameters")
    if (is.null(x$alpha))
      stop("Parameter 'alpha' not specified")
    if (is.null(x$beta))
      stop("Parameter 'beta' not specified")
    if (any(c(x$alpha, x$beta) <= 0))
      stop(paste("Parameters of beta distribution must be",
                 "numeric values larger than 0"))
    param <- c(x$alpha, x$beta)
  }

  if (distr == "beta-expert"){
    if (is.null(x$mode) & is.null(x$mean))
      stop("At least 'mode' or 'mean' must be specified")
    if (!is.null(x$mode) & !is.null(x$mean))
      stop("'mode' and 'mean' cannot both be specified")
    method <- c("mode", "mean")[c(!is.null(x$mode), !is.null(x$mean))]
    best <- ifelse(method == "mode", x$mode, x$mean)
    if (is.null(x$lower) & is.null(x$upper))
      stop("At least 'lower' or 'upper' must be specified")
    if (is.null(x$p))
      stop("Parameter 'p' not specified")
    target <- c(x$lower, x$upper)[c(!is.null(x$lower), !is.null(x$upper))]
    if (any(c(best, x$p, x$target) < 0) | any(c(best, x$p, x$target) > 1))
      stop(paste("Parameters of beta-expert distribution must be",
                 "numeric values between 0 and 1"))
    if (!is.null(x$lower))
      if (x$lower > x$m)
        stop("'lower' cannot be larger than 'm'")
    if (!is.null(x$upper))
      if (x$upper < x$m)
        stop("'upper' cannot be smaller than 'm'")
    if (!is.null(x$lower) & !is.null(x$upper))
      if (x$lower > x$upper)
        stop("'lower' cannot be larger than 'upper'")
    distr <- "beta"
    if (is.null(x$upper)){
      param <-
        betaExpert(best = best, method = method, lower = x$lower, p = x$p)
    } else if (is.null(x$lower)){
      param <-
        betaExpert(best = best, method = method, upper = x$upper, p = x$p)
    } else {
      param <-
        betaExpert(best = best, method = method,
                   lower = x$lower, upper = x$upper, p = x$p)
    }
  }

  if (distr == "pert"){
    if (length(x) > 5)
      warning("A PERT distribution requires maximum 5 parameters")
    if (length(x) < 3)
      warning("A PERT distribution requires at least 3 parameters")
    if (is.null(x$a))
      stop("Parameter 'a' not specified")
    if (is.null(x$m))
      stop("Parameter 'm' not specified")
    if (is.null(x$b))
      stop("Parameter 'b' not specified")
    if (any(c(x$a, x$m, x$b) < 0) | any(c(x$a, x$m, x$b) > 1))
      stop(paste("Parameters of PERT distribution must be",
                 "numeric values between 0 and 1"))
    if (x$a > x$m)
      stop("'a' of PERT distribution cannot be larger than 'm'")
    if (x$m > x$b)
      stop("'m' of PERT distribution cannot be larger than 'b'")
    pertK <- ifelse(is.null(x$k), 4, x$k)
    pertP <- ifelse(is.null(x$p), .95, x$p)
    pertM <- ifelse(is.null(x$method), "classic", x$method)
    param <- c(x$a, x$m, x$b, pertK, pertP)
    distr <- c(distr, pertM)
  }

  return(list(d = distr, p = as.numeric(param)))
}