truePrevMulti <-
function(x, n, prior, conf.level = 0.95,
  nchains = 2, burnin = 5000, update = 10000,
  verbose = FALSE, plot = FALSE){

  ## check x and n
  if (missing(x))
    stop("'x' is missing")
  if (missing(n))
    stop("'n' is missing")
  checkInput(x, "x", class = "integer", min = 0)
  checkInput(n, "n", class = "integer", minEq = 0)
  if (sum(x) != n) stop("'x' does not sum to 'n'")

  ## check prior
  if (missing(prior))
    stop("'prior' is missing")
  prior <- checkMultiPrior(substitute(prior))

  ## check conf.level
  checkInput(conf.level, "conf.level", class = "numeric", range = c(0, 1))

  ## check nchains, burnin & update
  checkInput(nchains, "nchains", class = "integer", min = 2)
  checkInput(burnin, "burnin", class = "integer", min = 1)
  checkInput(update, "update", class = "integer", min = 1)

  ## check options
  checkInput(plot, "plot", class = "logical")
  checkInput(verbose, "verbose", class = "logical")

  ## get output
  out <- truePrevMultinom(x, n, prior, conf.level,
                          nchains, burnin, update, verbose)

  ## return output
  if (plot) plot(out)
  return(out)
}