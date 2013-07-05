\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,prev-method}
\alias{plot,prev,ANY-method}

\title{Methods for Function \code{plot} in Package \pkg{prevalence}}

\description{Plot objects of class \code{prev}}

\usage{\S4method{plot}{prev,ANY}(x, y = NULL, \dots)}

\arguments{
  \item{x}{An object of class \code{prev}}
  \item{y}{Unused argument; defaults to \code{NULL}}
  \item{\dots}{Other arguments to pass to the \code{plot} function}
}

\section{Methods}{
  \describe{
    \item{\code{signature(x = "prev", y = "ANY")}}{
      Show \link[coda:densplot]{density}, \link[coda:traceplot]{trace}, \link[coda:gelman.plot]{Brooks-Gelman-Rubin} and \link[coda:autocorr.plot]{autocorrelation} plots.
	}
  }
}

\seealso{
  \code{\link{prev-class}}\cr
  \code{\link[coda:densplot]{densplot}}, 
  \code{\link[coda:traceplot]{traceplot}}, 
  \code{\link[coda:gelman.plot]{gelman.plot}}, 
  \code{\link[coda:autocorr.plot]{autocorr.plot}}
}

\keyword{methods}
