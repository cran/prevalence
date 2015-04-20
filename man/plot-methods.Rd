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
  \item{y}{Which parameter to plot? Defaults to \code{NULL}, in which case TP will be used}
  \item{\dots}{Other arguments to pass to the \code{plot} function}
}

\section{Methods}{
  \describe{
    \item{\code{signature(x = "prev", y = "ANY")}}{
      Show \code{\link[prevalence:densplot-methods]{density}},
	  \code{\link[prevalence:traceplot-methods]{trace}},
	  \code{\link[prevalence:gelman.plot-methods]{Brooks-Gelman-Rubin}} and
	  \code{\link[prevalence:autocorr.plot-methods]{autocorrelation}} plots.
	}
  }
}

\seealso{
  \code{\link{prev-class}}\cr
  \code{\link{densplot-methods}}, 
  \code{\link{traceplot-methods}}, 
  \code{\link{gelman.plot-methods}}, 
  \code{\link{autocorr.plot-methods}}
}

\keyword{methods}
