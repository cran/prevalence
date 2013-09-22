\name{plot-methods-coda}
\docType{methods}
\alias{plot-methods-coda}
\alias{densplot-methods}
\alias{densplot,prev-method}
\alias{traceplot-methods}
\alias{traceplot,prev-method}
\alias{gelman.plot-methods}
\alias{gelman.plot,prev-method}
\alias{autocorr.plot-methods}
\alias{autocorr.plot,prev-method}

\title{Plotting functions from package \pkg{coda}}

\description{
  Different plotting functions from package \pkg{coda} have been
  made available as method to class \code{prev}
}

\usage{
\S4method{densplot}{prev}(x, exclude_fixed = TRUE, \dots)

\S4method{traceplot}{prev}(x, exclude_fixed = TRUE, \dots)

\S4method{autocorr.plot}{prev}(x, exclude_fixed = TRUE, chain = 1, \dots)
}

\arguments{
  \item{x}{An object of class \code{prev}}
  \item{exclude_fixed}{Should fixed parameters be excluded from plotting?
                       defaults to \code{TRUE}}
  \item{chain}{Which chain to plot in \code{autocorr.plot};
               defaults to 1}
  \item{\dots}{Other arguments to pass to the specific plot function.}
}

\section{Methods}{
  \describe{
    \item{\code{signature(x = "prev")}}{
      Show \link[coda:densplot]{density}, \link[coda:traceplot]{trace},
	  \link[coda:gelman.plot]{Brooks-Gelman-Rubin} and
	  \link[coda:autocorr.plot]{autocorrelation} plots.
	}
  }
}

\seealso{
  \code{\link{prev-class}}\cr
  \code{\link{plot-methods}}\cr
  \code{\link[coda:densplot]{densplot}}, 
  \code{\link[coda:traceplot]{traceplot}}, 
  \code{\link[coda:gelman.plot]{gelman.plot}}, 
  \code{\link[coda:autocorr.plot]{autocorr.plot}}
}

\keyword{methods}
