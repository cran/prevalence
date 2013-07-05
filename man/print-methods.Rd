\name{print-methods}
\docType{methods}
\alias{print-methods}
\alias{print,prev-method}

\title{Methods for Function \code{print} in Package \pkg{prevalence}}

\description{Print objects of class \code{prev}}

\usage{\S4method{print}{prev}(x, conf.level, dig = 3, \dots)}

\arguments{
  \item{x}{An object of class \code{prev}}
  \item{conf.level}{Confidence level to be used in credibility interval}
  \item{dig}{Number of decimal digits to print}
  \item{\dots}{Other arguments to pass to the \code{print} function}
}

\section{Methods}{
  \describe{
    \item{\code{signature(x = "prev")}}{
      Print mean, median, mode and credibility interval of estimated true prevalence.
	  In addition, print \link[coda:gelman.diag]{Brooks-Gelman-Rubin statistic} and corresponding upper confidence limit. BGR values substantially above 1 indicate lack of convergence.
    }
  }
}

\seealso{
  \code{\link{prev-class}}\cr
  \code{\link[coda:gelman.diag]{gelman.diag}}
}

\keyword{methods}
