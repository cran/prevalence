\name{print-methods}
\docType{methods}
\alias{print-methods}
\alias{print,prev-method}

\title{Methods for Function \code{print} in Package \pkg{prevalence}}

\description{Print objects of class \code{prev}}

\usage{\S4method{print}{prev}(x, conf.level = 0.95, dig = 3, \dots)}

\arguments{
  \item{x}{An object of class \code{prev}}
  \item{conf.level}{Confidence level to be used in credibility interval}
  \item{dig}{Number of decimal digits to print}
  \item{\dots}{Other arguments to pass to the \code{print} function}
}

\section{Methods}{
  \describe{
    \item{\code{signature(x = "prev")}}{Print mean, median, mode, standard deviation and credibility interval of estimated true prevalence, sensitivities and specificities. In addition, print multivariate \link[coda:gelman.diag]{Brooks-Gelman-Rubin statistic} (or univariate BGR statistic with corresponding upper confidence limit in case of a single stochastic node). BGR values substantially above 1 indicate lack of convergence. For \code{prev} objects created by \code{\link{truePrevMulti}}, the Bayes-P statistic is also printed. Bayes-P should be as close to 0.5 as possible.}
  }
}

\seealso{
  \code{\link{prev-class}}\cr
  \code{\link[coda:gelman.diag]{gelman.diag}}
}

\keyword{methods}
