\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,prev-method}

\title{Methods for Function \code{summary} in Package \pkg{prevalence}}

\description{Summarize objects of class \code{prev}}

\usage{\S4method{summary}{prev}(object, conf.level)}

\arguments{
  \item{object}{An object of class \code{prev}}
  \item{conf.level}{Confidence level to be used in credibility intervals}
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "prev")}}{
    Obtain mean, median, mode, standard deviation, variance, credibility interval and number of samples for each chain separately and for all chains combined.
    }
  }
}

\seealso{
  \code{\link{prev-class}}\cr
}

\keyword{methods}
