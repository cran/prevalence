\name{betaExpert}
\alias{betaExpert}
\alias{print.betaExpert}

\title{Calculate the parameters of a Beta distribution based on expert information}

\description{
The \link{betaExpert} function fits a (standard) Beta distribution to expert opinion.
The expert provides information on a best-guess estimate (mode or mean), and an uncertainty range:
\itemize{
  \item{The parameter value is with \code{100*p\%} certainty greater than \code{lower}}
  \item{The parameter value is with \code{100*p\%} certainty smaller than \code{upper}}
  \item{The parameter value lies with \code{100*p\%} in between \code{lower} and \code{upper}}
}
}

\usage{
betaExpert(best, lower, upper, p = 0.95, method = "mode")

\method{print}{betaExpert}(x, conf.level = .95, \dots)
}

\arguments{
  \item{best}{Best-guess estimate; see argument \code{method}}
  \item{lower}{Lower uncertainty limit}
  \item{upper}{Upper uncertainty limit}
  \item{p}{Expert's certainty level}
  \item{method}{Does best-guess estimate correspond to the \code{mode} or to the \code{mean}? Defaults to \code{mode}}
  
  \item{x}{Object of class \code{betaExpert}}
  \item{conf.level}{Confidence level used in printing quantiles of resulting Beta distribution}
  \item{\dots}{Other arguments to pass to function \code{print}}
}

\details{
  The methodology behind the \link{betaExpert} function is presented by Branscum et al. (2005)
  and implemented in the \emph{BetaBuster} software.\cr\cr
  The parameters of a standard Beta distribution are calculated 
  based on a best-guess estimate and a 100(\eqn{p})\% uncertainty range, defined by a lower and/or upper limit.
  The \code{betaExpert} function uses minimization (\code{\link{optimize}}) to derive \eqn{\alpha} and \eqn{\beta}
  from this best guess and lower and/or upper limit. The resulting distribution is a standard 2-parameter Beta distribution:
  Beta(\eqn{\alpha}, \eqn{\beta}).
}

\value{
A list of class \code{"betaExpert"}:
\item{alpha }{Parameter \eqn{\alpha} (shape1) of the Beta distribution}
\item{beta }{Parameter \eqn{\beta} (shape2) of the Beta distribution}

The \code{print} method for \code{"betaExpert"} additionally calculates the mean, median, mode, variance and range of the corresponding Beta distribution.
}

\references{
Branscum AJ, Gardner IA, Johnson WO (2005) Estimation of diagnostic-test sensitivity and specificity through Bayesian modeling. \emph{Prev Vet Med} \strong{68}:145-163.\cr
\href{http://www.epi.ucdavis.edu/diagnostictests/betabuster.html}{\emph{BetaBuster} homepage}
}

\author{
\email{Brecht.Devleesschauwer@UGent.be}
}

\seealso{
\code{\link{betaPERT}}, for modelling a generalized Beta distribution based on expert opinion
}

\examples{
## Most likely value (mode) is 90%
## Expert states with 95% certainty that true value is larger than 70%
betaExpert(best = 0.90, lower = 0.75, p = 0.95)

## Most likely value (mode) is 0%
## Expert states with 95% certainty that true value is smaller than 40%
betaExpert(best = 0, upper = 0.40, p = 0.95)

## Most likely value (mode) is 80%
## Expert states with 90% certainty that true value lies in between 40% and 90%
betaExpert(best = 0.80, lower = 0.40, upper = 0.90, p = 0.90)

## Mean value is assumed to be 80%
## Expert states with 90% certainty that true value lies in between 40% and 90%
betaExpert(best = 0.80, lower = 0.40, upper = 0.90, p = 0.90, method = "mean")
}

\keyword{Expert}
