\name{truePrev}
\alias{truePrev}

\title{Estimate true prevalence from individuals samples}

\description{
Bayesian estimation of true prevalence from apparent prevalence obtained by testing \emph{individual} samples.
}

\usage{
truePrev(x, n, SE = 1, SP = 1, prior = c(1, 1), conf.level = 0.95,
  nchains = 2, burnin = 5000, update = 10000,
  verbose = FALSE, plot = FALSE)
}

\arguments{
  \item{x}{The apparent number of positive samples}
  \item{n}{The sample size}
  \item{SE, SP}{The prior distribution for sensitivity (SE) and specificity (SP); 
    see 'Details' below for specification of these distributions}
  \item{prior}{The parameters of the prior Beta distribution for true prevalence}
  \item{conf.level}{The confidence level used in the construction of the confidence interval}
  \item{nchains}{The number of chains used in the estimation process; \code{'n'} must be \eqn{\ge 2}}
  \item{burnin}{The number of discarded model iterations}
  \item{update}{The number of withheld model iterations}
  \item{verbose}{Logical flag, indicating if JAGS process output should be printed to the R console; 
    defaults to \code{FALSE}}
  \item{plot}{Logical flag, indicating if diagnostic plots should be given; defaults to \code{FALSE}}
}

\details{
  \code{truePrev} calls on \pkg{JAGS}/\pkg{\link[rjags:rjags]{rjags}} 
  to estimate the true prevalence from the apparent prevalence in a Bayesian framework.
  The default model, in BUGS language, is given below. To see the actual fitted model, see the model slot of the \link[prevalence:prev-class]{prev}-object.\cr
  \preformatted{
  model {
    x ~ dbin(AP, n)
    AP <- SE * TP + (1 - SP) * (1 - TP)
    # SE ~ user-defined (see below)
    # SP ~ user-defined (see below)
    TP ~ dbeta(prior[1], prior[2])
  }
  }  
  The test sensitivity (\code{SE}) and specificity (\code{SP}) can be specified, independently, 
  as one of \code{"fixed"}, \code{"uniform"}, \code{"beta"}, \code{"pert"}, or \code{"beta-expert"},
  with \code{"fixed"} as the default.

  Distribution parameters must be specified in a \emph{named} \code{list()} as follows:

\itemize{
  \item{\strong{Fixed:  }}{\code{list(dist = "fixed", par)}}
  \item{\strong{Uniform:  }}{\code{list(dist = "uniform", min, max)}}
  \item{\strong{Beta:  }}{\code{list(dist = "beta", alpha, beta)}}
  \item{\strong{Beta-PERT:  }}{\code{list(dist = "pert", method, a, m, b, k)}\cr
    \code{'method'} must be \code{"Classic"} or \code{"Vose"};\cr
    \code{'a'} denotes the pessimistic (minimum) estimate, \code{'m'} the most likely estimate, and \code{'b'} the optimistic (maximum) estimate;\cr
    \code{'k'} denotes the scale parameter.\cr
    See \link{betaPERT} for more information on Beta-PERT parametrization.}
  \item{\strong{Beta-Expert:  }}{\code{list(dist = "beta-expert", mode, mean, lower, upper, p)}\cr
    \code{'mode'} denotes the most likely estimate, \code{'mean'} the mean estimate;\cr
    \code{'lower'} denotes the lower bound, \code{'upper'} the upper bound;\cr
    \code{'p'} denotes the confidence level of the expert.\cr
	Only \code{mode} or \code{mean} should be specified; \code{lower} and \code{upper} can be specified together or alone.\cr
    See \link{betaExpert} for more information on Beta-Expert parametrization.}
}
}

\value{
An object of class \link[prevalence:prev-class]{prev}.
}

\note{
Markov chain Monte Carlo sampling in \code{truePrev} is performed by \pkg{JAGS (Just Another Gibbs Sampler)} through the \pkg{\link[rjags:rjags]{rjags}} package. JAGS can be downloaded from \url{http://sourceforge.net/projects/mcmc-jags/}.
}

\author{
\email{Brecht.Devleesschauwer@UGent.be}
}

\references{
\itemize{
  \item{
    Speybroeck N, Devleesschauwer B, Joseph L, Berkvens D (2012)
    \emph{Misclassification errors in prevalence estimation: Bayesian handling with care.}
	International Journal of Public Health
	}
  \item{
    Online Shiny application: \url{http://users.ugent.be/~bdvleess/R/prevalence/shiny/}
	}
  }
}

\seealso{
  \pkg{\link[coda:mcmc]{coda}} for various functions that can be applied to the \code{prev@mcmc.list} object\cr
  \code{\link{truePrevPools}}: estimate true prevalence from apparent prevalence obtained by testing \emph{pooled} samples\cr
  \code{\link{betaPERT}}: Calculate the parameters of a Beta-PERT distribution\cr
  \code{\link{betaExpert}}: Calculate the parameters of a Beta distribution based on expert opinion
}

\examples{
## Taenia solium cysticercosis in Nepal
## 142 positives out of 742 pigs sampled

## Model SE and SP based on literature data
## Sensitivity ranges uniformly between 60% and 100%
## Specificity ranges uniformly between 75% and 100%
SE <- list(dist = "uniform", min = 0.60, max = 1.00)
SP <- list(dist = "uniform", min = 0.75, max = 1.00)
truePrev(x = 142, n = 742, SE = SE, SP = SP)

## Model SE and SP based on expert opinions
## Sensitivity lies in between 60% and 100%; most likely value is 90%
## Specificity is with 95% confidence larger than 75%; most likely value is 90%
SE <- list(dist = "pert", a = 0.60, m = 0.90, b = 1.00)
SP <- list(dist = "beta-expert", mode = 0.90, lower = 0.75, p = 0.95)
truePrev(x = 142, n = 742, SE = SE, SP = SP)

## Model SE and SP as fixed values (each 90%)
truePrev(x = 142, n = 742, SE = 0.90, SP = 0.90)
}
