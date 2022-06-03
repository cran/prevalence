\name{truePrevMulti2}
\alias{truePrevMulti2}

\title{Estimate true prevalence from individuals samples using multiple tests -- covariance scheme}

\description{
Bayesian estimation of true prevalence from apparent prevalence obtained by applying \emph{multiple} tests to \emph{individual} samples. \code{\link{truePrevMulti2}} implements and extends the approach described by Dendukuri and Joseph (2001), which uses a multinomial distribution to model observed test results,
and in which conditional dependence between tests is modelled through
covariances.
}

\usage{
truePrevMulti2(x, n, prior, nchains = 2, burnin = 10000, update = 10000,
               verbose = FALSE)
}

\arguments{
  \item{x}{Vector of apparent test results; see 'Details' below}
  \item{n}{The total sample size}
  \item{prior}{The prior distributions; see 'Details' below}
  \item{nchains}{The number of chains used in the estimation process; must be \eqn{\ge 2}}
  \item{burnin}{The number of discarded model iterations; defaults to 10,000}
  \item{update}{The number of withheld model iterations; defaults to 10,000}
  \item{verbose}{Logical flag, indicating if JAGS process output should be printed to the R console; defaults to \code{FALSE}}
}

\details{
  \code{\link{truePrevMulti2}} calls on \pkg{JAGS} via the \pkg{\link[rjags:rjags]{rjags}} package to estimate true prevalence from apparent prevalence in a Bayesian framework. \code{\link{truePrevMulti2}} fits a multinomial model to the apparent test results obtained by testing individual samples with a given number of tests. To see the actual fitted model, see the model slot of the \code{\link[prevalence:prev-class]{prev}}-object.\cr

  The vector of apparent tests results, \code{x}, must contain the number of samples corresponding to each combination of test results. To see how this vector is defined for the number of tests \code{h} at hand, use \code{\link{define_x}}.
  
  Argument \code{prior} consists of prior distributions for:
  \itemize{
    \item{True Prevalence: \code{TP}}
    \item{SEnsitivity of each individual test: vector \code{SE}}
    \item{SPecificity of each individual test: vector \code{SP}}
    \item{Conditional covariance of all possible test combinations given a truly positive disease status: vector \code{a}}
    \item{Conditional covariance of all possible test combinations given a truly negative disease status: vector \code{b}}
  }
  To see how \code{prior} is defined for the number of tests \code{h} at hand, use \code{\link{define_prior2}}.\cr
  
  The values of \code{prior} can be specified in two ways, referred to as BUGS-style and list-style, respectively. See also below for some examples.\cr
  
  For BUGS-style specification, the values of \code{prior} should be given between curly brackets (i.e., \code{{}}), separated by line breaks. Priors can be specified to be deterministic (i.e., fixed), using the \code{<-} operator, or stochastic, using the \code{~} operator. In the latter case, the following distributions can be used:

\itemize{
  \item{\strong{Uniform:  }}{\code{dunif(min, max)}}
  \item{\strong{Beta:  }}{\code{dbeta(alpha, beta)}}
  \item{\strong{Beta-PERT:  }}{\code{dpert(min, mode, max)}}
}
  
  Alternatively, priors can  be specified in a \emph{named} \code{list()} as follows:

\itemize{
  \item{\strong{Fixed:  }}{\code{list(dist = "fixed", par)}}
  \item{\strong{Uniform:  }}{\code{list(dist = "uniform", min, max)}}
  \item{\strong{Beta:  }}{\code{list(dist = "beta", alpha, beta)}}
  \item{\strong{Beta-PERT:  }}{\code{list(dist = "pert", method, a, m, b, k)}\cr
    \code{'method'} must be \code{"classic"} or \code{"vose"};\cr
    \code{'a'} denotes the pessimistic (minimum) estimate, \code{'m'} the most
  likely estimate, and \code{'b'} the optimistic (maximum) estimate;\cr
    \code{'k'} denotes the scale parameter.\cr
    See \code{\link{betaPERT}} for more information on Beta-PERT parameterization.}
  \item{\strong{Beta-Expert:  }}{\code{list(dist = "beta-expert", mode, mean,
  lower, upper, p)}\cr
    \code{'mode'} denotes the most likely estimate, \code{'mean'} the mean
	estimate;\cr
    \code{'lower'} denotes the lower bound, \code{'upper'} the upper bound;\cr
    \code{'p'} denotes the confidence level of the expert.\cr
	Only \code{mode} or \code{mean} should be specified; \code{lower} and
	\code{upper} can be specified together or alone.\cr
    See \code{\link{betaExpert}} for more information on Beta-Expert parameterization.}
}
}

\value{
An object of class \code{\link[prevalence:prev-class]{prev}}.
}

\note{
Markov chain Monte Carlo sampling in \code{truePrevMulti2} is performed by \pkg{JAGS (Just Another Gibbs Sampler)} through the \pkg{\link[rjags:rjags]{rjags}} package. JAGS can be downloaded from \url{https://mcmc-jags.sourceforge.io/}.
}

\author{
Brecht Devleesschauwer <\email{brechtdv@gmail.com}>
}

\references{
\itemize{
  \item{Dendukuri N, Joseph L (2001) Bayesian approaches to modeling the conditional dependence between multiple diagnostic tests. \emph{Biometrics} \strong{57}:158-167}
  }
}

\seealso{
  \code{\link{define_x}}: how to define the vector of apparent test results \code{x}\cr
  \code{\link{define_prior2}}: how to define \code{prior}\cr
  
  \pkg{\link[coda:mcmc]{coda}} for various functions that can be applied to the \code{prev@mcmc} object\cr
  \code{\link{truePrevMulti}}: estimate true prevalence from apparent prevalence obtained by testing \emph{individual} samples with multiple tests, using a conditional probability scheme\cr
  \code{\link{truePrev}}: estimate true prevalence from apparent prevalence obtained by testing \emph{individual} samples with a single test\cr
  \code{\link{truePrevPools}}: estimate true prevalence from apparent prevalence obtained by testing \emph{pooled} samples\cr
  \code{\link{betaPERT}}: calculate the parameters of a Beta-PERT distribution\cr
  \code{\link{betaExpert}}: calculate the parameters of a Beta distribution based on expert opinion
}

\examples{
\dontrun{
## ===================================================== ##
## 2-TEST EXAMPLE: Strongyloides                         ##
## ----------------------------------------------------- ##
## Two tests were performed on 162 humans                ##
## -> T1 = stool examination                             ##
## -> T2 = serology test                                 ##
## Expert opinion generated the following priors:        ##
## -> SE1 ~ dbeta( 4.44, 13.31)                          ##
## -> SP1 ~ dbeta(71.25,  3.75)                          ##
## -> SE2 ~ dbeta(21.96,  5.49)                          ##
## -> SP2 ~ dbeta( 4.10,  1.76)                          ##
## The following results were obtained:                  ##
## -> 38 samples T1+,T2+                                 ##
## ->  2 samples T1+,T2-                                 ##
## -> 87 samples T1-,T2+                                 ##
## -> 35 samples T1-,T2-                                 ##
## ===================================================== ##

## how is the 2-test model defined?
define_x(2)
define_prior2(2)

## fit Strongyloides 2-test model
## a first model assumes conditional independence
## -> set covariance terms to zero
strongy_indep <-
truePrevMulti2(
  x = c(38, 2, 87, 35),
  n = 162,
  prior = {
    TP ~ dbeta(1, 1)
    SE[1] ~ dbeta( 4.44, 13.31)
    SP[1] ~ dbeta(71.25,  3.75)
    SE[2] ~ dbeta(21.96,  5.49)
    SP[2] ~ dbeta( 4.10,  1.76)
    a[1] <- 0
    b[1] <- 0
  })

## show model results
strongy_indep

## fit same model using 'list-style'
strongy_indep <-
truePrevMulti2(
  x = c(38, 2, 87, 35),
  n = 162,
  prior =
    list(
      TP = list(dist = "beta", alpha = 1, beta = 1),
      SE1 = list(dist = "beta", alpha = 4.44, beta = 13.31),
      SP1 = list(dist = "beta", alpha = 71.25, beta = 3.75),
      SE2 = list(dist = "beta", alpha = 21.96, beta = 5.49),
      SP2 = list(dist = "beta", alpha = 4.10, beta = 1.76),
      a1 = 0,
      b1 = 0
    )
  )

## show model results
strongy_indep

## fit Strongyloides 2-test model
## a second model allows for conditional dependence
## -> a[1] is the covariance between T1 and T2, given D+
## -> b[1] is the covariance between T1 and T2, given D-
## -> a[1] and b[1] can range between +/- 2^-h, ie, (-.25, .25)
strongy <-
truePrevMulti2(
  x = c(38, 2, 87, 35),
  n = 162,
  prior = {
    TP ~ dbeta(1, 1)
    SE[1] ~ dbeta( 4.44, 13.31)
    SP[1] ~ dbeta(71.25,  3.75)
    SE[2] ~ dbeta(21.96,  5.49)
    SP[2] ~ dbeta( 4.10,  1.76)
    a[1] ~ dunif(-0.25, 0.25)
    b[1] ~ dunif(-0.25, 0.25)
  })

## explore model structure
str(strongy)         # overall structure
str(strongy@par)     # structure of slot 'par'
str(strongy@mcmc)    # structure of slot 'mcmc'
strongy@model        # fitted model
strongy@diagnostics  # DIC, BGR and Bayes-P values

## standard methods
print(strongy)
summary(strongy)
par(mfrow = c(2, 2))
plot(strongy)           # shows plots of TP by default
plot(strongy, "SE[1]")  # same plots for SE1
plot(strongy, "SE[2]")  # same plots for SE2
plot(strongy, "SP[1]")  # same plots for SP1
plot(strongy, "SP[2]")  # same plots for SP2
plot(strongy, "a[1]")   # same plots for a[1]
plot(strongy, "b[1]")   # same plots for b[1]

## coda plots of all parameters
par(mfrow = c(2, 4)); densplot(strongy, col = "red")
par(mfrow = c(2, 4)); traceplot(strongy)
par(mfrow = c(2, 4)); gelman.plot(strongy)
par(mfrow = c(2, 4)); autocorr.plot(strongy)
}}