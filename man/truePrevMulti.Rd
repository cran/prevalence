\name{truePrevMulti}
\alias{truePrevMulti}

\title{Estimate true prevalence from individuals samples using multiple tests}

\description{
Bayesian estimation of true prevalence from apparent prevalence obtained by
applying \emph{multiple} tests to \emph{individual} samples.
\code{truePrevMulti} implements the approach described by Berkvens et al.
(2006), which uses a multinomial distribution to model observed test results,
and in which conditional dependence between tests is modelled through
conditional probabilities.
}

\usage{
truePrevMulti(x, n, prior,
              conf.level = 0.95, nchains = 2, burnin = 5000, update = 10000,
              verbose = FALSE, plot = FALSE)
}

\arguments{
  \item{x}{
    Vector of apparent test results;
	see 'Details' below
  }
  \item{n}{
    The total sample size
  }
  \item{prior}{
    The prior distribution for \code{theta};
	see 'Details' below
  }
  \item{conf.level}{
    The confidence level used in the construction of the confidence interval;
	defaults to 0.95
  }
  \item{nchains}{
    The number of chains used in the estimation process;
	must be \eqn{\ge 2}
  }
  \item{burnin}{
    The number of discarded model iterations;
	defaults to 5000
  }
  \item{update}{
    The number of withheld model iterations;
	defaults to 10000
  }
  \item{verbose}{
    Logical flag, indicating if JAGS process output should be printed to the
	R console; defaults to \code{FALSE}}
  \item{plot}{
    Logical flag, indicating if diagnostic plots should be given;
    defaults to \code{FALSE}}
}

\details{
  \code{truePrevMulti} calls on \pkg{JAGS}/\pkg{\link[rjags:rjags]{rjags}} 
  to estimate the true prevalence from the apparent prevalence in a Bayesian
  framework. \code{truePrevMulti} fits a multinomial model to the apparent
  test results obtained by testing individual samples with a given number of
  tests. To see the actual fitted model, see the model slot of the
  \link[prevalence:prev-class]{prev}-object.\cr

  The vector of apparent tests results, \code{x}, must contain the number of
  samples corresponding to each combination of test results. To see how this
  vector is defined for the number of tests \code{n} at hand, use
  \code{\link{definition_x}}.
  
  The prior in the multinomial model consists of a vector \code{theta}, which
  holds values for the true prevalence (TP), the sensitivity and specificity
  of the first test (SE1, SP1), and the conditional dependencies between the
  results of the subsequent tests and the preceding one(s). To see how this
  vector is defined for the number of tests \code{n} at hand, use
  \code{\link{definition_theta}}.\cr
  
  The values of \code{prior} can be specified in two ways, referred to as
  BUGS-style and list-style, respectively. See also below for some examples.\cr
  
  For BUGS-style specification, the values of \code{theta} should be given
  between curly brackets (i.e., \code{{}}), separated by line breaks.
  \code{theta} values can be specified to be deterministic (i.e., fixed),
  using the \code{<-} operator, or stochastic, using the \code{~} operator.
  In the latter case, the following distributions can be used:

\itemize{
  \item{\strong{Uniform:  }}{\code{dunif(min, max)}}
  \item{\strong{Beta:  }}{\code{dbeta(alpha, beta)}}
  \item{\strong{Beta-PERT:  }}{\code{dpert(min, mode, max)}}
}
  
  Alternatively, \code{theta} values can  be specified in a \emph{named}
  \code{list()} as follows:

\itemize{
  \item{\strong{Fixed:  }}{\code{list(dist = "fixed", par)}}
  \item{\strong{Uniform:  }}{\code{list(dist = "uniform", min, max)}}
  \item{\strong{Beta:  }}{\code{list(dist = "beta", alpha, beta)}}
  \item{\strong{Beta-PERT:  }}{\code{list(dist = "pert", method, a, m, b, k)}\cr
    \code{'method'} must be \code{"Classic"} or \code{"Vose"};\cr
    \code{'a'} denotes the pessimistic (minimum) estimate, \code{'m'} the most
	likely estimate, and \code{'b'} the optimistic (maximum) estimate;\cr
    \code{'k'} denotes the scale parameter.\cr
    See \link{betaPERT} for more information on Beta-PERT parametrization.}
  \item{\strong{Beta-Expert:  }}{\code{list(dist = "beta-expert", mode, mean,
  lower, upper, p)}\cr
    \code{'mode'} denotes the most likely estimate, \code{'mean'} the mean
	estimate;\cr
    \code{'lower'} denotes the lower bound, \code{'upper'} the upper bound;\cr
    \code{'p'} denotes the confidence level of the expert.\cr
	Only \code{mode} or \code{mean} should be specified; \code{lower} and
	\code{upper} can be specified together or alone.\cr
    See \link{betaExpert} for more information on Beta-Expert parametrization.}
}
}

\value{
An object of class \link[prevalence:prev-class]{prev}.
}

\note{
Markov chain Monte Carlo sampling in \code{truePrevMulti} is performed by
\pkg{JAGS (Just Another Gibbs Sampler)} through the
\pkg{\link[rjags:rjags]{rjags}} package. JAGS can be downloaded from
\url{http://sourceforge.net/projects/mcmc-jags/}.
}

\author{
\email{Brecht.Devleesschauwer@UGent.be}
}

\references{
\itemize{
  \item{
    Berkvens D, Speybroeck N, Praet N, Adel A, Lesaffre E (2006)
    \emph{Estimating disease prevalence in a Bayesian framework using
	probabilistic constraints.}
	Epidemiology 17; 145-153
	}
  \item{
    Habib I, Sampers I, Uyttendaele M, De Zutter L, Berkvens D (2008)
    \emph{A Bayesian modelling framework to estimate Campylobacter prevalence
	and culture methods sensitivity: application to a chicken meat survey in
	Belgium.}
	Journal of Applied Microbiology 105; 2002-2008
	}
  \item{
    Geurden T, Berkvens D, Casaert S, Vercruysse J, Claerebout E (2008)
    \emph{A Bayesian evaluation of three diagnostic assays for the detection
	of Giardia duodenalis in symptomatic and asymptomatic dogs.}
	Veterinary Parasitology 157; 14-20
	}
  }
}

\seealso{
  \code{\link{definition_x}}:
  how to define the vector of apparent test results \code{x}\cr
  \code{\link{definition_theta}}:
  how to define the vector of \code{theta} values in \code{prior}\cr
  
  \pkg{\link[coda:mcmc]{coda}}
  for various functions that can be applied to the \code{prev@mcmc} object\cr
  \code{\link{truePrev}}:
  estimate true prevalence from apparent prevalence obtained by testing
  \emph{individual} samples with a single test\cr
  \code{\link{truePrevPools}}:
  estimate true prevalence from apparent prevalence obtained by testing
  \emph{pooled} samples\cr
  \code{\link{betaPERT}}:
  calculate the parameters of a Beta-PERT distribution\cr
  \code{\link{betaExpert}}:
  calculate the parameters of a Beta distribution based on expert opinion
}

\examples{
\dontrun{
## ===================================================== ##
## 2-TEST EXAMPLE: Campylobacter                         ##
## ----------------------------------------------------- ##
## Two tests were performed on 656 chicken meat samples  ##
## -> T1 = enrichment culture                            ##
## -> T2 = direct plating                                ##
## The following assumption were made:                   ##
## -> TP is larger than 45\% and smaller than 80\%         ##
## -> SE1 must lie within 24\% and 50\%                    ##
## -> SP1 and SP2 both equal 100\%                        ##
## -> beta(30, 12) describes P(T2+|D+,T1+)               ##
## The following results were obtained:                  ##
## -> 341 samples T1-,T2-                                ##
## -> 156 samples T1-,T2+                                ##
## ->  46 samples T1+,T2-                                ##
## -> 113 samples T1+,T2+                                ##
## ===================================================== ##

## how is the 2-test model defined?
definition_x(2)
definition_theta(2)

## fit campylobacter 2-test model
campy <-
truePrevMulti(
  x = c(341, 156, 46, 113),
  n = 656,
  prior = {
    theta[1] ~ dunif(0.45, 0.80)
    theta[2] ~ dunif(0.24, 0.50)
    theta[3] <- 1
    theta[4] ~ dbeta(30, 12)
    theta[5] ~ dbeta(1, 1)
    theta[6] <- 1
    theta[7] <- 1
  }
)

## fit same model using 'list-style'
campy <-
truePrevMulti(
  x = c(341, 156, 46, 113),
  n = 656,
  prior =
    list(
      theta1 = list(dist = "uniform", min = 0.45, max = 0.80),
      theta2 = list(dist = "uniform", min = 0.24, max = 0.50),
      theta3 = 1,
      theta4 = list(dist = "beta", alpha = 30, beta = 12),
      theta5 = list(dist = "beta", alpha = 1, beta = 1),
      theta6 = 1,
      theta7 = 1
    )
)

## show model results
campy

## explore model structure
str(campy)         # overall structure
str(campy@par)     # structure of slot 'par'
str(campy@mcmc)    # structure of slot 'mcmc'
campy@model        # fitted model
campy@diagnostics  # DIC, BGR and Bayes-P values

## standard methods
print(campy)
summary(campy)
par(mfrow = c(2, 2))
plot(campy)         # shows plots of TP by default
plot(campy, "SE1")  # same plots for SE1
plot(campy, "SE2")  # same plots for SE2

## coda plots of TP, SE1, SE2
par(mfrow = c(1, 3))
densplot(campy, col = "red")
traceplot(campy)
gelman.plot(campy)
autocorr.plot(campy)


## ===================================================== ##
## 3-TEST EXAMPLE: Giardia                               ##
## ----------------------------------------------------- ##
## Three tests were performed on stools from 272 dogs    ##
## -> T1 = immunofluorescence assay                      ##
## -> T2 = direct microscopy                             ##
## -> T3 = SNAP immunochromatography                     ##
## The following assumption were made:                   ##
## -> TP is smaller than 20\%                             ##
## -> SE1 must be higher than 80\%                        ##
## -> SP1 must be higher than 90\%                        ##
## The following results were obtained:                  ##
## -> 220 samples T1-,T2-,T3-                            ##
## ->   3 samples T1-,T2-,T3+                            ##
## ->  14 samples T1-,T2+,T3-                            ##
## ->   1 sample  T1-,T2+,T3+                            ##
## ->  12 samples T1+,T2-,T3-                            ##
## ->  12 samples T1+,T2-,T3+                            ##
## ->   4 samples T1+,T2+,T3-                            ##
## ->   6 samples T1+,T2+,T3+                            ##
## ===================================================== ##

## how is the 3-test model defined?
definition_x(3)
definition_theta(3)

## fit giardia 3-test model
giardia <-
truePrevMulti(
  x = c(220, 3, 14, 1, 12, 12, 4, 6),
  n = 272,
  prior = {
    theta[1] ~ dunif(0.00, 0.20)
    theta[2] ~ dunif(0.90, 1.00)
    theta[3] ~ dunif(0.80, 1.00)
    theta[4] ~ dbeta(1, 1)
    theta[5] ~ dbeta(1, 1)
    theta[6] ~ dbeta(1, 1)
    theta[7] ~ dbeta(1, 1)
    theta[8] ~ dbeta(1, 1)
    theta[9] ~ dbeta(1, 1)
    theta[10] ~ dbeta(1, 1)
    theta[11] ~ dbeta(1, 1)
    theta[12] ~ dbeta(1, 1)
    theta[13] ~ dbeta(1, 1)
    theta[14] ~ dbeta(1, 1)
    theta[15] ~ dbeta(1, 1)
  }
)

## show model results
giardia

## coda densplots
par(mfcol = c(2, 4))
densplot(giardia, col = "red")
}}