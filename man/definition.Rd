\name{definition}
\alias{definition_x}
\alias{definition_theta}

\title{Definition of \code{truePrevMulti} model}

\description{
These two utility functions can help to specify function
\code{\link{truePrevMulti}}.
}

\usage{
definition_x(n)
definition_theta(n)
}

\arguments{
  \item{n}{
    Number of tests
  }
}

\details{
  Function \code{\link{truePrevMulti}} estimates true prevalence from
  individual samples tested with \code{n} tests, using the approach of
  Berkvens et al. (2006).\cr

  The vector of apparent tests results, \code{x}, must contain the number of
  samples corresponding to each combination of test results. The model assumes
  that the first value corresponds to the number of samples that tested
  negative on all tests and that the last value corresponds to the number
  of samples that tested positive on all tests.\cr
  
  The prior in the multinomial model consists of a vector \code{theta}, which
  holds values for the true prevalence (TP), the sensitivity and specificity
  of the first test (SE1, SP1), and the conditional dependencies between the
  results of the subsequent tests and the preceding one(s).
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
  }
}

\seealso{
  \code{\link{truePrevMulti}}
}

\examples{
## how is a 2-test model defined?

definition_x(2)
# Definition of the apparent test results, 'x', for 2 tests:
# x[1] : T1-,T2- 
# x[2] : T1-,T2+ 
# x[3] : T1+,T2- 
# x[4] : T1+,T2+

definition_theta(2)
# Definition of the prior, 'theta', for 2 tests: 
# theta[1] : P(D+) = TP
# theta[2] : P(T1+|D+) = SE1
# theta[3] : P(T1-|D-) = SP1
# theta[4] : P(T2+|D+,T1+)
# theta[5] : P(T2+|D+,T1-)
# theta[6] : P(T2-|D-,T1-)
# theta[7] : P(T2-|D-,T1+)

## how is a 3-test model defined?

definition_x(3)
# Definition of the apparent test results, 'x', for 3 tests:
# x[1] : T1-,T2-,T3- 
# x[2] : T1-,T2-,T3+ 
# x[3] : T1-,T2+,T3- 
# x[4] : T1-,T2+,T3+ 
# x[5] : T1+,T2-,T3- 
# x[6] : T1+,T2-,T3+ 
# x[7] : T1+,T2+,T3- 
# x[8] : T1+,T2+,T3+

definition_theta(3)
# Definition of the prior, 'theta', for 3 tests: 
# theta[1] : P(D+) = TP
# theta[2] : P(T1+|D+) = SE1
# theta[3] : P(T1-|D-) = SP1
# theta[4] : P(T2+|D+,T1+)
# theta[5] : P(T2+|D+,T1-)
# theta[6] : P(T2-|D-,T1-)
# theta[7] : P(T2-|D-,T1+)
# theta[8] : P(T3+|D+,T1+,T2+)
# theta[9] : P(T3+|D+,T1+,T2-)
# theta[10] : P(T3+|D+,T1-,T2+)
# theta[11] : P(T3+|D+,T1-,T2-)
# theta[12] : P(T3-|D-,T1-,T2-)
# theta[13] : P(T3-|D-,T1-,T2+)
# theta[14] : P(T3-|D-,T1+,T2-)
# theta[15] : P(T3-|D-,T1+,T2+)
}
