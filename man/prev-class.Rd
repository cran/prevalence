\name{prev-class}
\Rdversion{1.1}
\docType{class}
\alias{prev-class}

\title{Class \code{"prev"}}
\description{
The \code{"prev"} class represents output from Bayesian true prevalence
estimation models.
}

\section{Objects from the Class}{
Objects of class \code{"prev"} are created by \code{\link{truePrev}}, \code{\link{truePrevMulti}}, \code{\link{truePrevMulti2}} and \code{\link{truePrevPools}}.
}

\section{Slots}{
Objects of class \code{"prev"} contain the following four slots:
  \describe{
    \item{\code{par}:}{
	  A list of input parameters
	}
    \item{\code{model}:}{
	  The fitted Bayesian model, in BUGS language (S3 class \code{"prevModel"}) 
	}
    \item{\code{mcmc}:}{
	  A list, with one element per chain, of the simulated true prevalences, sensitivities and specificities
	}
    \item{\code{diagnostics}:}{
	  A list with elements for the Deviance Information Criterion (\code{$DIC}), the Brooks-Gelman-Rubin statistic (\code{$BGR}), and in the case of \code{\link{truePrevMulti}} and \code{\link{truePrevMulti2}}, the Bayes-P statistic
	  (\code{$bayesP})
	}
  }
}

\author{
Brecht Devleesschauwer <\email{brechtdv@gmail.com}>
}

\seealso{
  \code{\link{truePrev}}, \code{\link{truePrevMulti}}, \code{\link{truePrevMulti2}}, \code{\link{truePrevPools}}\cr
  \code{\link{show-methods}}, \code{\link{print-methods}}, \code{\link{summary-methods}}, \code{\link{convert-methods}}, \code{\link{plot-methods}}, \code{\link{plot-methods-coda}}
}

\examples{
## Taenia solium cysticercosis in Nepal
SE <- list(dist = "uniform", min = 0.60, max = 1.00)
SP <- list(dist = "uniform", min = 0.75, max = 1.00)
TP <- truePrev(x = 142, n = 742, SE = SE, SP = SP)

## Summarize estimates per chain
summary(TP)

## Diagnostic plots
par(mfrow = c(2, 2))
plot(TP)

## Generic plots from package coda
par(mfrow = c(1, 1))
densplot(TP)
traceplot(TP)
gelman.plot(TP)
autocorr.plot(TP)

## Use 'slotNames()' to see the slots of object TP
slotNames(TP)

## Every slot can be accessed using the '@' operator
## Use 'str()' to see the structure of each object
str(TP@par)          # input parameters
str(TP@model)        # fitted model
str(TP@mcmc)         # simulated TP, SE, SP
str(TP@diagnostics)  # DIC and BGR (and bayesP)

## Each element of TP@mcmc inherits from coda class 'mcmc.list'
## List all available methods for this class
methods(class = "mcmc.list")
## List all available functions in the coda package
library(help = "coda")

## Highest Posterior Density interval, from coda package
coda::HPDinterval(TP@mcmc$TP)
}

\keyword{classes}
