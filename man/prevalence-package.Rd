\name{prevalence-package}

\alias{prevalence-package}
\alias{prevalence}

\docType{package}

\title{Tools for prevalence assessment studies}

\description{
  The \pkg{prevalence} package provides Frequentist and Bayesian methods useful in prevalence assessment studies.
}

\details{
  \tabular{ll}{
    Package: \tab prevalence\cr
    Type: \tab Package\cr
    Version: \tab 0.2.0\cr
    Date: \tab 2013-09-22\cr
    License: \tab GNU >= 2\cr
}

Available functions in the \pkg{prevalence} package:
  \tabular{ll}{
    \code{\link[prevalence:truePrev]{truePrev}} \tab Estimate TP from AP obtained by testing individual samples with a single test\cr
	\code{\link[prevalence:truePrevMulti]{truePrevMulti}} \tab Estimate TP from AP obtained by testing individual samples with multiple tests\cr
    \code{\link[prevalence:truePrevPools]{truePrevPools}} \tab Estimate TP from AP obtained by testing pooled samples\cr
	\code{\link[prevalence:propCI]{propCI}} \tab Derive confidence intervals for a prevalence estimate\cr
    \code{\link[prevalence:betaPERT]{betaPERT}} \tab Calculate the parameters of a Beta-PERT distribution\cr
	\code{\link[prevalence:betaExpert]{betaExpert}} \tab Calculate the parameters of a Beta distribution based on expert opinion
  }

\strong{IMPORTANT}: the \code{truePrev} functions in the \pkg{prevalence} package call on \pkg{JAGS (Just Another Gibbs Sampler)}, through the \pkg{\link[rjags:rjags]{rjags}} package. Therefore, JAGS has to be installed on the user's system.\cr\cr
JAGS can be downloaded from \url{http://sourceforge.net/projects/mcmc-jags/}
}

\author{
  \strong{Creator, Maintainer}\cr
  \email{Brecht.Devleesschauwer@UGent.be}\cr\cr
  \strong{Contributors}\cr
  Paul Torgerson, Johannes Charlier, Bruno Levecke, Nicolas Praet, Pierre Dorny, Dirk Berkvens, Niko Speybroeck
}

\keyword{package}
