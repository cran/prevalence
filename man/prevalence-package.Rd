\name{prevalence-package}

\alias{prevalence-package}
\alias{prevalence}

\docType{package}

\title{Tools for prevalence assessment studies}

\description{
  The \pkg{prevalence} package provides Frequentist and Bayesian methods useful in prevalence assessment studies. Visit \url{http://prevalence.cbra.be/} for more information and tutorials.
}

\details{
  \tabular{ll}{
    Package: \tab prevalence\cr
    Type: \tab Package\cr
    Version: \tab 0.4.1\cr
    Date: \tab 2022-06-03\cr
    BugReports: \tab \url{https://github.com/brechtdv/prevalence/issues}\cr
    Depends: \tab R (>= 4.0.0), rjags, coda, methods\cr
    SystemRequirements: \tab JAGS (>= 3.2.0) (see \url{https://mcmc-jags.sourceforge.io/})\cr
    License: \tab GNU >= 2\cr
}

Available functions in the \pkg{prevalence} package:
  \tabular{ll}{
    \code{\link{propCI}} \tab Derive confidence intervals for an apparent prevalence estimate\cr
    \code{\link{truePrev}} \tab Estimate TP from AP obtained by testing individual samples with a single test\cr
	\code{\link{truePrevMulti}} \tab Estimate TP from AP obtained by testing individual samples with multiple tests, using a conditional probability scheme\cr
  \code{\link{truePrevMulti2}} \tab Estimate TP from AP obtained by testing individual samples with multiple tests, using a covariance scheme\cr
    \code{\link{truePrevPools}} \tab Estimate TP from AP obtained by testing pooled samples\cr

    \code{\link{betaPERT}} \tab Calculate the parameters of a Beta-PERT distribution\cr
	\code{\link{betaExpert}} \tab Calculate the parameters of a Beta distribution based on expert opinion
  }

\strong{IMPORTANT}: the \code{truePrev} functions in the \pkg{prevalence} package call on \pkg{JAGS (Just Another Gibbs Sampler)}, through the \pkg{\link[rjags:rjags]{rjags}} package. Therefore, JAGS has to be installed on the user's system.\cr\cr
JAGS can be downloaded from \url{https://mcmc-jags.sourceforge.io/}
}

\author{
  \strong{Creator, Maintainer}\cr
  Brecht Devleesschauwer <\email{brechtdv@gmail.com}>\cr\cr
  \strong{Contributors}\cr
  Paul Torgerson, Johannes Charlier, Bruno Levecke, Nicolas Praet, Sophie Roelandt, Suzanne Smit, Pierre Dorny, Dirk Berkvens, Niko Speybroeck
}

\keyword{package}
