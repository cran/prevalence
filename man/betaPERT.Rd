\name{betaPERT}
\alias{betaPERT}
\alias{print.betaPERT}
\alias{plot.betaPERT}

\title{Calculate the parameters of a Beta-PERT distribution}

\description{
The Beta-PERT methodology allows to parametrize a generalized Beta distribution based on expert opinion regarding
a pessimistic estimate (minimum value), a most likely estimate (mode), 
and an optimistic estimate (maximum value). The \code{betaPERT} function incorporates two methods of
calculating the parameters of a Beta-PERT distribution, designated \code{"Classic"} and \code{"Vose"}.
}

\usage{
betaPERT(a, m, b, k = 4, method = "classic", p = 0.95, plot = FALSE)
}

\arguments{
  \item{a}{Pessimistic estimate (Minimum value)}
  \item{m}{Most likely estimate (Mode)}
  \item{b}{Optimistic estimate (Maximum value)}
  \item{k}{Scale parameter}
  \item{method}{\code{"Classic"} or \code{"Vose"}; see details below}
  \item{p}{Confidence level for printing confidence intervals}
  \item{plot}{Logical flag: should resulting distribution be plotted?}
}

\details{
The Beta-PERT methodology was developed in the context of Program Evaluation and Review Technique (PERT). 
Based on a pessimistic estimate (minimum value), a most likely estimate (mode), 
and an optimistic estimate (maximum value), typically derived through expert elicitation, 
the parameters of a Beta distribution can be calculated. The Beta-PERT distribution is used in stochastic modeling 
and risk assessment studies to reflect uncertainty regarding specific parameters.

Different methods exist in literature for defining the parameters of a Beta distribution based on PERT.
The two most common methods are included in the \code{BetaPERT} function:
\describe{
  \item{\strong{Classic}: }{The standard formulas for mean, standard deviation, \eqn{\alpha} and \eqn{\beta}, are as follows:
    \deqn{mean = \frac{a + k*m + b}{k + 2}}{mean = (a + k*m + b) / (k + 2)}
    \deqn{sd = \frac{b - a}{k + 2}}{sd = (b - a) / (k + 2)}
    \deqn{\alpha = \frac{mean - a}{b - a} * \left\{ (mean - a) * \frac{b - mean}{sd^{2}} - 1 \right\} }{
      \alpha = \{ (mean - a) / (b - a) \} * \{ (mean - a) * (b - mean) / sd^{2} - 1 \} }
    \deqn{\beta = \alpha * \frac{b - mean}{mean - a}}{\beta = \alpha * (b - mean) / (mean - a)}
    The resulting distribution is a 4-parameter Beta distribution: Beta(\eqn{\alpha}, \eqn{\beta}, a, b).\cr

  }
  \item{\strong{Vose}: }{Vose (2000) describes a different formula for \eqn{\alpha}:
    \deqn{(mean - a) * \frac{2 * m - a - b}{(m - mean) * (b - a)}}{(mean - a) * (2 * m - a - b) / \{ (m - mean) * (b - a) \}}
    Mean and \eqn{\beta} are calculated using the standard formulas; as for the classical PERT,
    the resulting distribution is a 4-parameter Beta distribution: Beta(\eqn{\alpha}, \eqn{\beta}, a, b).\cr\cr
    Note: If \eqn{m = mean}, \eqn{\alpha} is calculated as \eqn{1 + k/2}, in accordance with the \pkg{mc2d} package (see 'Note').\cr

  }
}
}

\value{
A list of class \code{"betaPERT"}:
\item{alpha }{Parameter \eqn{\alpha} (shape1) of the Beta distribution}
\item{beta }{Parameter \eqn{\beta} (shape2) of the Beta distribution}
\item{a }{Pessimistic estimate (Minimum value)}
\item{m }{Most likely estimate (Mode)}
\item{b }{Optimistic estimate (Maximum value)}
\item{method }{Applied method}
\item{p }{Confidence level for printing confidence intervals}

Available generic functions for class \code{"betaPERT"} are \code{print} and \code{plot}.

}

\references{
\describe{
  \item{\strong{Classic}: }{
    Malcolm DG, Roseboom JH, Clark CE, Fazar W (1959) Application of a technique for research and development program evaluation. \emph{Oper Res} \strong{7}(5):646-669.
  }
  \item{\strong{Vose}: }{
    David Vose. \emph{Risk analysis, a quantitative guide, 2nd edition.} Wiley and Sons, 2000.\cr
    \href{http://vosesoftware.com/ModelRiskHelp/index.htm\#Distributions/Continuous_distributions/PERT_distribution.htm}{PERT distribution in \emph{ModelRisk} (Vose software)}
  }
}
}

\author{
\email{Brecht.Devleesschauwer@UGent.be}
}

\note{
The \pkg{\href{http://cran.r-project.org/web/packages/mc2d/}{mc2d}} package provides 
the probability density function, cumulative distribution function, quantile function and random number generation function 
for the PERT distribution, parametrized by the \code{"Vose"} method.
}

\seealso{
\code{\link{betaExpert}}, for modelling a standard Beta distribution based on expert opinion
}

\examples{
## The value of a parameter of interest is believed to lie between 0 and 50
## The most likely value is believed to be 10

# Classical PERT
betaPERT(a = 0, m = 10, b = 50)

# Vose parametrization
betaPERT(a = 0, m = 10, b = 50, method = "Vose")
}

\keyword{Expert}
\keyword{PERT}
