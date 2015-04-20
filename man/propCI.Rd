\name{propCI}
\alias{propCI}

\title{Calculate confidence intervals for prevalences and other proportions}

\description{
The \code{propCI} function calculates five types of confidence intervals for proportions:
\itemize{
  \item{Wald interval (= Normal approximation interval, asymptotic interval)}
  \item{Agresti-Coull interval (= adjusted Wald interval)}
  \item{Exact interval (= Clopper-Pearson interval)}
  \item{Jeffreys interval (= Bayesian interval)}
  \item{Wilson score interval}
}
}

\usage{
propCI(x, n, method = "all", level = 0.95, sortby = "level")
}

\arguments{
  \item{x}{Number of successes (positive samples)}
  \item{n}{Number of trials (sample size)}
  \item{method}{Confidence interval calculation method; see details}
  \item{level}{Confidence level for confidence intervals}
  \item{sortby}{Sort results by \code{"level"} or \code{"method"}}
}

\details{
Five methods are available for calculating confidence intervals. For convenience, synonyms are allowed. Please refer to the PDF version of the manual for proper formatting of the below formulas.
\describe{
  \item{\code{"agresti.coull", "agresti-coull", "ac"}}{
    \deqn{\tilde{n} = n + z_{1-\frac{\alpha}{2}}^2}
    \deqn{\tilde{p} = \frac{1}{\tilde{n}}(x + \frac{1}{2} z_{1-\frac{\alpha}{2}}^2)}
	\deqn{\tilde{p} \pm z_{1-\frac{\alpha}{2}} \sqrt{\frac{\tilde{p}(1-\tilde{p})}{\tilde{n}}}}
  }
  \item{\code{"exact", "clopper-pearson", "cp"}}{
    \deqn{(Beta(\frac{\alpha}{2}; x, n - x + 1), Beta(1 - \frac{\alpha}{2}; x + 1, n - x))}
  }
  \item{\code{"jeffreys", "bayes"}}{
    \deqn{(Beta(\frac{\alpha}{2}; x + 0.5, n - x + 0.5), Beta(1 - \frac{\alpha}{2}; x + 0.5, n - x + 0.5))}
  }
  \item{\code{"wald", "asymptotic", "normal"}}{
    \deqn{p \pm z_{1-\frac{\alpha}{2}} \sqrt{\frac{p(1-p)}{n}}}
  }
  \item{\code{"wilson"}}{
    \deqn{
	  \frac{p + \frac{z_{1-\frac{\alpha}{2}}^2}{2n} \pm z_{1-\frac{\alpha}{2}} \sqrt{\frac{p(1-p)}{n} + \frac{z_{1-\frac{\alpha}{2}}^2}{4n^2}}}
	       {1 + \frac{z_{1-\frac{\alpha}{2}}^2}{n}}
	}
  }
}
}

\note{
In case the observed prevalence equals 0\% (ie, \code{x == 0}), an upper one-sided confidence interval is returned. 
In case the observed prevalence equals 100\% (ie, \code{x == n}), a lower one-sided confidence interval is returned. 
In all other cases, two-sided confidence intervals are returned.
}

\value{
Data frame with seven columns:
  \item{x }{Number of successes (positive samples)}
  \item{n }{Number of trials (sample size)}
  \item{p }{Proportion of successes (prevalence)}
  \item{method }{Confidence interval calculation method}
  \item{level }{Confidence level}
  \item{lower }{Lower confidence limit}
  \item{upper }{Upper confidence limit}
}

\author{
Brecht Devleesschauwer <\email{brechtdv@gmail.com}>
}

\examples{
## All methods, 95% confidence intervals
propCI(x = 142, n = 742)

## Wald-type 90%, 95% and 99% confidence intervals
propCI(x = 142, n = 742, method = "wald", level = c(0.90, 0.95, 0.99))
}

\keyword{prevalence}
\keyword{confidence interval}
