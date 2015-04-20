\name{convert-methods}
\docType{methods}
\alias{convert-methods}
\alias{as.matrix-methods}
\alias{as.matrix,prev-method}

\title{Methods for Function \code{as.matrix} in Package \pkg{prevalence}}

\description{Convert objects of class \code{prev} to matrix}

\usage{\S4method{as.matrix}{prev}(x, iters = FALSE, chains = FALSE)}

\arguments{
  \item{x}{An object of class \code{prev}}
  \item{iters}{Logical flag, indicating whether a column should be added for iteration number; defaults to \code{FALSE}}
  \item{chains}{Logical flag, indicating whether a column should be added for chain number; defaults to \code{FALSE}}
}

\section{Methods}{
  \describe{
    \item{\code{signature(x = "prev")}}{
      Convert objects of class \code{prev} to \code{\link{matrix}}
    }
  }
}

\seealso{
  \code{\link{prev-class}}
}

\examples{\dontrun{

## Taenia solium cysticercosis 1-test model
cysti <-
truePrev(x = 142, n = 742,
         SE = ~dunif(0.60, 1.00), SP = ~dunif(0.75, 1.00))

head(as.matrix(cysti))


## Campylobacter 2-test model
campy <-
truePrevMulti(
  x = c(113, 46, 156, 341),
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

head(as.matrix(campy, iters = TRUE, chains = TRUE))
}}

\keyword{methods}
