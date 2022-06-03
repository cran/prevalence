## prevalence

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/prevalence)](https://cran.r-project.org/package=prevalence)
[![Cranlogs Downloads](http://cranlogs.r-pkg.org/badges/grand-total/prevalence)](https://cran.r-project.org/package=prevalence)

The prevalence package provides Frequentist and Bayesian methods useful in prevalence assessment studies. Several methods are available for estimating True Prevalence (TP) from Apparent Prevalence (AP).

#### Available functions
<table>
<tr><td><code>propCI</code></td><td>Derive confidence intervals for a prevalence estimate</td></tr>
<tr><td><code>truePrev</code></td><td>Estimate TP from AP obtained by testing individual samples with a single test</td></tr>
<tr><td><code>truePrevMulti</code></td><td>Estimate TP from AP obtained by testing individual samples with multiple tests, using a conditional probability scheme</td></tr>
<tr><td><code>truePrevMulti2</code></td><td>Estimate TP from AP obtained by testing individual samples with multiple tests, using a covariance scheme</td></tr>
<tr><td><code>truePrevPools</code></td><td>Estimate TP from AP obtained by testing pooled samples</td></tr>
<tr><td><code>betaPERT</code></td><td>Calculate the parameters of a Beta-PERT distribution</td></tr>
<tr><td><code>betaExpert</code></td><td>Calculate the parameters of a Beta distribution based on expert opinion </td></tr>
</table>

#### Install

To download and install the latest released version from [CRAN](https://cran.r-project.org/package=prevalence):
```r
install.packages("prevalence")
```

To download and install the latest development version from GitHub:
```r
devtools::install_github("brechtdv/prevalence")
```

**IMPORTANT**: the truePrev functions in the prevalence package call on JAGS (Just Another Gibbs Sampler), which therefore has to be available on the user's system. JAGS can be downloaded from http://mcmc-jags.sourceforge.net/.

#### More

Function `truePrev` is also available as an online Shiny application: https://cbra.shinyapps.io/truePrev/

More information and tutorials are available at http://prevalence.cbra.be/
