print.betaPERT <-
function(x, conf.level, ...){
  ## summary statistics
  beta_mean <- (x$alpha / (x$alpha + x$beta)) * (x$b - x$a) + x$a
  beta_var <- (x$alpha * x$beta * (x$b - x$a) ^ 2) /
              ( ((x$alpha + x$beta) ^ 2) * (x$alpha + x$beta + 1))
  beta_med <- qbeta(.5, x$alpha, x$beta) * (x$b - x$a) + x$a

  ## quantiles
  if (missing(conf.level)) conf.level <- x$p
  ci <- c(0, conf.level) + (1 - conf.level) / 2
  beta_lwr <- qbeta(ci[1], x$alpha, x$beta) * (x$b - x$a) + x$a
  beta_upr <- qbeta(ci[2], x$alpha, x$beta) * (x$b - x$a) + x$a
  ciLabel <- paste(100 * ci, "%", sep = "")

  ## create 'out' dataframe
  out <- data.frame(x$method, x$alpha, x$beta, x$a, x$b, 
                    beta_mean, beta_med, x$m, beta_var, beta_lwr, beta_upr)
  colnames(out) <- c("method", "alpha", "beta", "a", "b",
                     "mean", "median", "mode", "var", ciLabel)

  ## print 'out' dataframe
  print(out)
}

print.betaExpert <-
function(x, conf.level = .95, ...){
  ## summary statistics
  beta_mean <- x$alpha / (x$alpha + x$beta)
  if (x$alpha > 1 & x$beta > 1){
    beta_mode <- (x$alpha - 1) / (x$alpha + x$beta - 2)
  } else if (x$alpha == 1) {
    beta_mode <- 0
  } else if (x$beta == 1){
    beta_mode <- 1
  } else if (x$alpha == x$beta) {
    beta_mode <- NA
  } else {
    beta_mode <- ifelse(x$alpha > x$beta, 1, 0)
  }
  beta_var <- (x$alpha * x$beta) /
              ( ((x$alpha + x$beta) ^ 2) * (x$alpha + x$beta + 1))
  beta_med <- qbeta(.5, x$alpha, x$beta)

  ## quantiles
  ci <- c(0, conf.level) + (1 - conf.level) / 2
  beta_lwr <- qbeta(ci[1], x$alpha, x$beta)
  beta_upr <- qbeta(ci[2], x$alpha, x$beta)
  ciLabel <- paste(100 * ci, "%", sep = "")

  ## create 'out' dataframe
  out <- data.frame(x$alpha, x$beta,
                    beta_mean, beta_med, beta_mode,
                    beta_var, beta_lwr, beta_upr)
  colnames(out) <- c("alpha", "beta",
                     "mean", "median", "mode",
                     "var", ciLabel)

  ## print 'out' dataframe
  print(out)
}

print.prevModel <-
function(x, ...){
  l <- length(x)
  spacer <- 0
  for (i in seq(l)){
    if (substr(x[i], nchar(x[i]), nchar(x[i])) == "}")
      spacer <- spacer - 1
    cat(rep(" ", 2 * spacer), x[i], "\n", sep = "")
    if (substr(x[i], nchar(x[i]), nchar(x[i])) == "{")
      spacer <- spacer + 1
  }
}
