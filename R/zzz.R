##= Define S4 classes =====================================================
setOldClass("prevModel")  # virtual S3 class
setOldClass("mcmc.list")  # virtual S3 class
setClass("prev",
  representation(
    par = "list",
    model = "prevModel",
    mcmc = "list",
    diagnostics = "list"))

##= Define S4 methods =====================================================
setMethod("show", "prev",
  function(object) print(object, object@par$conf.level)
)

setMethod("print", "prev",
  function(x, conf.level, dig = 3, ...){
    ## get 'conf.level' from 'x', but allow to override
    if (missing(conf.level)) conf.level <- x@par$conf.level

    ## get summary statistics from 'summary()'
    stats <- summary(x, conf.level)
    stats <- as.data.frame(t(stats[nrow(stats), ]))  # 'with()' needs df

    ## get BGR statistic
    BGR <- x@diagnostics$BGR

    ## define 'out' dataframe
    out <- subset(stats, select = c(1, 2, 3, 6, 7))
    rownames(out) <- "True prevalence (%)"

    ## print 'out' dataframe
    print(round(100 * out, dig), ...)

    ## print diagnostic information
    cat("\nBGR statistic = ", round(BGR[[1]], 4),
        " (upper CL = ", round(BGR[[2]], 4), ")\n", sep = "")
    cat("BGR values substantially above 1 indicate lack of convergence\n")
  }
)

setMethod("summary", "prev",
  function(object, conf.level){
    ## get 'conf.level' from 'object', but allow to override
    if (missing(conf.level)) conf.level <- object@par$conf.level

    ## derive lower and upper confidence level
    if (sum(object@par$x) == 0){
      p <- c(0, conf.level)
    } else if (ifelse(length(object@par$x) == 1,
                 object@par$x == object@par$n,
                 sum(object@par$x) == length(object@par$x))){
      p <- c(1 - conf.level, 1)
    } else {
      p <- c((1 - conf.level) / 2,
              1 - (1 - conf.level) / 2)
    }
    ciLabel <- paste(100 * p, "%", sep = "")

    ## define 'stats' matrix
    n <- object@par$nchains
    stats <- matrix(ncol = 8, nrow = n + 1)
    colnames(stats) <- list("mean", "median", "mode", "sd", "var",
                            ciLabel[1], ciLabel[2], "samples")
    dimnames(stats)[[1]] <- c(paste(rep("chain", n), seq(n)), "all chains")

    ## calculate summary statistics per chain
    for (i in seq(object@par$nchains)){
      stats[i, 1] <- mean(object@mcmc[[i]], na.rm = TRUE)
      stats[i, 2] <- median(object@mcmc[[i]], na.rm = TRUE)
      d <- density(object@mcmc[[i]], na.rm = TRUE)
      stats[i, 3] <- d$x[which.max(d$y)]
      stats[i, 4] <- sd(object@mcmc[[i]][, 1], na.rm = TRUE)
      stats[i, 5] <- var(object@mcmc[[i]], na.rm = TRUE)
      stats[i, 6] <- quantile(object@mcmc[[i]], probs = p[1], na.rm = TRUE)
      stats[i, 7] <- quantile(object@mcmc[[i]], probs = p[2], na.rm = TRUE)
      stats[i, 8] <- length(object@mcmc[[i]])
    }

    ## calculate overall summary statistics
    y <- unlist(object@mcmc)
    i <- i + 1

    stats[i, 1] <- mean(y, na.rm = TRUE)
    stats[i, 2] <- median(y, na.rm = TRUE)
    d <- density(y, na.rm = TRUE)
    stats[i, 3] <- d$x[which.max(d$y)]
    stats[i, 4] <- sd(y, na.rm = TRUE)
    stats[i, 5] <- var(y, na.rm = TRUE)
    stats[i, 6] <- quantile(y, probs = p[1], na.rm = TRUE)
    stats[i, 7] <- quantile(y, probs = p[2], na.rm = TRUE)
    stats[i, 8] <- length(y)

    ## return resulting 'stats' matrix
    return(stats)
  }
)

setMethod("plot", "prev",
  function(x, y = NULL, ...){
    ## Define 'ask'
    ask_old <- par("ask")
    ask_new <- ifelse(prod(par("mfrow")) == 4, FALSE, TRUE)
    devAskNewPage(ask_new)
    on.exit(devAskNewPage(ask_old))

    ## 4 Plots
    densplot(x@mcmc, main = "Density plot", ylab = "density", ask = FALSE)
    traceplot(x@mcmc, main = "Trace plot", ylab = "prevalence", ask = TRUE)
    gelman.plot(x@mcmc, ask = TRUE, auto.layout = FALSE)
    title(main = expression(symbol("\250")),
          col.main = "white", cex.main = 5)
    title(main = "Brooks-Gelman-Rubin plot")
    autocorr.plot(x@mcmc[[1]], ask = TRUE, auto.layout = FALSE)
    title(main = expression(symbol("\250")),
          col.main = "white", cex.main = 5)
    title(main = "Autocorrelation plot (chain 1)")
  }
)
