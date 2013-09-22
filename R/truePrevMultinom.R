truePrevMultinom <-
function(x, n, prior, conf.level,
         nchains, burnin, update, verbose){

  ## create model
  t <- log(length(x), 2)     # number of tests
  ntheta <- 2 ^ (t + 1) - 1  # number of thetas

  model <- character()

  ## write model initiation
  model[1] <- "model {"
  model[2] <- paste("x[1:", 2 ^ t,
                    "] ~ dmulti(AP[1:", 2 ^ t, "], n)",
                    sep = "")

  ## write AP[] definitions in terms of theta[]
  s <- multiModel_select(t)  # define theta construct for SE/SP
  p <- multiModel_probs(s)   # define AP[.] in terms of theta[.]
  model <- c(model, "", p, "")

  ## write theta[] prior
  for (i in seq(ntheta))
    model <- c(model,
      writeSeSp(paste("theta[", i, "]", sep = ""), prior[[i]]))

  ## write bayesP definition
  bayesP <-
    c(paste("x2[1:", (2^t), "] ~ dmulti(AP[1:", (2^t), "], n)", sep=""),
      paste("for (i in 1:", (2^t), "){", sep=""),
      "d1[i] <- x[i] * log(max(x[i],1) / (AP[i]*n))",
      "d2[i] <- x2[i] * log(max(x2[i],1) / (AP[i]*n))",
      "}",
      "G0 <- 2 * sum(d1[])",
      "Gt <- 2 * sum(d2[])",
      "bayesP <- step(G0 - Gt)")
  model <- c(model, "", bayesP)

  ## write SE[]/SP[] definition
  model <- c(model, "", multiModel_SeSp(t))

  ## close model
  model <- c(model, "}")

  ## define model class
  class(model) <- "prevModel"

  ## create data
  data <- list(x = x, n = n)

  ## generate inits
  inits <- NULL

  ## get results!
  if (verbose) cat("JAGS progress:\n\n")

  nodes <- paste(c("SE", "SP"), rep(seq(t), each = 2), sep = "")
  nodes <- c("TP", nodes, "bayesP")

  JAGSout <- R2JAGS(model = model, data = data, inits = inits,
                    nchains = nchains, burnin = burnin, update = update,
                    nodes = nodes, verbose = verbose)

  ## define mcmc samples
  mcmc.list <- JAGSout$mcmc.list
  class(mcmc.list) <- c("list", "mcmc.list")
  names <- colnames(mcmc.list[[1]])
  mcmc.list_list <- list()
  order <- c(length(names) - 1, c(t(cbind(1:t, 1:t+t))), length(names))
  for (i in seq_along(names))
    mcmc.list_list[[i]] <- mcmc.list[, order[i]]
  names(mcmc.list_list) <- names[order]

  ## define diagnostics
  DIC <- JAGSout$dic
  BGR <- c(gelman.diag(mcmc.list_list$TP, autoburnin = FALSE)$psrf)
  bayesP <- mean(unlist(mcmc.list_list$bayesP))

  ## define output
  out <-
    new("prev",
        par = list(x = x, n = n, prior = prior,
                   conf.level = conf.level, nchains = nchains,
                   burnin = burnin, update = update, inits = inits),
        model = model,
        mcmc = mcmc.list_list,
        diagnostics = list(DIC = DIC,
                           BGR = data.frame(mean = BGR[1],
                                            upperCL = BGR[2]),
                           bayesP = bayesP))

  ## return output
  return(out)
}