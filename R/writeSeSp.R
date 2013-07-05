writeSeSp <-
function(f, x){
  out <- character()

  if (x$d[1] == "pert"){
    pertK <- ifelse(is.null(x$p[4]), 4, x$p[4])
    pertP <- ifelse(is.null(x$p[5]), 0.95, x$p[5])
    pertM <- x$d[2]

    betaPERT <- betaPERT(a = x$p[1], m = x$p[2], b = x$p[3],
      k = pertK, p = pertP, method = pertM, plot = FALSE)

    if (pertM  == "branscum"){
      distSpec <- paste("~ dbeta(", betaPERT$alpha,
        ", ", betaPERT$beta, ")", sep = "")
    } else {
      distSpec1 <- paste("<- ", f, "p * (", betaPERT$b,
        "-", betaPERT$a, ") + ", betaPERT$a, sep = "")
      distSpec2 <- paste("~ dbeta(", betaPERT$alpha,
        ", ", betaPERT$beta, ")", sep = "")
      out <- c(out, paste(f, distSpec1))
      out <- c(out, paste(f, "p ", distSpec2, sep = ""))
    }
  }

  if (x$d[1] == "fixed"){
    distSpec <- paste("<- ", x$p, sep="")
    out <- c(out, paste(f, distSpec))
  }

  if (x$d[1] == "uniform"){
    distSpec <- paste("~ dunif(", x$p[1], ", ", x$p[2], ")", sep = "")
    out <- c(out, paste(f, distSpec))
  }

  if (x$d[1] == "beta"){
    distSpec <- paste("~ dbeta(", x$p[1], ", ", x$p[2], ")", sep = "")
    out <- c(out, paste(f, distSpec))
  }

  return(out)
}