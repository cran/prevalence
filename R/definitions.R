definition_theta <-
function(n){
  ## check if n is defined
  if (missing(n))
    stop("The number of tests 'n' is not defined")

  ## check if n is defined correctly
  if (is.null(n) || is.na(n) || is.infinite(n))
    stop("The number of tests 'n' is not defined")
  if (is.character(n))
    stop("The number of tests 'n' must be a numeric value")
  checkInput(n, "n", class = "integer", min = 1)

  ## print title
  test <- ifelse(n == 1, "test:", "tests:")
  cat("Definition of the prior, 'theta', for", n, test, "\n")

  ## print theta[1-3]
  cat("theta[1] : P(D+) = TP\n")
  cat("theta[2] : P(T1+|D+) = SE1\n")
  cat("theta[3] : P(T1-|D-) = SP1\n")

  ## print remaining thetas, if needed
  if (n > 1){
    t <- 4
    for (i in 2:n){
      N <- 2 ^ i  # number of thetas for test i
      for (k in seq(N)){
        D <- ifelse(k <= (N/2), "+", "-")  # true disease status
        T <- ifelse(k <= (N/2), "+", "-")  # current test status
        out <- paste("P(T", i, T, "|D", D, sep = "")

        for (j in seq(i-1)){
          select <- rep(c("+", "-"), each = (2^(i-j-1)), times = (2^(j-1)))
          select <- c(select, rev(select))
          out <- paste(out, ",T", j, select[k], sep = "")
        }

        cat(paste("theta[", t, "] : ", out, ")\n", sep = ""))
        t <- t + 1
      }
    }
  }
}

definition_x <-
function(n){
  ## check if n is defined
  if (missing(n))
    stop("The number of tests 'n' is not defined")

  ## check if n is defined correctly
  if (is.null(n) || is.na(n) || is.infinite(n))
    stop("The number of tests 'n' is not defined")
  if (is.character(n))
    stop("The number of tests 'n' must be a numeric value")
  checkInput(n, "n", class = "integer", min = 1)

  ## print title
  test <- ifelse(n == 1, "test:", "tests:")
  cat("Definition of the apparent test results, 'x', for", n, test)

  ## define test status for all APs
  status <- matrix(nrow = 2 ^ n, ncol = n)
  for (i in seq(n)){
    status[, i] <- rep(c("-", "+"), each = 2 ^ (n - i), times = 2 ^ (i - 1))
  }

  ## paste output
  T <- character(2 ^ n)
  for (i in seq(2 ^ n))
    T[i] <-
      paste("T", seq(n), status[i, ], ",",
            sep = "", collapse = "")

  out <-
    paste("\nx[", seq(2^n), "] : ",
          sapply(T, substr, start = 1, stop = nchar(T) - 1),
          sep = "")

  cat(out, "\n")
}