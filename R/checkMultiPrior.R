## Check prior for multinomial model

checkMultiPrior <-
function(prior){
  ## evaluate whether prior is defined correctly
  first_element <- as.character(prior)[1]
  if (!any(c("{", "list") == first_element))
    stop("'prior' is not defined correctly")

  ## if prior is defined as a list
  ## note: list element names currently not taken in account!
  if (first_element == "list"){
    n <- length(prior) - 1
    theta_list <- vector("list", n)
    for (i in seq(n))
      theta_list[[i]] <- checkSeSp(eval(parse(text = prior)[[i + 1]]))
  }

  ## if prior is defined as a function
  if (first_element == "{"){
    n <- length(prior) - 1
    thetas <- vector("list", n)
    for (i in seq(n))
      thetas[[i]] <- explode(as.character(prior[[i + 1]]))

    ## get indices from theta_list
    index <- sapply(thetas, function(x) as.numeric(x[[1]]))

    ## check if all indices exist
    if (length(index) != max(index))
      stop("The indices of 'theta[.]' are not correctly specified.\n",
           "See ?theta for more info.")
    if (!all(unique(index) == index))
      stop("The indices of 'theta[.]' are not correctly specified.\n",
           "See ?theta for more info.")
    if (length(index) == 1 | log(length(index) + 1, 2)%%1 != 0)
      stop("The number of specified theta values is incorrect.\n",
           "See ?theta for more info.")

    ## re-arrange list elements if needed
    order <- order(index)
    theta_list <- vector("list", n)
    for (i in seq(n))
      theta_list[[i]] <- thetas[[order[i]]][[2]]
    
  }

  ## return prior in list format
  return(theta_list)
}