#' Title
#'
#' @param n
#'
#' @return
#' @export
#'
#' @examples
semi.fact <- Vectorize(function(n)
{
  stopifnot(n >= 0) ## Argument must be positive integer

  if(n %% 2 == 0) ###Even n
  {
    k <- as.integer(n/2)
    R <- (2^k)*factorial(k)
  }

  ## Odd n
  else{
    k <- as.integer(0.5*(n+1))
    R <- factorial(n)/semi.fact(n-1) ##Recursive call
  }

  return(R)
})
