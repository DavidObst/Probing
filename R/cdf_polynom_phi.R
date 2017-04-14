#' Calculation of the polynomials appearing in the CDF
#'
#' @param v An integer >= 2.
#' @param x A real number between 0 and 1.
#'
#' @return
#' @export
#'
#' @examples
Phi.v <- Vectorize(function(v,x)
{
  stopifnot((v >= 2)) ## v parameter and greater or equal to 2

  ## Particular cases ##
  if( v==2)
  {
    Phi <- 0
    return(Phi)
  }

  else if (v == 3)
  {
    Phi <- 1
    return(Phi)
  }

  else if (v == 4)
  {
    Phi <- 1
    return(Phi)
  }

  ########################

  ## Even v ##
  else if(((v %% 2) == 0) & (v >= 6) ) ## Even v
  {
    k <- seq(1,(v/2-2))
    d.fact <-
    s <- (2^k)*(factorial(k)/semi.fact(2*k+1))*(1-x)^k
    Phi <- 1 + sum(s)

    return(Phi)
  }

  ## Odd v ##
  else
  {
    k <- seq(1,(v-3)/2)
    s <- (1/2^k)*(semi.fact(2*k-1)/factorial(k))*(1-x)^k
    Phi <- 1 + sum(s)

    return(Phi)
  }

},vectorize.args = "x")
