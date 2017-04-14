#' Title
#'
#' @param v
#' @param x
#'
#' @return
#' @export
#'
#' @examples
cdf.v <- Vectorize(function(v,x)
{
  stopifnot((v >= 2) | (x >= 0)) ##Stop if v not greater or equal to 2 or x not positive

  if(v %% 2 == 0) ##v even
  {
    cdf <- (2/pi)*(asin(sqrt(x))+sqrt(x*(1-x))*Phi.v(v,x) )
  }

  else{ ##v odd
    cdf <- sqrt(x)*Phi.v(v,x)
  }

  return(cdf)
},vectorize.args = "x")
