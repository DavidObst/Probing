#' Title
#'
#' @param v
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Proba.v <- function(v,x)
{
  R <- 1 - cdf.v(v,x)

  return(R)
}
