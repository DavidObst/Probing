#' Title
#'
#' @param u
#' @param a
#'
#' @return
#' @export
#'
#' @examples
proj <- function(u,a)
{

  p.u <- as.numeric( t(u)%*%a/((t(a)%*%a)) )*a
  return(p.u)
}
