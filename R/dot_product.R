#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
dp <- function(x,y)
{

  stopifnot((length(x) == length(y)))

  R <- as.numeric(t(x)%*%y)
  return(R)
}
