#' Title
#'
#' @param y
#' @param X
#' @param r
#'
#' @return
#' @export
#'
#' @examples
#' theta <- c(1,-2.5,0.4,0.7,-0.2,rep(0,5))
#' w <- rnorm(15,0,0.02)
#' X <- replicate(10,rnorm(15,1,0.05))
#' y <- X%*%theta + w
#' X <- as.data.frame(X)
#' probing(y,X,r=0.1)

probing <- function(y,X,r)
{
  browser()
  ##### Initialization  #####
  Q <- ncol(X)
  N <- nrow(X)


  remaining <- colnames(X)
  selected <- c()
  end <- FALSE

  n <- 0


  G <- rep(0,Q)


  ## Probing ##

  while((n < Q) & (end==FALSE))
  {
    n <- n+1
    remaining.indices <- which(!(remaining %in% selected))

    ## Calculation of relevance of all remaining covariates ##

    cos.2 <- rep(0,length(remaining.indices))

    for(k in 1:length(remaining.indices))
    {
      cos.2[k] <- (dp(y,X[,k]))^2/(dp(y,y)*dp(X[,k],X[,k]))
    }

    most.relevant <- which.max(cos.2)



    ## Update of G ##

    if(n == 1) {G[n] <- Proba.v(N-n,cos.2[most.relevant])}

    else{
      G[n] <- G[n-1] + Proba.v(N-n,cos.2[most.relevant])*(1-G[n-1])
    }

    ## See if current feature is relevant ##

    if(G[n] < r) {
      selected <- c(selected,colnames(X)[most.relevant])
      y <- y - proj(y,X[,most.relevant])
      X <- X[,-most.relevant] - apply(X[,-most.relevant],2,proj,a=X[,most.relevant])
    }

    else{
      end <- TRUE
    }

  }

    to.return <- list("G"=G,"selected"=selected,"cos.2"=cos.2)

    return(to.return)


}
