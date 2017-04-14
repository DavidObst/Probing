#' Variable selection through probing
#'
#' This function allows the user to perform a variable selection for a gaussian linear model \eqn{y = X*B + w}.
#' In fact this algorithm can be applied for any linear in its parameter model (e.g. GAM models).
#'
#' @param y The response vector.
#' @param X A data frame corresponding to the design matrix.
#' @param r A real number between 0 and 1 giving the risk of selecting a feature
#' less relevant than a random probe.
#'
#' @return A list containing the following elements:
#' \item{G}{A numeric vector corresponding to the CDF of the rank of the probe.}
#' \item{selected}{A character vector giving the chosen variables at the risk level r.}
#'
#' @details
#' As it is, it cannot be applied for selecting variables for models such as ANN, but one can first learn a task on a
#' linear in its parameters model, apply this algorithm and then re-use the variables it yielded on a more complex model.
#'
#' Since the CDF calculation involves calculation of a factorial, using high amounts of samples (typically N > 100)
#' is a bad idea and will lead R to crash.
#'
#' Important remark : if r < 1, usually only the beginning of the CDF of the rank of the probe will be obtained (since
#' the algorithm stops as soon as G < r).
#' In order to obtain the full one, the user should take r=1.
#'
#' @export
#'
#' @examples
#' ### Linear model ###
#' theta <- c(1,-2.5,0.4,0.7,-0.2,rep(0,5))
#' w <- rnorm(15,0,0.02)
#' X <- cbind(replicate(5,rnorm(15,1,0.05)),replicate(5,runif(15,-1,1)))
#' y <- X%*%theta + w
#' X <- as.data.frame(X)
#' probing(y,X,r=0.1) ### Usully at a risk level of 0.1, at least 4 of the relevant variables are chosen.
#' probing(y,X,r=1) ### yields the CDF of the random probe.
#'
#' ### Polynomial model ###
#' library(mgcv)
#' w <- rnorm(200,0,0.1)
#' X <- cbind(replicate(5,rnorm(200,1,0.05)),replicate(5,runif(200,-1,1))) ##N = 200 to allow the gam model to be learnt.
#' X <- as.data.frame(X)
#' y <- 2*(X[,1]^2) -0.8*exp(X[,2]-0.2) + 0.5*abs(X[,4]-1) + w
#' formula.gam <- paste("y~",paste("s(V",1:10,")",sep="",collapse="+"),sep="")
#' gam <- gam(as.formula(formula.gam),data=X)
#' X.gam <- as.data.frame(gam$model)[,-1]
#' probing(y[1:70],X.gam[1:70,],r=0.1) ##gam$model is the design matrix after doing all the transformation for GAM estimate
#' probing(y[1:70],X.gam[1:70,],r=1)

probing <- function(y,X,r)
{
  #browser()
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

    if(n == 1) {
      G[n] <- Proba.v(N-n,cos.2[most.relevant])
    }

    else{
      G[n] <- G[n-1] + Proba.v(N-n,cos.2[most.relevant])*(1-G[n-1])
    }

    ## See if current feature is relevant ##

    if((G[n] <= r ) & (n < Q-1)) { ## G_n < r : the probe is less relevant that the n-th feature
      selected <- c(selected,colnames(X)[most.relevant])
      y <- y - proj(y,X[,most.relevant])
      X <- X[,-most.relevant] - apply(X[,-most.relevant],2,proj,a=X[,most.relevant])
    }

    else if( (G[n] <= r ) & (n == Q-1))
    {
      selected <- c(selected,colnames(X)[most.relevant])
      y <- y - proj(y,X[,most.relevant])
      X <- as.matrix(X[,-most.relevant] - proj(u= X[,-most.relevant],a=X[,most.relevant]))
    }

    else{
      end <- TRUE
    }

  }

    to.return <- list("G"=G,"selected"=selected)

    return(to.return)


}
