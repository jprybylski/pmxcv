#' Integratable moment function
#'
#' @param x numeric vector
#' @param u mean
#' @param v variance
#' @param n moment number
#' @param pdist un-transform function for transformed random variable (eg, exp())
#' @param qdist transform function (eg, log())
#'
#' @return Point result of the moment function
#' @export
#'
#' @examples
moment_f <- function(x,u,v,n,pdist,qdist) {
  mu <- qdist(u) # u provided in natural units
  sigma <- sqrt(v)

  dens <-  stats::dnorm(x, mean = mu, sd = sigma)
  p <- pdist(x)
  o <- (p^n)*dens
  o[!is.finite(o)] <- 0
  o
}
