#' Box-Cox transform typically used in NONMEM
#'
#' Parameters are typically treated as lognormally-distributed by NONMEM users.
#' Box-Cox transforms are typically applied to the exponentiated individual ETA parameters;
#' this means the parameter is neither Box-Cox distributed nor lognormally-distributed, but both.
#' To get the "Box-Cox Transform" as it would be relevant for CV% calculation, these properties
#' have to be considered.
#'
#' @param x random vector. Must be positive.
#' @param lambda shape parameter
#' @param theta centrality parameter
#' @param inv inverse transform
#'
#' @return Box-Cox transformed or untransformed vector
#' @export
#'
#' @examples
nonmemboxcox <- function(x,lambda,theta=1,inv=FALSE) {
  if (!inv) {
    normalized <- x/theta
    {
      if (lambda==0) bc1 <- log(normalized)
      else bc1 <- ((normalized^lambda) - 1)/lambda
    }
    return( log(theta) + bc1 )
  } else {
    normalized <- x - log(theta)
    {
      if (lambda==0) bc1 <- exp(normalized)
      else bc1 <- (normalized*lambda + 1)^(1/lambda)
    }
    return( theta*bc1 )
  }
}
