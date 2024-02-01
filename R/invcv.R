#' Variance from CV%
#'
#' @param cvfun intcv()-based function
#' @param cv CV% generated from cvfun
#' @param verbose extra output
#' @param ... Other parameters to pass to cvfun
#'
#' @return Best-fit variance
#' @export
#'
#' @importFrom stats optim
#'
#' @examples
invcv <- function(cvfun,cv,verbose=FALSE,...) {
  # given cv function and cv, find omega
  ssq <- function(x) {
    expcv <- cvfun(...,v=x)
    out <- (expcv - cv)^2
    out[!is.finite(out)] <- 1e6
    out
  }
  # Starting value should assume lognormal
  start <- log((cv/100)^2 + 1)
  if (ssq(start)<.Machine$double.eps) { # check if "exact"
    if (verbose) message("Solved numerically as lognormal")
    return(start)
  }
  opt <- optim(start,ssq,lower=0,method="L-BFGS-B") # L-BFGS-needed for bounds
  if (verbose) message(opt$message)
  opt$par
}
