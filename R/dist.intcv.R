#' Built-in integration-based %CV functions
#'
#' @param dist Selection of built-in distributions.
#' @param ... passed to moment()
#' @param exact If there is an exact moment generating function, use that. Default TRUE only for log
#' @param lambda shape parameter for nonmemboxcox()
#' @param fun return function (for use in invcv())
#'
#' @return Percent CV
#' @export
#'
#' @importFrom stats plogis qlogis
#'
#' @examples
dist.intcv <- function(dist="log",...,exact=ifelse(dist=="log",TRUE,FALSE),lambda=NULL,fun=FALSE) {
  if (fun) return(function(...) dist.intcv(dist=dist, ...))
  out <- NULL
  if (dist=="log") {
    list2env(list(...), environment())
    if (exact) out <- 100*sqrt(exp(v) - 1)
    else if ("u" %in% names(list(...))) out <- intcv(...,pdist = exp,qdist= log)
    else out <- intcv(...,u=1,pdist = exp,qdist= log)
  } else if (dist == "logexp") {
    out <- intcv(...,pdist = function(x) exp(x) - 1,qdist= function(x) log(x+1))
  } else if (dist == "logit") {
    out <- intcv(...,pdist = plogis,qdist= qlogis)
  } else if (dist == "arcsin") {
    out <- intcv(...,pdist = function(x) sin(x)^2,qdist= function(x) asin(sqrt(x)))
  } else if (dist == "nmboxcox") {
    list2env(list(...), environment())
    qbxcxt <- function(x) nonmemboxcox(x,lambda=lambda, theta = u)
    pbxcxt <- function(x) nonmemboxcox(x,lambda=lambda, theta = u,inv=TRUE)
    out <- intcv(...,pdist = pbxcxt,qdist= qbxcxt)
  }
  if (is.null(out)) stop(sprintf("Distribution %s not built into package.", dist))
  out
}
